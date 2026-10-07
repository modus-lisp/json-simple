;;;; parse.lisp — JSON text to Lisp values.
;;;;
;;;; Default mapping is com.inuoe.jzon's, so PARSE is a drop-in for JZON:PARSE:
;;;;
;;;;   object -> hash-table, :test EQUAL      array -> simple-vector
;;;;   string -> simple-string                number -> integer or double-float
;;;;   true -> T    false -> NIL    null -> the symbol CL:NULL
;;;;
;;;; and the representation is selectable, because not every consumer wants
;;;; jzon's: a Nostr relay keeps object key ORDER and needs false and null to
;;;; be distinguishable from an empty list --
;;;;
;;;;   (parse frame :object-type :alist :true :true :false :false :null :null)
;;;;
;;;; gives objects as ((key . value) ...) in source order.
;;;;
;;;; Strict RFC 8259: no comments, no trailing commas, no leading zeros, no raw
;;;; control characters in strings, nothing after the top-level value.

(in-package #:json-simple)

(defun %parse-fail (pos fmt &rest args)
  (error 'json-parse-error :position pos
                           :format-control (concatenate 'string fmt " at position ~D")
                           :format-arguments (append args (list pos))))

(defun %utf8-decode (octets start end)
  "Decode OCTETS[START,END) as strict UTF-8 into a fresh simple-string."
  (let ((out (make-string (- end start)))
        (i start) (o 0))
    (flet ((bad () (%parse-fail i "invalid UTF-8"))
           (cont (k)
             (let ((b (if (< k end) (aref octets k) 0)))
               (if (= (logand b #xC0) #x80) (logand b #x3F) nil))))
      (loop while (< i end) do
        (let ((b (aref octets i)) (code 0) (len 0))
          (cond ((< b #x80) (setq code b len 1))
                ((= (logand b #xE0) #xC0) (setq code (logand b #x1F) len 2))
                ((= (logand b #xF0) #xE0) (setq code (logand b #x0F) len 3))
                ((= (logand b #xF8) #xF0) (setq code (logand b #x07) len 4))
                (t (bad)))
          (loop for k from 1 below len do
            (let ((c (cont (+ i k))))
              (unless c (bad))
              (setq code (logior (ash code 6) c))))
          ;; Overlong forms, surrogates and out-of-range code points.
          (when (or (and (= len 2) (< code #x80))
                    (and (= len 3) (< code #x800))
                    (and (= len 4) (< code #x10000))
                    (> code #x10FFFF)
                    (<= #xD800 code #xDFFF))
            (bad))
          (setf (char out o) (code-char code))
          (incf o)
          (incf i len))))
    (subseq out 0 o)))

(defun %input-string (in)
  "IN (string, octet vector, stream or pathname) as a simple-string of text."
  (etypecase in
    (simple-string in)
    (string (coerce in 'simple-string))
    ((vector (unsigned-byte 8)) (%utf8-decode in 0 (length in)))
    (stream
     (if (subtypep (stream-element-type in) 'character)
         (with-output-to-string (s)
           (loop for c = (read-char in nil nil) while c do (write-char c s)))
         (let ((buf (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
           (loop for b = (read-byte in nil nil) while b do (vector-push-extend b buf))
           (%utf8-decode buf 0 (length buf)))))
    (pathname
     (with-open-file (s in :element-type '(unsigned-byte 8))
       (%input-string s)))))

(defun parse (in &key (max-depth 128) (object-type :hash-table)
                      (true t) (false nil) (null 'null))
  "Read one JSON value from IN -- a string, a (vector (unsigned-byte 8)) of
   UTF-8, a character or binary stream, or a pathname.  See the file header for
   the mapping; OBJECT-TYPE is :HASH-TABLE (default) or :ALIST, and TRUE /
   FALSE / NULL are the values those literals read as.  MAX-DEPTH bounds the
   nesting of arrays and objects (NIL = no bound).  Signals JSON-PARSE-ERROR."
  (let* ((s (%input-string in))
         (end (length s))
         (pos 0)
         (depth 0)
         (max-depth (or max-depth most-positive-fixnum)))
    (declare (simple-string s) (fixnum end pos depth))
    (labels ((ws-p (c)
               (or (char= c #\Space) (char= c #\Newline) (char= c #\Return) (char= c #\Tab)))
             (skip-ws ()
               (loop while (and (< pos end) (ws-p (schar s pos))) do (incf pos)))
             (expect (ch what)
               (skip-ws)
               (if (and (< pos end) (char= (schar s pos) ch))
                   (incf pos)
                   (%parse-fail pos "expected ~A" what)))
             (deeper ()
               (incf depth)
               (when (> depth max-depth)
                 (%parse-fail pos "nesting deeper than ~D" max-depth)))
             (value ()
               (skip-ws)
               (when (>= pos end) (%parse-fail pos "unexpected end of input"))
               (let ((c (schar s pos)))
                 (cond ((char= c #\") (incf pos) (str))
                       ((char= c #\{) (incf pos) (object))
                       ((char= c #\[) (incf pos) (arr))
                       ((or (char= c #\-) (char<= #\0 c #\9)) (num))
                       ((char= c #\t) (lit "true" true))
                       ((char= c #\f) (lit "false" false))
                       ((char= c #\n) (lit "null" null))
                       (t (%parse-fail pos "unexpected character ~S" c)))))
             (lit (word v)
               (let ((n (length word)))
                 (if (and (<= (+ pos n) end) (string= word s :start2 pos :end2 (+ pos n)))
                     (progn (incf pos n) v)
                     (%parse-fail pos "invalid literal"))))
             (object ()
               (deeper)
               (let ((ht (if (eq object-type :alist) nil (make-hash-table :test 'equal)))
                     (acc nil))
                 (skip-ws)
                 (if (and (< pos end) (char= (schar s pos) #\}))
                     (incf pos)
                     (loop
                       (skip-ws)
                       (unless (and (< pos end) (char= (schar s pos) #\"))
                         (%parse-fail pos "expected object key"))
                       (incf pos)
                       (let ((k (str)))
                         (expect #\: "':'")
                         (let ((v (value)))
                           (if ht (setf (gethash k ht) v) (push (cons k v) acc))))
                       (skip-ws)
                       (when (>= pos end) (%parse-fail pos "unterminated object"))
                       (let ((c (schar s pos)))
                         (incf pos)
                         (cond ((char= c #\,))
                               ((char= c #\}) (return))
                               (t (%parse-fail (1- pos) "expected ',' or '}'"))))))
                 (decf depth)
                 (or ht (nreverse acc))))
             (arr ()
               (deeper)
               (let ((acc nil) (n 0))
                 (skip-ws)
                 (if (and (< pos end) (char= (schar s pos) #\]))
                     (incf pos)
                     (loop
                       (push (value) acc)
                       (incf n)
                       (skip-ws)
                       (when (>= pos end) (%parse-fail pos "unterminated array"))
                       (let ((c (schar s pos)))
                         (incf pos)
                         (cond ((char= c #\,))
                               ((char= c #\]) (return))
                               (t (%parse-fail (1- pos) "expected ',' or ']'"))))))
                 (decf depth)
                 (let ((v (make-array n)))
                   (loop for i from (1- n) downto 0 do (setf (svref v i) (pop acc)))
                   v)))
             (hex4 ()
               (when (> (+ pos 4) end) (%parse-fail pos "truncated \\u escape"))
               (let ((code 0))
                 (loop repeat 4 do
                   (let ((d (digit-char-p (schar s pos) 16)))
                     (unless d (%parse-fail pos "bad \\u escape"))
                     (setq code (+ (* code 16) d))
                     (incf pos)))
                 code))
             (str ()
               ;; POS is just past the opening quote.  Fast path: no escapes.
               (let ((start pos))
                 (loop
                   (when (>= pos end) (%parse-fail start "unterminated string"))
                   (let ((c (schar s pos)))
                     (cond ((char= c #\") (incf pos) (return-from str (subseq s start (1- pos))))
                           ((char= c #\\) (return))
                           ((< (char-code c) #x20) (%parse-fail pos "control character in string"))
                           (t (incf pos)))))
                 ;; Slow path: an escape at POS.
                 (with-output-to-string (out)
                   (write-string s out :start start :end pos)
                   (loop
                     (when (>= pos end) (%parse-fail start "unterminated string"))
                     (let ((c (schar s pos)))
                       (incf pos)
                       (cond
                         ((char= c #\") (return))
                         ((< (char-code c) #x20) (%parse-fail (1- pos) "control character in string"))
                         ((char/= c #\\) (write-char c out))
                         (t
                          (when (>= pos end) (%parse-fail pos "truncated escape"))
                          (let ((e (schar s pos)))
                            (incf pos)
                            (case e
                              (#\" (write-char #\" out))
                              (#\\ (write-char #\\ out))
                              (#\/ (write-char #\/ out))
                              (#\b (write-char (code-char 8) out))
                              (#\f (write-char (code-char 12) out))
                              (#\n (write-char (code-char 10) out))
                              (#\r (write-char (code-char 13) out))
                              (#\t (write-char (code-char 9) out))
                              (#\u
                               (let ((at (- pos 2)) (code (hex4)))
                                 ;; A high surrogate must be followed by \u and a low
                                 ;; surrogate (one character, U+10000..U+10FFFF) or it
                                 ;; is an error; a lone LOW surrogate is kept as is.
                                 ;; Both exactly as jzon.
                                 ;; Combined with +, not jzon's LOGIOR, which drops
                                 ;; planes 2-16 (\uDBFF\uDFFF read as U+FFFFF).
                                 (cond
                                   ((<= #xD800 code #xDBFF)
                                    (unless (and (< (+ pos 1) end)
                                                 (char= (schar s pos) #\\)
                                                 (char= (schar s (1+ pos)) #\u))
                                      (%parse-fail at "lone surrogate"))
                                    (incf pos 2)
                                    (let ((lo (hex4)))
                                      (unless (<= #xDC00 lo #xDFFF)
                                        (%parse-fail at "lone surrogate"))
                                      (setq code (+ #x10000 (ash (- code #xD800) 10) (- lo #xDC00))))))
                                 (write-char (code-char code) out)))
                              (t (%parse-fail (1- pos) "bad escape \\~A" e)))))))))))
             (digits ()
               ;; [0-9]+ at POS: (values integer count).
               (let ((v 0) (n 0))
                 (loop while (and (< pos end) (char<= #\0 (schar s pos) #\9)) do
                   (setq v (+ (* v 10) (- (char-code (schar s pos)) 48)))
                   (incf n) (incf pos))
                 (values v n)))
             (num ()
               (let ((start pos) (neg nil))
                 (when (char= (schar s pos) #\-) (setq neg t) (incf pos))
                 (when (or (>= pos end) (not (char<= #\0 (schar s pos) #\9)))
                   (%parse-fail start "bad number"))
                 (when (and (char= (schar s pos) #\0) (< (1+ pos) end)
                            (char<= #\0 (schar s (1+ pos)) #\9))
                   (%parse-fail start "leading zero in number"))
                 (multiple-value-bind (int nint) (digits)
                   (let ((frac 0) (nfrac 0) (exp 0) (floatp nil))
                     (when (and (< pos end) (char= (schar s pos) #\.))
                       (incf pos) (setq floatp t)
                       (multiple-value-setq (frac nfrac) (digits))
                       (when (zerop nfrac) (%parse-fail start "bad number")))
                     (when (and (< pos end) (char-equal (schar s pos) #\e))
                       (incf pos) (setq floatp t)
                       (let ((eneg nil))
                         (when (< pos end)
                           (case (schar s pos)
                             (#\- (setq eneg t) (incf pos))
                             (#\+ (incf pos))))
                         (multiple-value-bind (e ne) (digits)
                           (when (zerop ne) (%parse-fail start "bad number"))
                           (setq exp (if eneg (- e) e)))))
                     (when (and (< pos end)
                                (let ((c (schar s pos)))
                                  (not (or (ws-p c) (char= c #\,) (char= c #\]) (char= c #\})))))
                       (%parse-fail pos "bad number"))
                     (if (not floatp)
                         ;; "-0" is negative zero to jzon, which no integer is.
                         (cond ((not neg) int) ((zerop int) -0d0) (t (- int)))
                         (let ((d (%decimal-double neg (+ (* int (expt 10 nfrac)) frac)
                                                   (+ nint nfrac) (- exp nfrac))))
                           (if (eq d :overflow)
                               (%parse-fail start "number out of double-float range")
                               d))))))))
      (let ((v (value)))
        (skip-ws)
        (when (< pos end) (%parse-fail pos "content after the JSON value"))
        v))))

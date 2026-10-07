;;;; write.lisp — Lisp values to JSON text.
;;;;
;;;; STRINGIFY writes what JZON:STRINGIFY writes, byte for byte, for the values
;;;; jzon's type mapping covers:
;;;;
;;;;   T -> true   NIL -> false   CL:NULL -> null
;;;;   integer, float, ratio -> number (floats shortest round-trip; ratios as
;;;;                            the nearest double)
;;;;   string, other symbol, character, pathname -> string
;;;;   hash-table -> object (keys: strings as is, symbols downcased unless
;;;;                 mixed-case, characters, integers, floats, else ~A)
;;;;   vector, list, multi-dimensional array -> array (arrays of arrays)
;;;;
;;;; :PRETTY gives jzon's layout (2-space indent, "key": value, empty {} / []).
;;;; CLOS instances and structures are not serialized (jzon walks their slots);
;;;; build a hash table.

(in-package #:json-simple)

(defun write-json-string (string stream &key canonical)
  "Write STRING to STREAM as a JSON string literal.  Escapes \\\" \\\\ \\b \\f
   \\n \\r \\t and, unless CANONICAL, every other C0 control as \\u00XX (jzon's
   output, and valid JSON).  CANONICAL is NIP-01's event-id serialization: only
   those seven are escaped and every other character is written verbatim, so the
   id computed is the id the author signed."
  (write-char #\" stream)
  (let ((run 0) (n (length string)))
    (flet ((flush (i) (when (< run i) (write-string string stream :start run :end i))))
      (dotimes (i n)
        (let* ((c (char string i))
               (code (char-code c))
               (esc (cond ((char= c #\") "\\\"")
                          ((char= c #\\) "\\\\")
                          ((>= code #x20) nil)
                          ((= code 8) "\\b")
                          ((= code 12) "\\f")
                          ((= code 10) "\\n")
                          ((= code 13) "\\r")
                          ((= code 9) "\\t")
                          (canonical nil)
                          (t (concatenate 'string "\\u00"
                                          (string (char "0123456789ABCDEF" (ash code -4)))
                                          (string (char "0123456789ABCDEF" (logand code 15))))))))
          (when esc
            (flush i)
            (write-string esc stream)
            (setq run (1+ i)))))
      (flush n)))
  (write-char #\" stream)
  string)

(defun %integer-text (n)
  (if (typep n 'fixnum)
      (if (zerop n)
          "0"
          (let ((buf (make-string 21)) (i 21) (m (abs n)))
            (loop while (> m 0) do
              (multiple-value-bind (q r) (floor m 10)
                (decf i)
                (setf (char buf i) (code-char (+ 48 r)))
                (setq m q)))
            (when (minusp n) (decf i) (setf (char buf i) #\-))
            (subseq buf i)))
      (let ((*print-base* 10) (*print-radix* nil)) (princ-to-string n))))

(defun %key-text (key)
  "jzon's COERCE-KEY: the string an object key is written as."
  (cond ((stringp key) key)
        ((symbolp key)
         (let ((name (symbol-name key)))
           (if (some #'lower-case-p name) name (string-downcase name))))
        ((characterp key) (string key))
        ((integerp key) (%integer-text key))
        ((floatp key) (%float-text key))
        (t (format nil "~A" key))))

(defun %newline-indent (stream indent)
  (write-char #\Newline stream)
  (loop repeat indent do (write-char #\Space stream)))

(defun %write-value (x stream pretty indent)
  (cond
    ((eq x t) (write-string "true" stream))
    ((null x) (write-string "false" stream))
    ((eq x 'null) (write-string "null" stream))
    ((integerp x) (write-string (%integer-text x) stream))
    ((floatp x) (write-string (%float-text x) stream))
    ((rationalp x) (write-string (%float-text (float x 1d0)) stream))
    ((stringp x) (write-json-string x stream))
    ((hash-table-p x) (%write-object x stream pretty indent))
    ((symbolp x) (write-json-string (symbol-name x) stream))
    ((characterp x) (write-json-string (string x) stream))
    ((pathnamep x) (write-json-string (namestring x) stream))
    ((vectorp x)
     (%write-array (length x) (lambda (i) (aref x i)) stream pretty indent))
    ((consp x)
     (let ((cell x))
       (%write-array (length x) (lambda (i) (declare (ignore i)) (pop cell)) stream pretty indent)))
    ((arrayp x) (%write-md-array x stream pretty indent))
    (t (error 'json-write-error :format-control "cannot write ~S as JSON"
                                :format-arguments (list x)))))

(defun %write-array (n elt stream pretty indent)
  "Write an N-element array whose I-th element is (FUNCALL ELT I), in order."
  (write-char #\[ stream)
  (if (zerop n)
      nil
      (let ((inner (+ indent 2)))
        (dotimes (i n)
          (when (> i 0) (write-char #\, stream))
          (when pretty (%newline-indent stream inner))
          (%write-value (funcall elt i) stream pretty inner))
        (when pretty (%newline-indent stream indent))))
  (write-char #\] stream))

(defun %write-md-array (a stream pretty indent)
  "A multi-dimensional array as nested arrays: #2A((1 2) (3 4)) -> [[1,2],[3,4]]."
  (labels ((emit (dims prefix indent)
             (write-char #\[ stream)
             (let ((d (car dims)) (inner (+ indent 2)))
               (when (> d 0)
                 (dotimes (i d)
                   (when (> i 0) (write-char #\, stream))
                   (when pretty (%newline-indent stream inner))
                   (if (cdr dims)
                       (emit (cdr dims) (cons i prefix) inner)
                       (%write-value (apply #'aref a (reverse (cons i prefix))) stream pretty inner)))
                 (when pretty (%newline-indent stream indent))))
             (write-char #\] stream)))
    (if (null (array-dimensions a))
        (%write-value (aref a) stream pretty indent)
        (emit (array-dimensions a) nil indent))))

(defun %write-object (ht stream pretty indent)
  (write-char #\{ stream)
  (let ((first t) (inner (+ indent 2)))
    (maphash (lambda (k v)
               (if first (setq first nil) (write-char #\, stream))
               (when pretty (%newline-indent stream inner))
               (write-json-string (%key-text k) stream)
               (write-char #\: stream)
               (when pretty (write-char #\Space stream))
               (%write-value v stream pretty inner))
             ht)
    (when (and pretty (not first)) (%newline-indent stream indent)))
  (write-char #\} stream))

(defun stringify (element &key stream pretty)
  "Serialize ELEMENT to JSON (mapping in the file header).  STREAM is a FORMAT
   destination or a pathname: NIL returns a fresh string, T writes to
   *STANDARD-OUTPUT*, a string with a fill pointer is appended to, a pathname
   is (over)written as UTF-8.  PRETTY indents.  Returns the string for NIL,
   else NIL."
  (flet ((emit (out) (%write-value element out (and pretty t) 0)))
    (cond ((null stream) (with-output-to-string (out) (emit out)))
          ((eq stream t) (emit *standard-output*) nil)
          ((pathnamep stream)
           (with-open-file (out stream :direction :output :if-exists :supersede
                                       :if-does-not-exist :create :external-format :utf-8)
             (emit out))
           nil)
          ((stringp stream) (with-output-to-string (out stream) (emit out)) nil)
          (t (emit stream) nil))))

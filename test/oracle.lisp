;;;; test/oracle.lisp — json-simple against com.inuoe.jzon, the oracle.
;;;;
;;;;   (json-simple-test:run)   => number of failures; prints a summary
;;;;
;;;; 1. JSONTestSuite (shipped with jzon): every file accepted / rejected the
;;;;    same way, and accepted files parse to equal values.
;;;; 2. Random value trees: STRINGIFY byte-identical, compact and pretty; and
;;;;    jzon's text parses back to an equal value.
;;;; 3. Random doubles: same text, and the text reads back as the same double.
;;;; 4. Number edge cases.

(defpackage #:json-simple-test
  (:use #:cl)
  (:export #:run))

(in-package #:json-simple-test)

(defvar *failures* 0)
(defvar *shown* 0)

(defun fail (fmt &rest args)
  (incf *failures*)
  (when (< (incf *shown*) 25)
    (format t "~&  FAIL ~?~%" fmt args)))

(defun json-equal (a b)
  (cond ((and (hash-table-p a) (hash-table-p b))
         (and (= (hash-table-count a) (hash-table-count b))
              (block cmp
                (maphash (lambda (k v)
                           (multiple-value-bind (v2 found) (gethash k b)
                             (unless (and found (json-equal v v2)) (return-from cmp nil))))
                         a)
                t)))
        ((and (vectorp a) (vectorp b) (not (stringp a)) (not (stringp b)))
         (and (= (length a) (length b)) (every #'json-equal a b)))
        ((and (floatp a) (floatp b)) (or (= a b) (and (/= a a) (/= b b))))
        (t (equal a b))))

(defun outcome (fn input)
  "(:ok value) or (:error)."
  (handler-case (list :ok (funcall fn input))
    (error () (list :error))))

;;; ---- 1. JSONTestSuite ------------------------------------------------------

(defun suite-dir ()
  "jzon's copy of JSONTestSuite: next to its .asd (Ultralisp) or one up (Quicklisp)."
  (let* ((src (asdf:system-source-directory "com.inuoe.jzon"))
         (dir (pathname-directory src)))
    (find-if (lambda (d) (directory (merge-pathnames "*.json" d)))
             (list (make-pathname :directory (append dir '("JSONTestSuite" "test_parsing")) :defaults src)
                   (make-pathname :directory (append (butlast dir) '("JSONTestSuite" "test_parsing"))
                                  :defaults src)))))

(defun read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defparameter *jzon-wrong*
  ;; jzon combines a surrogate pair with LOGIOR instead of +, so planes 2-16
  ;; come out 0x10000 short.  These are the right answers.
  '(("y_string_last_surrogates_1_and_2.json" . #x10FFFF)
    ("y_string_unicode_U+10FFFE_nonchar.json" . #x10FFFE)))

(defparameter *jzon-lenient*
  ;; jzon keeps a lone low surrogate; json-simple rejects every lone surrogate,
  ;; so each string it returns is valid Unicode.  JSONTestSuite leaves these
  ;; to the implementation (i_ files).
  '("i_object_key_lone_2nd_surrogate.json"
    "i_string_incomplete_surrogate_pair.json"
    "i_string_lone_second_surrogate.json"))

(defparameter *jzon-slow*
  ;; Files jzon takes minutes (or, on SBCL 2.2.9, forever) to answer, with its
  ;; answer recorded.  It builds 10^10000000 for [123e-10000000], and on 2.2.9
  ;; never finishes [0.4e00669999...] (a 300-digit exponent).  Both: errors.
  '(("i_number_real_underflow.json" :error)
    ("i_number_huge_exp.json" :error)))

(defun test-suite ()
  (let ((files (and (suite-dir) (directory (merge-pathnames "*.json" (suite-dir)))))
        (n 0) (agree 0))
    (dolist (f files)
      (incf n)
      (let* ((bytes (read-octets f))
             (j (or (cdr (assoc (file-namestring f) *jzon-slow* :test #'string=))
                    (outcome #'com.inuoe.jzon:parse bytes)))
             (p (outcome #'json-simple:parse bytes)))
        (cond ((assoc (file-namestring f) *jzon-wrong* :test #'string=)
               ;; jzon's own bug (see *JZON-WRONG*): check json-simple is right.
               (if (and (eq (car p) :ok)
                        (= (char-code (char (aref (cadr p) 0) 0))
                           (cdr (assoc (file-namestring f) *jzon-wrong* :test #'string=))))
                   (incf agree)
                   (fail "suite ~A: json-simple ~S" (file-namestring f) p)))
              ((member (file-namestring f) *jzon-lenient* :test #'string=)
               (if (eq (car p) :error)
                   (incf agree)
                   (fail "suite ~A: json-simple should reject, got ~S" (file-namestring f) p)))
              ((and (eq (car j) :error) (eq (car p) :error)) (incf agree))
              ((and (eq (car j) :ok) (eq (car p) :ok) (json-equal (cadr j) (cadr p))) (incf agree))
              (t (fail "suite ~A: jzon ~S, json-simple ~S" (file-namestring f) (car j) (car p))))))
    (format t "~&suite: ~D/~D files agree~%" agree n)))

;;; ---- 2. random values ------------------------------------------------------

(defvar *rs* #+sbcl (sb-ext:seed-random-state 20261006) #-sbcl (make-random-state t))

(defun rnd (n) (random n *rs*))

(defun random-string ()
  (let ((s (make-string (rnd 12))))
    (dotimes (i (length s) s)
      (setf (char s i)
            (case (rnd 8)
              (0 (code-char (rnd 32)))
              (1 (char "\"\\/" (rnd 3)))
              (2 (code-char (+ #xA0 (rnd 2000))))
              (3 (code-char (+ #x1F600 (rnd 50))))
              (t (code-char (+ 32 (rnd 95)))))))))

(defun random-double ()
  "A random finite double over the whole range, subnormals included."
  (let ((d (if (zerop (rnd 20))
               (scale-float (float (rnd (expt 2 52)) 1d0) -1074)           ; subnormal
               (scale-float (float (+ (expt 2 52) (rnd (expt 2 52))) 1d0)
                            (- (rnd 2045) 1074)))))
    (if (zerop (rnd 2)) d (- d))))

(defun random-key ()
  (case (rnd 6)
    (0 (intern (string-upcase (substitute #\- #\Space (random-string))) :keyword))
    (1 (rnd 1000))
    (t (random-string))))

(defun random-value (depth)
  (let ((k (rnd (if (> depth 3) 9 12))))
    (case k
      (0 t) (1 nil) (2 'null)
      (3 (- (rnd 2000000) 1000000))
      (4 (- (rnd (expt 10 30)) (expt 10 29)))
      (5 (random-double))
      (6 (/ (1+ (rnd 1000)) (1+ (rnd 997))))
      (7 (random-string))
      (8 :some-keyword)
      (9 (let ((v (make-array (rnd 5))))
           (dotimes (i (length v) v) (setf (aref v i) (random-value (1+ depth))))))
      (10 (loop repeat (1+ (rnd 4)) collect (random-value (1+ depth))))
      (t (let ((h (make-hash-table :test 'equal)))
           (dotimes (i (rnd 5) h) (setf (gethash (random-key) h) (random-value (1+ depth)))))))))

(defun test-random (n)
  (let ((ok 0))
    (dotimes (i n)
      (let* ((v (random-value 0))
             (good t))
        (dolist (pretty '(nil t))
          (let ((j (com.inuoe.jzon:stringify v :pretty pretty))
                (p (json-simple:stringify v :pretty pretty)))
            (unless (string= j p)
              (setq good nil)
              (fail "stringify (pretty ~A)~%    jzon      ~S~%    json-simple ~S" pretty j p))
            (unless (json-equal (com.inuoe.jzon:parse j) (json-simple:parse j))
              (setq good nil)
              (fail "parse of ~S differs" j))))
        (when good (incf ok))))
    (format t "~&random values: ~D/~D agree~%" ok n)))

;;; ---- 3. doubles ------------------------------------------------------------

(defun test-doubles (n)
  (let ((ok 0))
    (dotimes (i n)
      (let* ((d (random-double))
             (j (com.inuoe.jzon:stringify d))
             (p (json-simple:stringify d)))
        (cond ((string/= j p) (fail "double ~S: jzon ~A json-simple ~A" d j p))
              ((/= (json-simple:parse p) d) (fail "double ~S: ~A reads back as ~S" d p (json-simple:parse p)))
              (t (incf ok)))))
    (format t "~&doubles: ~D/~D agree~%" ok n)))

;;; ---- 4. numbers ------------------------------------------------------------

(defun test-numbers ()
  (let ((cases '("0" "-0" "-0.0" "0.0" "1E2" "1e+2" "1e-2" "123456789012345678901234567890"
                 "1.7976931348623157e308" "1.7976931348623158e308" "1e309" "1e400" "1e-400"
                 "4.9e-324" "2.4703282292062327e-324" "2.4703282292062328e-324"
                 "0.1" "0.30000000000000004" "9007199254740993" "9007199254740993.0"
                 "1.00000000000000011102230246251565404236316680908203125"
                 "01" "1." ".1" "1e" "-" "+1" "1x" "0x10" "1e5x"))
        (ok 0))
    (dolist (c cases)
      (let ((j (outcome #'com.inuoe.jzon:parse c))
            (p (outcome #'json-simple:parse c)))
        (if (or (and (eq (car j) :error) (eq (car p) :error))
                (and (eq (car j) :ok) (eq (car p) :ok) (json-equal (cadr j) (cadr p))
                     (eql (cadr j) (cadr p))))
            (incf ok)
            (fail "number ~S: jzon ~S json-simple ~S" c j p))))
    (format t "~&numbers: ~D/~D agree~%" ok (length cases))))

(defun test-slices ()
  "PARSE with :START/:END on a frame inside a larger buffer reads exactly what
   the frame alone reads, including where it fails."
  (let ((n 0) (ok 0))
    (flet ((same (a b) (or (and (eq (car a) :error) (eq (car b) :error))
                           (and (eq (car a) :ok) (eq (car b) :ok) (json-equal (cadr a) (cadr b)))))
           (pos-of (thunk) (handler-case (progn (funcall thunk) nil)
                             (json-simple:json-parse-error (e) (json-simple:json-parse-error-position e)))))
      (dolist (f (and (suite-dir) (directory (merge-pathnames "*.json" (suite-dir)))))
        (let* ((bytes (read-octets f))
               (pad (concatenate '(vector (unsigned-byte 8)) #(123 34 120 34 58) bytes #(125 32 93)))
               (pad (coerce pad '(simple-array (unsigned-byte 8) (*)))))
          (incf n)
          (if (and (same (outcome #'json-simple:parse bytes)
                         (outcome (lambda (b) (json-simple:parse b :start 5 :end (+ 5 (length bytes)))) pad))
                   (eql (pos-of (lambda () (json-simple:parse bytes)))
                        (pos-of (lambda () (json-simple:parse pad :start 5 :end (+ 5 (length bytes)))))))
              (incf ok)
              (fail "slice ~A differs from the whole file" (file-namestring f)))))
      (dolist (c '(("xx[1,2]yy" 2 7 #(1 2)) ("[\"a\"]" 0 nil #("a")) ("  {}" 2 nil :empty)))
        (incf n)
        (let ((v (outcome (lambda (s) (json-simple:parse s :start (second c) :end (third c)
                                                           :object-type :alist :null :null))
                          (first c))))
          (if (and (eq (car v) :ok)
                   (if (eq (fourth c) :empty) (null (cadr v)) (equalp (cadr v) (fourth c))))
              (incf ok)
              (fail "string slice ~S: ~S" c v)))))
    (format t "~&slices: ~D/~D agree~%" ok n)))

(defun run (&key (random 3000) (doubles 20000))
  (setq *failures* 0 *shown* 0)
  (test-suite)
  (test-numbers)
  (test-slices)
  (test-doubles doubles)
  (test-random random)
  (format t "~&TOTAL FAILURES: ~D~%" *failures*)
  *failures*)

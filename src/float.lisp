;;;; float.lisp — floats to the shortest JSON text that reads back the same
;;;; float, and decimal JSON numbers to correctly rounded doubles.
;;;;
;;;; Both directions are EXACT rational arithmetic rather than a port of
;;;; Schubfach / Eisel-Lemire: the digits are the shortest that round-trip,
;;;; chosen as the closest at that length, which is what those algorithms
;;;; produce, and the rounding is whatever FLOAT of a rational gives (correct
;;;; rounding).  Slower per number than the specialised algorithms, which does
;;;; not matter for the JSON this stack moves (mostly strings and integers);
;;;; small and obviously right does.

(in-package #:json-simple)

(defun %expt10 (k)
  "10^K as an exact rational, for any integer K."
  (if (>= k 0) (expt 10 k) (/ 1 (expt 10 (- k)))))

(defun %floor-log2 (r)
  "The integer E with 2^E <= R < 2^(E+1), for a positive rational R."
  (let ((e (- (integer-length (numerator r)) (integer-length (denominator r)))))
    ;; integer-length difference is E or E+1.
    (if (< r (if (>= e 0) (expt 2 e) (/ 1 (expt 2 (- e))))) (1- e) e)))

(defun %floor-log10 (r)
  "The integer E with 10^E <= R < 10^(E+1), for a positive rational R.  The
   estimate comes from the binary exponent, not from a float LOG, so it is
   right on hosts whose LOG mishandles subnormals."
  (let ((e (floor (* (%floor-log2 r) 30103) 100000)))
    (loop while (< r (%expt10 e)) do (decf e))
    (loop while (>= r (%expt10 (1+ e))) do (incf e))
    e))

(defun %rounding-interval (x)
  "For a positive finite float X = M x 2^K: (values LOW HIGH INCLUSIVE), the
   exact rationals bounding every real number that rounds to X under IEEE
   round-to-nearest-even, and whether the bounds themselves round to X (they
   do when M is even).  The gap below is half the usual when X is a power of
   two above the smallest normal.  Pure rational arithmetic: no conversion
   back to a float, which some hosts get wrong for subnormals."
  (multiple-value-bind (m k) (integer-decode-float x)
    (let* ((p (float-digits x))
           (ulp (expt 2 k))
           (half (/ ulp 2))
           (r (* m ulp))
           (low-half (if (and (= m (expt 2 (1- p)))
                              (> k (nth-value 1 (integer-decode-float
                                                 (if (typep x 'double-float)
                                                     least-positive-normalized-double-float
                                                     least-positive-normalized-single-float)))))
                         (/ half 2)
                         half)))
      (values (- r low-half) (+ r half) (evenp m)))))

(defun %shortest-digits (x)
  "For a positive finite float X: (values DIGITS E10), DIGITS the shortest
   decimal digit string (no trailing zeros, at least one digit) such that
   D.IGITS x 10^E10 reads back as X, the closest such at that length."
  (let* ((r (rational x))
         (e10 (%floor-log10 r))
         (max-n (if (typep x 'double-float) 17 9)))
    (multiple-value-bind (low high inclusive) (%rounding-interval x)
      (loop for n from 1 to max-n do
        (let* ((q (round (* r (%expt10 (- (1- n) e10)))))
               (ee e10))
          ;; Rounding up can carry into an extra digit (9.99.. -> 10.0..).
          (when (= q (expt 10 n))
            (setq q (expt 10 (1- n)) ee (1+ e10)))
          (let ((cand (* q (%expt10 (- ee (1- n))))))
            (when (if inclusive (<= low cand high) (< low cand high))
              (let* ((s (princ-to-string q))
                     (end (length s)))
                (loop while (and (> end 1) (char= (char s (1- end)) #\0)) do (decf end))
                (return-from %shortest-digits (values (subseq s 0 end) ee))))))))
    ;; Unreachable for IEEE floats (17 / 9 digits always round-trip).
    (error 'json-write-error :format-control "cannot print float ~S" :format-arguments (list x))))

(defun %float-text (x)
  "X (a float) as jzon writes it: plain decimal when its decimal exponent is in
   [-3, 7) -- 100.0, 0.0012 -- otherwise d.ddde<exp> -- 1.0e7, 2.5e-10.  Zero
   is 0.0 or -0.0."
  (let ((neg (minusp (float-sign x))))
    (if (zerop x)
        (if neg "-0.0" "0.0")
        (multiple-value-bind (d e) (%shortest-digits (abs x))
          (let ((n (length d)))
            (concatenate
             'string
             (if neg "-" "")
             (cond
               ((and (>= e 0) (< e 7))
                (if (> n (1+ e))
                    (concatenate 'string (subseq d 0 (1+ e)) "." (subseq d (1+ e)))
                    (concatenate 'string d (make-string (- (1+ e) n) :initial-element #\0) ".0")))
               ((and (< e 0) (>= e -3))
                (concatenate 'string "0." (make-string (- (- e) 1) :initial-element #\0) d))
               (t
                (concatenate 'string (subseq d 0 1) "." (if (> n 1) (subseq d 1) "0")
                             "e" (princ-to-string e))))))))))

(defun %decimal-double (negative mantissa ndigits exponent)
  "MANTISSA x 10^EXPONENT as a correctly rounded double, negated when NEGATIVE,
   or :OVERFLOW when it is out of range.  jzon's range rules: a value up to one
   unit past the largest double reads as the largest double, anything bigger is
   an error, and so is a nonzero value that rounds to zero.  NDIGITS is
   MANTISSA's decimal length, used to refuse absurd exponents before computing
   (EXPT 10 ...) -- \"1e999999999\" must not allocate a gigabyte."
  (let ((mag (+ ndigits exponent)))
    (cond
      ((zerop mantissa) (if negative -0d0 0d0))
      ((or (> mag 310) (< mag -330)) :overflow)
      (t
       (let ((r (* mantissa (%expt10 exponent))))
         (if (>= r most-positive-double-float)
             ;; In rationals: a double operand would turn R into a double first.
             (if (< r (+ (rational most-positive-double-float) (expt 2 971)))
                 (if negative (- most-positive-double-float) most-positive-double-float)
                 :overflow)
             (let ((d (%rational-double r)))
               (cond ((zerop d) :overflow)
                     (negative (- d))
                     (t d)))))))))

(defun %rational-double (r)
  "The double nearest the positive rational R (ties to even), 0d0 when it
   rounds below the smallest subnormal.  Built from an exact 53-bit (or
   subnormal-width) integer mantissa and SCALE-FLOAT, so the rounding does not
   depend on the host's FLOAT of a rational.  R must be below the largest
   double (the caller checks)."
  (let* ((e2 (%floor-log2 r))
         ;; Scale so the mantissa has 53 bits, but never below the subnormal
         ;; grid 2^-1074.
         (shift (max (- e2 52) -1074))
         (m (round (* r (if (>= shift 0) (/ 1 (expt 2 shift)) (expt 2 (- shift)))))))
    (when (= m (expt 2 53))               ; rounding carried into bit 54
      (setq m (expt 2 52) shift (1+ shift)))
    (if (zerop m)
        0d0
        (%scale2 (float m 1d0) shift))))

(defun %scale2 (d k)
  "D x 2^K, in steps of at most 2^-500 so no step needs a power of two outside
   the double range: some hosts' SCALE-FLOAT builds 2^|K| as a double and
   signals overflow for K below -1023 instead of producing a subnormal.  With D
   an exact integer below 2^53 every step is exact, and the last one rounds
   into the subnormal grid the caller already aligned to."
  (loop while (< k -500) do
    (setq d (* d (scale-float 1d0 -500)) k (+ k 500)))
  (scale-float d k))

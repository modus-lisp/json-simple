;;;; package.lisp

(defpackage #:json-simple
  (:use #:cl)
  (:export #:parse #:stringify #:write-json-string
           #:json-error #:json-parse-error #:json-write-error
           #:json-parse-error-position))

(in-package #:json-simple)

(define-condition json-error (simple-error) ()
  (:documentation "Every error json-simple signals."))

(define-condition json-parse-error (json-error)
  ((position :initarg :position :initform nil :reader json-parse-error-position))
  (:documentation "Malformed JSON, or a limit (depth) exceeded.  POSITION is the
character index the parser had reached."))

(define-condition json-write-error (json-error) ()
  (:documentation "A value STRINGIFY cannot represent."))

#!/bin/sh
# run-tests.sh — json-simple's differential tests against jzon, on SBCL.
cd "$(dirname "$0")"
exec sbcl --noinform --non-interactive \
  --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(ql:quickload "json-simple/test" :silent t)' \
  --eval '(uiop:quit (if (zerop (json-simple-test:run)) 0 1))'

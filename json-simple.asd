;;;; json-simple.asd — the stack's JSON: parse and stringify as plain functions.

(asdf:defsystem :json-simple
  :description "Simple JSON for the stack: plain Common Lisp functions, no dependencies.
PARSE and STRINGIFY are drop-in for com.inuoe.jzon's (same value mapping, same
output byte for byte, pretty or not), written as plain functions -- no generic
functions, no CLOS, no Gray streams -- so it is fast on modus as well as SBCL.
Also the NIP-01 canonical string escaper Nostr event ids need."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "package") (:file "float")
                             (:file "parse") (:file "write")))))

(asdf:defsystem :json-simple/test
  :description "Differential tests against com.inuoe.jzon: JSONTestSuite, random
values both ways, and floats."
  :depends-on ("json-simple" "com.inuoe.jzon")
  :serial t
  :components ((:module "test" :serial t :components ((:file "oracle")))))

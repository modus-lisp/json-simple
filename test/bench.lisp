;;;; test/bench.lisp — json-simple vs jzon on an LLM-chat-request-shaped message.
;;;; Load json-simple and com.inuoe.jzon, then this file, then (run-bench N).

(defun ht (&rest kv) (let ((h (make-hash-table :test 'equal))) (loop for (k v) on kv by #'cddr do (setf (gethash k h) v)) h))
(defparameter *msg*
  (ht "model" "deepseek/deepseek-v4-flash" "temperature" 0.7d0 "stream" t "max_tokens" 4096
      "messages" (coerce (loop for i below 12 collect
                                (ht "role" (if (evenp i) "user" "assistant")
                                    "content" (format nil "Message ~d: please look at src/engine.lisp and explain the \"run\" loop.~%It has tabs	and unicode é ✓ ~d" i (* i 1000))))
                         'vector)
      "tools" (coerce (loop for i below 8 collect
                             (ht "type" "function"
                                 "function" (ht "name" (format nil "tool_~d" i) "description" "Reads a file from the workspace and returns its contents."
                                                "parameters" (ht "type" "object" "properties" (ht "path" (ht "type" "string") "limit" (ht "type" "integer")) "required" (vector "path")))))
                      'vector)))
(defun ms (t0) (/ (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second) 1.0))
(defmacro bench (name n form) `(let ((t0 (get-internal-real-time))) (dotimes (i ,n) ,form) (format t "~&@@ ~28a ~8,3f ms/op~%" ,name (/ (ms t0) ,n))))
(defun run-bench (n)
  (let* ((text (com.inuoe.jzon:stringify *msg*)))
    (format t "~&@@ message ~d bytes~%" (length text))
    (assert (string= text (json-simple:stringify *msg*)))
    (bench "jzon stringify" n (com.inuoe.jzon:stringify *msg*))
    (bench "json-simple stringify" n (json-simple:stringify *msg*))
    (bench "jzon parse" n (com.inuoe.jzon:parse text))
    (bench "json-simple parse" n (json-simple:parse text))))

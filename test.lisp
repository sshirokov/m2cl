
(in-package :m2cl)

(defun run-test ()
  (with-handler (handler "dev" "tcp://127.0.0.1:8090" "tcp://127.0.0.1:8091")
    (loop
       (let ((message (handler-receive handler)))
         (format t "message: ~A~%" message)))))
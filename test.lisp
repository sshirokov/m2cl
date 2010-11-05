
(in-package :m2cl)

(defun run-test ()
  (with-handler (handler "dev" "tcp://127.0.0.1:8090" "tcp://127.0.0.1:8091")
    (loop
       (let ((request (handler-receive handler)))
         (with-slots (sender connection-id path headers body) request
           (format t "~&message~%")
           (format t "  sender: ~A~%" sender)
           (format t "  connection-id: ~A~%" connection-id)
           (format t "  path: ~A~%" path)
           (format t "  headers:~%")
           (maphash (lambda (name value)
                      (format t "    ~A: ~A~%" name value))
                    headers)
           (format t "  body: ~A~%" body))))))
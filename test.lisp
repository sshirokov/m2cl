
(in-package :m2cl)

(defun run-test ()
  (with-handler (handler "dev" "tcp://127.0.0.1:8090" "tcp://127.0.0.1:8091")
    (loop
       (process-request handler (handler-receive handler)))))

(defun process-request (handler request)
  (with-slots (sender connection-id path headers body) request
    (format t "~&message~%")
    (format t "  sender: ~A~%" sender)
    (format t "  connection-id: ~A~%" connection-id)
    (format t "  path: ~A~%" path)
    (format t "  headers:~%")
    (dolist (header headers)
      (format t "    ~A: ~A~%" (car header) (cdr header)))
    (format t "  body: ~A~%" body))
  (handler-reply-http handler request "ok"
                      :headers '(("Content-Type" . "text/plain"))))
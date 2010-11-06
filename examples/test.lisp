
(in-package :m2cl-examples)

(defun run-test ()
  (m2cl:with-handler (handler "dev" "tcp://127.0.0.1:8090" "tcp://127.0.0.1:8091")
    (loop
       (process-request handler (m2cl:handler-receive-json handler)))))

(defun process-request (handler request)
  (format t "~&message~%")
  (format t "  sender: ~A~%" (m2cl:request-sender request))
  (format t "  connection-id: ~A~%" (m2cl:request-connection-id request))
  (format t "  path: ~A~%" (m2cl:request-path request))
  (format t "  headers:~%")
  (dolist (header (m2cl:request-headers request))
    (format t "    ~A: ~A~%" (car header) (cdr header)))
  (format t "  body: ~A~%" (m2cl:request-body request))
  (format t "  data: ~A~%" (m2cl:request-data request))
  (m2cl:handler-reply-http handler request (format nil "{}~%")
                      :headers '(("Content-Type" . "text/javascript"))))
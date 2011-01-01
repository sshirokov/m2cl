
(in-package :m2cl-examples)

(defvar *users* (make-hash-table))

(define-condition chat-error (error)
  ((text
    :initarg :text
    :reader chat-error-text)))

(defun get-user-list ()
  (let ((users (list)))
    (maphash (lambda (connection-id user)
               (declare (ignore connection-id))
               (push user users))
             *users*)
    users))

(defun get-connections ()
  (let ((connections (list)))
    (maphash (lambda (connection-id user)
               (declare (ignore user))
               (push connection-id connections))
             *users*)
    connections))

(defvar *chat-thread* nil)

(defun chat-start ()
  (when *chat-thread*
    (error "Handler is already running"))
  (setf *chat-thread*
        (bordeaux-threads:make-thread 'chat-loop
                                      :name "chat handler")))

(defun chat-stop ()
  (when *chat-thread*
    (when (bordeaux-threads:thread-alive-p *chat-thread*)
      (bordeaux-threads:destroy-thread *chat-thread*))
    (setf *chat-thread* nil)))

(defun chat-restart ()
  (chat-stop)
  (chat-start))

(defun chat-loop ()
  (m2cl:with-handler (handler "chat" "tcp://127.0.0.1:8092" "tcp://127.0.0.1:8093")
    (loop
       (let ((request (m2cl:handler-receive handler)))
         (handler-case
             (chat-request-process handler request)
           (chat-error (cond)
             (m2cl:handler-reply-json handler request
                                      `((:error . ,(chat-error-text cond))))))))))

(defun chat-request-process (handler request)
  (format t "[~A] message: ~A~%"
          (m2cl:request-connection-id request)
          (m2cl:request-data request))
  (let ((message (m2cl:request-data request))
        (connection-id (m2cl:request-connection-id request)))
    (flet ((reply (data)
             (m2cl:handler-reply-json handler request data))
           (deliver (data)
             (m2cl:handler-deliver-json handler (m2cl:request-sender request)
                                        (get-connections) data))
           (has-field-p (key)
             (not (null (assoc key message))))
           (get-field (key)
             (let ((field (assoc key message)))
               (if field
                   (cdr field)
                   (error 'chat-error
                          :text (format nil "invalid message: missing field ~A"
                                        key))))))
      (if (has-field-p :type)
          (let ((type (get-field :type)))
            (cond
              ((string= type "disconnect")
               (remhash connection-id *users*)
               (deliver `((:op . "userList")
                          (:users . ,(get-user-list)))))))
          (let ((op (get-field :op)))
            (cond
              ((string= op "join")
               (let ((nick (get-field :nick)))
                 (setf (gethash connection-id *users*) nick)
                 (deliver `((:op . "userJoined")
                            (:user . ,nick)))
                 (deliver `((:op . "userList")
                            (:users . ,(get-user-list))))
                 (reply '((:ok . t)))))
              ((string= op "setNick")
               (let ((old-nick (gethash connection-id *users*))
                     (new-nick (get-field :nick)))
                 (setf (gethash connection-id *users*) new-nick)
                 (deliver `((:op . "userList")
                            (:users . ,(get-user-list))))
                 (deliver `((:op . "nickChange")
                            (:old-nick . ,old-nick)
                            (:new-nick . ,new-nick))))
               (reply '((:ok . t))))
              ((string= op "message")
               (deliver `((:op . "message")
                          (:user . ,(get-field :user))
                          (:message . ,(get-field :message))))
               (reply '((:ok . t))))
              (t (error 'chat-error
                        :text (format nil "invalid message: unknown operation ~A"
                                      op)))))))))
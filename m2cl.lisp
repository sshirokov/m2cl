
(in-package :m2cl)

(defclass handler ()
  ((pull-socket
    :accessor handler-pull-socket)
   (pub-socket
    :accessor handler-pub-socket)))

(defclass request ()
  ((sender
    :accessor request-sender
    :initarg :sender)
   (connection-id
    :accessor request-connection-id
    :initarg :connection-id)
   (path
    :accessor request-path
    :initarg :path)
   (headers
    :accessor request-headers
    :initarg :headers)
   (body
    :initarg :body
    :accessor request-body)))

(defmacro with-handler ((handler sender-id sub-address pub-address)
                        &body body)
  (let ((context (gensym)))
    `(let ((,handler (make-instance 'handler)))
       (zmq:with-context (,context 1)
         (zmq:with-socket (pull-socket ,context zmq:pull)
           (setf (handler-pull-socket ,handler) pull-socket)
           (zmq:connect pull-socket ,sub-address)
           (zmq:with-socket (pub-socket ,context zmq:pub)
             (setf (handler-pub-socket ,handler) pub-socket)
             (zmq:connect pub-socket ,pub-address)
             (zmq:setsockopt pub-socket zmq:identity ,sender-id)
             ,@body))))))

(defun handler-receive (handler)
  (let ((message (make-instance 'zmq:msg)))
    (zmq:recv (handler-pull-socket handler) message)
    (request-parse (zmq:msg-data-as-string message))))

(defun netstring-parse (string)
  (let ((colon (position #\: string)))
    (unless colon
      (error "colon not found in netstring"))
    (let* ((length (parse-integer string :end colon))
           (data (subseq string (+ colon 1) (+ colon length 1))))
      (unless (char= (aref string (+ colon length 1)) #\,)
        (error "netstring doesn't end with comma"))
      (values data (subseq string (+ colon length 2))))))

(defun json-parse (string)
  (json:decode-json-from-string string))

(defun headers-parse (string)
  (let ((json:*json-identifier-name-to-lisp* 'identity))
    (let ((alist (json-parse string))
          (table (make-hash-table :test 'equal)))
      (loop for (key . value) in alist
           do (setf (gethash key table) value))
      table)))

(defun request-parse (string)
  (ppcre:register-groups-bind (sender connection-id path rest)
      ("(\\S+) (\\S+) (\\S+) (.*)" string)
    (multiple-value-bind (headers-string rest)
        (netstring-parse rest)
      (make-instance 'request
                     :sender sender
                     :connection-id connection-id
                     :path path
                     :headers (headers-parse headers-string)
                     :body (netstring-parse rest)))))

(defun format-crlf (stream format-string &rest args)
    (format stream "~?~C~C"
            format-string args #\Return #\Linefeed))

(defun handler-send (handler uuid connection-id data)
  (zmq:send (handler-pub-socket handler)
            (make-instance 'zmq:msg
                           :data (format nil "~A ~A:~A, ~A"
                                         uuid
                                         (length connection-id)
                                         connection-id
                                         data))))

(defun handler-deliver (handler uuid connection-ids data)
  (handler-send handler uuid (format nil "~{~A~^ ~}" connection-ids) data))

(defun handler-reply (handler request string)
  (handler-send handler
                (request-sender request)
                (request-connection-id request)
                string))

(defun handler-reply-http (handler request body
                           &key
                           (code 200)
                           (status "OK")
                           (headers (make-hash-table :test 'eql)))
  (handler-reply handler request (http-format body code status headers)))

(defun handler-deliver-http (handler uuid connection-ids body
                             &key
                             (code 200)
                             (status "OK")
                             (headers (make-hash-table :test 'eql)))
  (handler-deliver handler uuid connection-ids
                   (http-format body code status headers)))

(defun http-format (body code status headers)
  (with-output-to-string (stream)
    (format-crlf stream "HTTP/1.1 ~A ~A" code status)
    (format-crlf stream "Content-Length: ~A" (length body))
    (maphash (lambda (name value)
               (format-crlf stream "~A: ~A" name value))
             headers)
    (format-crlf stream "")
    (format-crlf stream "~A" body)))

(defun handler-close (handler request)
  (handler-reply handler request ""))

(defun handler-deliver-close (handler uuid connection-ids)
  (handler-deliver handler uuid connection-ids ""))

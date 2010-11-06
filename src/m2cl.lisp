
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
    :accessor request-body)
   (data
    :initarg :data
    :accessor request-data
    :initform nil)))

(defun request-header (request name &optional default)
  (let ((header (assoc name (request-headers request) :test 'string=)))
    (if header
        (cdr header)
        default)))

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

(defun handler-receive-json (handler)
  (let ((request (handler-receive handler)))
    (with-slots (data) request
      (unless data
        (setf data (json-parse (request-body request)))))
    request))

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
    (json-parse string)))

(defun request-parse (string)
  (ppcre:register-groups-bind (sender connection-id path rest)
      ("(\\S+) (\\d+) (\\S+) (.*)" string)
    (multiple-value-bind (headers-string rest)
        (netstring-parse rest)
      (let ((request (make-instance 'request
                                    :sender sender
                                    :connection-id (parse-integer
                                                    (coerce connection-id 'string))
                                    :path path
                                    :headers (headers-parse headers-string)
                                    :body (netstring-parse rest))))
        (when (string= (request-header request "METHOD") "JSON")
          (setf (request-data request)
                (json-parse (request-body request))))
        request))))

(defun format-crlf (stream format-string &rest args)
    (format stream "~?~C~C"
            format-string args #\Return #\Linefeed))

(defun handler-send (handler uuid connection-id data)
  (let* ((connection-id-string (format nil "~A" connection-id))
         (data (format nil "~A ~A:~A, ~A"
                       uuid
                       (length connection-id-string)
                       connection-id-string
                       data)))
    (zmq:send (handler-pub-socket handler)
              (make-instance 'zmq:msg :data data))))

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
                           (headers (list)))
  (handler-reply handler request (http-format body code status headers)))

(defun handler-reply-json (handler request data)
  (handler-reply handler request (json:encode-json-to-string data)))

(defun handler-deliver-http (handler uuid connection-ids body
                             &key
                             (code 200)
                             (status "OK")
                             (headers (list)))
  (handler-deliver handler uuid connection-ids
                   (http-format body code status headers)))

(defun handler-deliver-json (handler uuid connection-ids data)
  (handler-deliver handler uuid connection-ids
                   (json:encode-json-to-string data)))

(defun http-format (body code status headers)
  (with-output-to-string (stream)
    (format-crlf stream "HTTP/1.1 ~A ~A" code status)
    (format-crlf stream "Content-Length: ~A" (length body))
    (dolist (header headers)
      (format-crlf stream "~A: ~A" (car header) (cdr header)))
    (format-crlf stream "")
    (format-crlf stream "~A" body)))

(defun handler-close (handler request)
  (handler-reply handler request ""))

(defun handler-deliver-close (handler uuid connection-ids)
  (handler-deliver handler uuid connection-ids ""))

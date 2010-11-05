
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

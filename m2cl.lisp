
(in-package :m2cl)

(defclass handler ()
  ((pull-socket
    :accessor handler-pull-socket)
   (pub-socket
    :accessor handler-pub-socket)))

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
    (zmq:msg-data-as-string message)))

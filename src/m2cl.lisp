
(in-package :m2cl)

(defparameter *default-input-character-encoding*
  babel:*default-character-encoding*
  "The default character encoding used when parsing requests.")

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
   (get-parameters
    :accessor request-get-parameters
    :initform nil)
   (body
    :initarg :body
    :accessor request-body)
   (data
    :initarg :data
    :accessor request-data
    :initform nil)))

(defgeneric handler-read-request (handler))
(defgeneric handler-receive (handler &key timeout))
(defgeneric handler-receive-json (handler &key timeout))

(defgeneric request-disconnect-p (request)
  (:documentation "Returns t if the given `request' is a disconnect message"))

(defun request-header (request name &optional default)
  (let ((header (assoc name (request-headers request) :test 'string=)))
    (if header
        (cdr header)
        default)))

(defun request-get (request name &optional default)
  (let ((get-parameter (assoc name (request-get-parameters request)
                              :test 'string=)))
    (if get-parameter
        (cdr get-parameter)
        default)))

(defmacro with-handler ((handler sender-id sub-address pub-address)
                        &body body)
  (let ((context (gensym)))
    `(let ((,handler (make-instance 'handler)))
       (zmq:with-context (,context 1)
         (with-handler-sockets (,handler ,context ,sender-id
                                         ,sub-address ,pub-address)
           ,@body)))))

(defmacro with-handler-sockets ((handler context sender-id
                                         sub-address pub-address)
                                &body body)
  `(zmq:with-sockets ((pull-socket ,context :pull)
                      (pub-socket ,context :pub))
     (setf (handler-pull-socket ,handler) pull-socket)
     (zmq:connect pull-socket ,sub-address)
     (setf (handler-pub-socket ,handler) pub-socket)
     (zmq:setsockopt pub-socket :identity ,sender-id)
     (zmq:connect pub-socket ,pub-address)
     ,@body))

(defmethod handler-read-request ((handler handler))
  "Read a single request from the pull socket of HANDLER."
  (zmq:with-msg-init (message)
    (zmq:recv (handler-pull-socket handler) message)
    (let* ((data (zmq:msg-data-array message))
           (request (request-parse data)))
      (values request data))))

(defmethod handler-receive ((handler handler) &key (timeout -1))
  "Poll the pull socket of HANDLER until there is an available message, read
it, and return the request it contains."
  (zmq:with-poll-items (items nb-items)
                       (((handler-pull-socket handler) :pollin))
    (when (> (zmq:poll items nb-items timeout))
      (when (zmq:poll-item-event-signaled-p (zmq:poll-items-aref items 0)
                                            :pollin)
        (handler-read-request handler)))))

(defmethod handler-receive-json ((handler handler) &key (timeout -1))
  (multiple-value-bind (request raw)
      (handler-receive handler :timeout timeout)
    (when request
      (with-slots (data) request
        (unless data
          (setf data (json-parse (request-body request))))))
    (values request raw)))

(defun netstring-parse (string)
  (let ((colon (position (char-code #\:) string)))
    (unless colon
      (error "colon not found in netstring"))
    (let* ((length-string (babel:octets-to-string string :end colon))
           (length (parse-integer length-string))
           (data (subseq string (+ colon 1) (+ colon length 1))))
      (unless (= (aref string (+ colon length 1)) (char-code #\,))
        (error "netstring doesn't end with comma"))
      (values
       (babel:octets-to-string data
                               :encoding *default-input-character-encoding*)
       (subseq string (+ colon length 2))))))

(defun json-parse (string)
  (json:decode-json-from-string string))

(defun headers-parse (string)
  (let ((json:*json-identifier-name-to-lisp* 'identity))
    (json-parse string)))

(defun url-decode (string)
  (let ((bytes (make-array (length string) :element-type '(unsigned-byte 8)
                           :fill-pointer 0)))
    (do ((i 0))
        ((= i (length string)))
      (let ((c (aref string i)))
        (incf i)
        (cond
          ((char= c #\%)
           (when (> (+ i 2) (length string))
             (error "Invalid url encoding, truncated character code"))
           (let ((code (parse-integer string :start i :end (+ i 2) :radix 16)))
             (vector-push code bytes)
             (incf i 2)))
          ((char= c #\+)
           (vector-push (char-code #\Space) bytes))
          (t
           (vector-push (char-code c) bytes)))))
    (babel:octets-to-string bytes
                            :encoding *default-input-character-encoding*)))

(defun query-parse (string)
  (let ((data (list))
        (pairs (ppcre:split "&|;" string)))
    (dolist (pair pairs)
      (let ((equal (position #\= pair)))
        (unless equal
          (error "Invalid query string encoding, equal separator not found"))
        (let* ((name (subseq pair 0 equal))
               (value (subseq pair (+ equal 1)))
               (decoded-value (url-decode value)))
          (push (cons name (if (string= decoded-value "")
                               nil
                               decoded-value))
                data))))
    (nreverse data)))

(defun token-parse (array)
  (let ((space (position (char-code #\Space) array)))
    (if space
        (values (babel:octets-to-string array :end space)
                (subseq array (+ space 1)))
        (values (babel:octets-to-string array) #()))))

(defun token-parse-n (array n)
  (labels ((get-next (acc rest c)
             (multiple-value-bind (token rest)
                 (token-parse rest)
               (if (< c n)
                   (get-next (cons token acc) rest (+ c 1))
                   (nreverse (cons rest (cons token acc)))))))
    (get-next (list) array 1)))

(defmethod request-disconnect-p ((request request))
  (and (string= (request-header request "METHOD") "JSON")
       (string= "disconnect" (cdr (assoc :type (request-data request))))))

(defun request-parse (array)
  (destructuring-bind (sender connection-id-string path rest)
      (token-parse-n array 3)
    (let ((connection-id (parse-integer (coerce connection-id-string 'string))))
      (restart-case
          (multiple-value-bind (headers rest)
              (netstring-parse rest)
            (let ((request (make-instance 'request
                                          :sender sender
                                          :connection-id connection-id
                                          :path path
                                          :headers (headers-parse headers)
                                          :body (netstring-parse rest))))
              (when (string= (request-header request "METHOD") "JSON")
                (setf (request-data request)
                      (json-parse (request-body request))))
              (let ((query (request-header request "QUERY")))
                (when query
                  (setf (request-get-parameters request)
                        (query-parse query))))
              request))

        (:reply-http-400 (handler &optional (body "Request parser has failed."))
          (ignore-errors
           (handler-send-http handler body
                              :code 400 :status "Bad Request"
                              :uuid sender :connections (list connection-id)))
          nil)))))

(defun handler-send (handler data
                     &key uuid connections request)
  (assert (or request
              (and uuid connections)))
  (let* ((uuid (or uuid (request-sender request)))
         (connections (or connections (list (request-connection-id request))))
         (connections-string (format nil "~{~A~^ ~}" connections))
         (data-sequence (etypecase data
                          (string (babel:string-to-octets data))
                          (vector data)))
         (msg-data (flex:with-output-to-sequence (stream)
                     (write-sequence (babel:string-to-octets
                                      (format nil "~A ~A:~A, "
                                              uuid
                                              (length connections-string)
                                              connections-string))
                                     stream)
                     (write-sequence data-sequence stream))))
    (zmq:with-msg-init-data (message msg-data)
      (zmq:send (handler-pub-socket handler) message))))

(defun handler-send-json (handler data
                          &key uuid connections request)
  (handler-send handler (json:encode-json-to-string data)
                :uuid uuid
                :connections connections
                :request request))

(defun handler-send-http-trailers (handler trailers &key
                                   uuid connections request)
  (handler-send handler (flex:with-output-to-sequence (stream)
                          (http-headers-format trailers stream)
                          (format-crlf stream ""))
                :uuid uuid
                :connections connections
                :request request))

(defun handler-send-http-chunked (handler &key
                                  uuid connections request
                                  (code 200) (status "OK")
                                  (headers (list)))
  (handler-send handler (http-format-chunked code status headers)
                :uuid uuid
                :connections connections
                :request request))

(defun handler-send-http-chunked-finish (handler &key uuid connections request)
  (handler-send-http-chunk handler ""
                           :uuid uuid :connections connections :request request))

(defun handler-send-http-chunk (handler body
                                &key
                                uuid connections request
                                binary-body-p)
  (let* ((body (if binary-body-p
                   body
                   (babel:string-to-octets body)))
         (chunk (flex:with-output-to-sequence (stream)
                  (format-crlf stream "~X" (length body))
                  (write-sequence body stream)
                  (format-crlf stream ""))))
    (handler-send handler chunk
                  :uuid uuid
                  :connections connections
                  :request request)))

(defun handler-send-http (handler body
                          &key
                          uuid connections request
                          (code 200) (status "OK")
                          (headers (list))
                          binary-body-p)
  (let ((body (if binary-body-p
                  body
                  (babel:string-to-octets body))))
    (handler-send handler (http-format body code status headers)
                  :uuid uuid
                  :connections connections
                  :request request)))

(defun format-crlf (stream format-string &rest args)
  (let ((string (format nil "~?~C~C"
                        format-string args #\Return #\Linefeed)))
    (write-sequence (babel:string-to-octets string) stream)))

(defun http-headers-format (headers &optional alt-stream)
  (flex:with-output-to-sequence (stream)
    (dolist (header headers)
      (format-crlf (or alt-stream stream) "~A: ~A" (car header) (cdr header)))))

(defmacro with-common-http-format ((code status headers) head-section &rest content-section)
  `(flex:with-output-to-sequence (stream)
     (format-crlf stream "HTTP/1.1 ~A ~A" ,code ,status)
     (http-headers-format ,headers stream)
     ,head-section
     (format-crlf stream "")
     ,@content-section))

(defun http-format-chunked (code status headers)
  (with-common-http-format (code status headers)
    (format-crlf stream "Transfer-Encoding: chunked")))

(defun http-format (body code status headers)
  (with-common-http-format (code status headers)
    (format-crlf stream "Content-Length: ~A" (length body))
    (write-sequence body stream)))


(defun handler-close (handler &key uuid connections request)
  (handler-send handler ""
                :uuid uuid :connections connections :request request))

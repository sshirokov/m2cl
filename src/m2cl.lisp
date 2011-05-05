
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

(defgeneric handler-receive (handler))
(defgeneric handler-receive-json (handler))

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
         (zmq:with-socket (pull-socket ,context zmq:pull)
           (setf (handler-pull-socket ,handler) pull-socket)
           (zmq:connect pull-socket ,sub-address)
           (zmq:with-socket (pub-socket ,context zmq:pub)
             (setf (handler-pub-socket ,handler) pub-socket)
             (zmq:connect pub-socket ,pub-address)
             (zmq:setsockopt pub-socket zmq:identity ,sender-id)
             ,@body))))))

(defmethod handler-receive ((handler handler))
  (let* ((message (make-instance 'zmq:msg))
         (raw (progn (zmq:recv (handler-pull-socket handler) message)
                     (zmq:msg-data-as-array message))))
    (values (request-parse raw) raw)))

(defmethod handler-receive-json ((handler handler))
  (multiple-value-bind (request raw) (handler-receive handler)
    (with-slots (data) request
      (unless data
        (setf data (json-parse (request-body request)))))
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
      (values (babel:octets-to-string data)
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
    (babel:octets-to-string bytes)))

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

(defun request-parse (array)
  (destructuring-bind (sender connection-id path rest)
      (token-parse-n array 3)
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
        (let ((query (request-header request "QUERY")))
          (when query
            (setf (request-get-parameters request)
                  (query-parse query))))
        request))))

(defun handler-send (handler data
                     &key uuid connections request)
  (assert (or request
              (and uuid connections)))
  (let* ((uuid (or uuid (request-connection-id request)))
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
    (zmq:send (handler-pub-socket handler)
              (make-instance 'zmq:msg :data msg-data))))

(defun handler-send-json (handler data
                          &key uuid connections request)
  (handler-send handler (json:encode-json-to-string data)
                :uuid uuid
                :connections connections
                :request request))

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

(defun http-format (body code status headers)
  (flex:with-output-to-sequence (stream)
    (format-crlf stream "HTTP/1.1 ~A ~A" code status)
    (format-crlf stream "Content-Length: ~A" (length body))
    (dolist (header headers)
      (format-crlf stream "~A: ~A" (car header) (cdr header)))
    (format-crlf stream "")
    (write-sequence body stream)))

(defun handler-close (handler &key uuid connections request)
  (handler-send handler ""
                :uuid uuid :connections connections :request request))

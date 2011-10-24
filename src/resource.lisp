
(in-package :m2cl)

(deftype http-method ()
  "A HTTP method."
  '(member :get :head :post :put :delete))

(defclass resource ()
  ((method
    :accessor resource-method
    :initarg :method
    :type 'http-method
    :documentation "The HTTP method applied to the resource.")
   (path
    :accessor resource-path
    :initarg :path
    :documentation "The path where the resource is located.")
   (handler
    :accessor resource-handler
    :initarg :handler
    :type '(or symbol function)
    :documentation "The function used to handle any request matching this
    method and path."))
  (:documentation "A resource handler."))

(defgeneric resource-match-p (resource request)
  (:documentation "Return whether the handler associated to RESOURCE can be
  used to process REQUEST."))

(defmethod print-object ((resource resource) stream)
  (print-unreadable-object (resource stream :type t :identity nil)
    (with-slots (method path) resource
      (format stream "~A ~A" (symbol-name method) path))))

(defmethod initialize-instance :after ((resource resource)
                                       &key &allow-other-keys)
  (check-type (resource-method resource) http-method))

(defmethod resource-match-p ((resource resource) (request request))
  (and (string= (symbol-name (resource-method resource))
                (request-header request "METHOD"))
       (string= (resource-path resource) (request-path request))))

(defun resource= (resource-1 resource-2)
  "Return T if RESOURCE-1 and RESOURCE-2 are equal or NIL if they are not. Two
resources are equal if they have the same method and path."
  (and (eq (resource-method resource-1) (resource-method resource-2))
       (string= (resource-path resource-1) (resource-path resource-2))))

;;; Resources are stored in a resource set, ie. a tree of resources.
;;;
;;; For example for the following list of resources:
;;;
;;; POST /login
;;; POST /logout
;;; GET  /article
;;; POST /article
;;; GET  /article/search
;;; POST /article/index/update
;;;
;;; The tree representation will be:
;;;
;;;                            +
;;;                            |
;;;        +------------+------------+--------------+
;;;        |            |            |              |
;;;    POST login   POST logout   GET article   POST article
;;;                                  |
;;;                            +-----+-----+
;;;                            |           |
;;;                         GET search   index
;;;                                        |
;;;                                    POST update
;;;
;;; The tree is stored as a simple S-expression where each element is either
;;; an instance of the RESOURCE class (if a resource is associated with the
;;; path) or a simple string.
;;;
;;; (list #<RESOURCE POST login>
;;;       #<RESOURCE POST logout>
;;;       (list #<RESOURCE GET article>
;;;             #<RESOURCE GET search>
;;;             (list "index"
;;;                   #<RESOURCE POST update>))
;;;       #<RESOURCE POST article>)
;;;
;;; POST "/article/index/update"
;;; => (search '("article" "index" "update")
;;;            '(#<P login> #<P logout> (#<G article> #<G search> ("index" #<P update>)) #<P article>))
;;;
;;;    (search '("index" "update")
;;;            '(#<G article> #<G search> ("index" #<P update>)))
;;;
;;;    (search '("update")
;;;            '("index" #<P update>))
;;;
;;;    #<P update>

(defclass resource-set ()
  ((tree
    :accessor resource-set-tree
    :initform nil
    :type '(or nil list)
    :documentation "The tree of resources."))
  (:documentation "A set of resources."))

(defvar *resource-set* (make-instance 'resource-set)
  "The resource set used to dispatch requests.")

(defun resource-set-add (resource-set resource)
  "Insert RESOURCE in RESOURCE-SET. If RESOURCE-SET already contains a
  resource which is equal to RESOURCE according to RESOURCE=, the resource in
  the tree is replaced by RESOURCE."
  (declare (ignore resource-set resource)))

(defun parse-absolute-path (path)
  "Parse an absolute path and return a list of path segments. PATH must not be
url encoded. For example, (PATH-PARSE \"/foo/bar/baz\") => (\"foo\" \"bar\"
\"baz\")."
  (unless (char= (char path 0) #\/)
    (error "Path isn't absolute."))
  (cdr (ppcre:split "/+" path)))

(defun request-search-match (request)
  "Search for a resource matching REQUEST in *RESOURCE-SET* and
  return it. Return NIL if no matching resource was found."
  (labels ((search-match (path tree)
             (declare (ignore path tree))
             nil))
    (search-match (parse-absolute-path (request-path request))
                  (resource-set-tree *resource-set*))))

(defun request-dispatch (request)
  "Search for a resource matching REQUEST and call its handler. Return two
values. The first one is the value returned by the handler, or NIL if no
matching resource was found. The second one is T if a matching resource was
found, or NIL otherwise."
  (declare (ignore request))
  (values nil nil))

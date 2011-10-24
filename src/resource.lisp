
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

(defmethod print-object ((resource resource) stream)
  (print-unreadable-object (resource stream :type t :identity nil)
    (with-slots (method path) resource
      (format stream "~A ~A" (symbol-name method) path))))

(defmethod initialize-instance :after ((resource resource)
                                       &key &allow-other-keys)
  (check-type (resource-method resource) http-method))

(defclass resource-set ()
  ((tree
    :accessor resource-set-tree
    :initform nil
    :type '(or nil list)
    :documentation "The tree of resources."))
  (:documentation "A set of resources."))

(defvar *resource-set* (make-instance 'resource-set)
  "The resource set used to dispatch requests.")

;;; Resources are stored in a tree.
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
;;;             #<RESOURCE POST save>
;;;             "index"
;;;             #<RESOURCE GET search>
;;;             #<RESOURCE POST article>))

(defun parse-absolute-path (path)
  "Parse an absolute path and return a list of path segments. PATH must not be
url encoded. For example, (PATH-PARSE \"/foo/bar/baz\") => (\"foo\" \"bar\"
\"baz\")."
  (unless (char= (char path 0) #\/)
    (error "Path isn't absolute."))
  (cdr (ppcre:split "/+" path)))


(in-package :m2cl-test)

(in-suite main)

(test request-search-match
    (flet ((request (method path)
             (make-instance 'm2cl::request
                            :path path
                            :headers `(("METHOD" . ,(symbol-name method)))))
           (resource-set (resources)
             (let ((resource-set (make-instance 'm2cl::resource-set)))
               (dolist (resource resources resource-set)
                 (destructuring-bind (method path id) resource
                   (m2cl::resource-set-add resource-set
                                           (make-instance 'm2cl::resource
                                                          :method method
                                                          :path path
                                                          :handler id)))))))
      (macrolet ((test-request-found (method path id)
                   `(let ((resource (m2cl::request-search-match
                                     (request ,method ,path))))
                      (is-true resource)
                      (when resource
                        (is (eq (m2cl::resource-handler resource) ,id)))))
                 (test-request-not-found (method path)
                   `(is-false (m2cl::request-search-match
                               (request ,method ,path)))))
        (let ((m2cl:*resource-set*
                (resource-set '((:post "/login" login)
                                (:post "/logout" logout)
                                (:get "/article" get-article)
                                (:post "/article" post-article)
                                (:get "/article/search" search)
                                (:post "/article/index/update" index)))))
          (test-request-found :post "/login" 'login)
          (test-request-found :post "/logout" 'logout)
          (test-request-found :get "/article" 'get-article)
          (test-request-found :post "/article" 'post-article)
          (test-request-found :get "/article/search" 'search)
          (test-request-found :get "/article/index/update" 'index)
          (test-request-not-found :get "/")
          (test-request-not-found :get "/login")
          (test-request-not-found :post "/log")
          (test-request-not-found :get "/article/index")
          (test-request-not-found :post "/article/index/update"))
        (let ((m2cl:*resource-set*
                (resource-set '((:get "/" root)
                                (:get "/index.html" index)))))
          (test-request-found :get "/" 'root)
          (test-request-found :get "/index.html" 'index)
          (test-request-not-found :post "/")))))


(defsystem m2cl
  :name "m2cl"
  :author "Nicolas Martyanoff"
  :license "BSD"
  :description "A handler for the mongrel2 HTTP server."
  :depends-on (:cl-json :cl-ppcre :zeromq)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "m2cl" :depends-on ("packages"))))))

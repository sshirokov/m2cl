
(defsystem m2cl-examples
  :name "m2cl-examples"
  :author "Nicolas Martyanoff"
  :license "BSD"
  :description "Examples for the m2cl mongrel2 handler."
  :depends-on (:m2cl)
  :components ((:module "examples"
                        :components ((:file "packages")
                                     (:file "test" :depends-on ("packages"))
                                     (:file "chat" :depends-on ("packages"))))))


(defsystem m2cl-test
  :name "m2cl-test"
  :author "Nicolas Martyanoff"
  :license "BSD"
  :description "Tests for the m2cl mongrel2 handler."
  :depends-on (:m2cl :fiveam)
  :components ((:module "test"
                :components ((:file "packages")
                             (:file "suites" :depends-on ("packages"))
                             (:file "encoding" :depends-on ("suites"))
                             (:file "resource" :depends-on ("suites"))))))

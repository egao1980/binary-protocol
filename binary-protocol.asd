(defsystem "binary-protocol"
  :version "0.1.0"
  :description "Python-struct pack/unpack (stack-binary)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "ieee")
               (:file "format")
               (:file "pack"))
  :in-order-to ((test-op (test-op "binary-protocol/tests"))))

(defsystem "binary-protocol/tests"
  :depends-on ("binary-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "format-test")
               (:file "pack-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

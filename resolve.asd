(defsystem "resolve"
  :description "Software difference resolution"
  :depends-on (resolve/core)
  :version "0.0.0"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :in-order-to ((test-op (test-op "resolve/test"))))

(register-system-packages "resolve/core" '(:resolve))

(defsystem "resolve/test"
  :description "Tests for resolve"
  :class :package-inferred-system  
  :defsystem-depends-on (:asdf-package-system)
  :perform (test-op (o c) (symbol-call :resolve/test '#:run-batch)))

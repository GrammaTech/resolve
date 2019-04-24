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

(defsystem "resolve/run-ast-diff"
  :author "GrammaTech"
  :description "Calculate difference between two programs."
  :version "0.0.0"
  :depends-on (resolve/commands)
  :build-operation "asdf:program-op"
  :build-pathname "bin/ast-diff"
  :entry-point "resolve/commands::run-ast-diff")

(defsystem "resolve/run-ast-merge"
  :author "GrammaTech"
  :description "Merge two programs that diverge from a common ancestor."
  :version "0.0.0"
  :depends-on (resolve/commands)
  :build-operation "asdf:program-op"
  :build-pathname "bin/ast-merge"
  :entry-point "resolve/commands::run-ast-merge")

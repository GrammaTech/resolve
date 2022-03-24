(defsystem "resolve"
  :description "Software difference resolution"
  :depends-on (resolve/core)
  :version "0.0.0"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :in-order-to ((test-op (load-op "resolve/test")))
  :perform (test-op (o c) (symbol-call :resolve/test '#:run-batch)))

(register-system-packages "resolve/core" '(:resolve))
(register-system-packages "fare-quasiquote-extras" '(:fare-quasiquote))
(register-system-packages "trivia" '(:trivia.fail))

(defsystem "resolve/run-ast-diff"
  :author "GrammaTech"
  :description "Calculate difference between two programs."
  :version "0.0.0"
  :depends-on (resolve/commands)
  :build-operation "asdf:program-op"
  :build-pathname "bin/ast-diff"
  :entry-point "resolve/commands::run-ast-diff")

(defsystem "resolve/run-serve-ast-diff"
    :description "REST `ast-diff' server from resolve."
    :depends-on ("resolve/routes/diff")
    :build-operation "asdf:program-op"
    :build-pathname "bin/serve-ast-diff"
    :entry-point "resolve/commands::run-serve-ast-diff")

(defsystem "resolve/run-serve-auto-merge"
    :description "REST `auto-merge' server from resolve."
    :depends-on ("resolve/routes/merge")
    :build-operation "asdf:program-op"
    :build-pathname "bin/serve-auto-merge"
    :entry-point "resolve/commands::run-serve-auto-merge")

(defsystem "resolve/run-ast-merge"
  :author "GrammaTech"
  :description "Merge two programs that diverge from a common ancestor."
  :version "0.0.0"
  :depends-on (resolve/commands)
  :build-operation "asdf:program-op"
  :build-pathname "bin/ast-merge"
  :entry-point "resolve/commands::run-ast-merge")

(defsystem "resolve/run-auto-merge"
  :author "GrammaTech"
  :description "Automatically two programs (w/ancestor) against a test script."
  :version "0.0.0"
  :depends-on (resolve/commands)
  :build-operation "asdf:program-op"
  :build-pathname "bin/auto-merge"
  :entry-point "resolve/commands::run-auto-merge")

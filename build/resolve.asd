(defsystem "resolve"
    :class asdf/bundle:prebuilt-system)

(defsystem "resolve/core"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               software-evolution-library/utility/git)
  :components ((:compiled-file "core")))

(defsystem "resolve/string"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt)
  :components ((:compiled-file "string")))

(defsystem "resolve/ast-diff"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               software-evolution-library/software/parseable
               software-evolution-library/software/clang
               software-evolution-library/software/simple
               software-evolution-library/software/ir
               software-evolution-library/view
               resolve/string
               metabang-bind
               cl-heap)
  :components ((:compiled-file "ast-diff")))

(defsystem "resolve/alist"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               software-evolution-library
               resolve/ast-diff)
  :components ((:compiled-file "alist")))

(defsystem "resolve/software/project"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               resolve/ast-diff
               resolve/alist
               software-evolution-library
               software-evolution-library/software/project)
  :components ((:compiled-file "software/project")))

(defsystem "resolve/software/auto-mergeable"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               metabang-bind
               resolve/alist
               resolve/ast-diff
               resolve/software/parseable
               software-evolution-library
               software-evolution-library/components/file
               software-evolution-library/software/clang
               software-evolution-library/software/simple
               software-evolution-library/software/parseable
               software-evolution-library/software/clang
               software-evolution-library/software/javascript
               software-evolution-library/software/json
               software-evolution-library/software/lisp
               software-evolution-library/software/project
               software-evolution-library/software/parseable-project
               software-evolution-library/software/clang-project
               software-evolution-library/software/javascript-project
               software-evolution-library/software/lisp-project)
  :components ((:compiled-file "software/auto-mergeable")))

(defsystem "resolve/software/parseable"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               resolve/ast-diff
               software-evolution-library
               software-evolution-library/software/parseable)
  :components ((:compiled-file "software/parseable")))

(defsystem "resolve/auto-merge"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               software-evolution-library
               software-evolution-library/utility/task
               software-evolution-library/utility/debug
               software-evolution-library/command-line
               software-evolution-library/components/lexicase
               software-evolution-library/components/test-suite
               software-evolution-library/software/parseable
               software-evolution-library/software/project
               software-evolution-library/software/clang
               software-evolution-library/software/clang-project
               software-evolution-library/software/simple
               resolve/core
               resolve/ast-diff
               resolve/alist
               resolve/software/project
               resolve/software/auto-mergeable
               resolve/software/parseable)
  :components ((:compiled-file "auto-merge")))

(defsystem "resolve/commands"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               resolve/core
               resolve/ast-diff
               resolve/auto-merge
               resolve/software/auto-mergeable
               resolve/software/parseable
               resolve/software/project
               resolve/software/lisp
               software-evolution-library
               software-evolution-library/utility/debug
               software-evolution-library/command-line
               software-evolution-library/command-line-rest
               software-evolution-library/software/parseable
               software-evolution-library/software/simple
               software-evolution-library/software/project
               software-evolution-library/software/clang
               software-evolution-library/software/clang-project
               software-evolution-library/software/javascript
               software-evolution-library/software/javascript-project
               software-evolution-library/software/json
               software-evolution-library/software/lisp)
  :components ((:compiled-file "commands")))

(defsystem "resolve/html"
  :class asdf/bundle:prebuilt-system
  :version "0.0.0"
  :depends-on (gt
               resolve/ast-diff
               cl-who)
  :components ((:compiled-file "html")))

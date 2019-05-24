;;;; auto-merge.lisp -- Main automatic merge command-line driver
(defpackage :resolve/auto-merge
  (:documentation "Main resolve command-line driver")
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :uiop
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :resolve/core
        :resolve/ast-diff
        :resolve/alist
        :resolve/software/project
        :resolve/software/parseable
        :resolve/software/lisp)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :resolve
           :populate
           :resolve-to))
(in-package :resolve/auto-merge)
(in-readtable :curry-compose-reader-macros)


;;; Utility functions
(defgeneric resolve-to (conflicted option)
  (:documentation "Resolve every conflict in CONFLICTED to OPTION.")
  (:method ((conflicted software) option &aux #+debug (cp (copy conflicted)))
    (nest
     #+debug (let ((counter 0))
               (to-file cp (format nil "/tmp/resolve-original.c")))
     ;; Modify the parent of all conflict nodes to replace with OPTION.
     (mapc (lambda (ast)
             #+debug (format t "Replacing conflict at ~S~%" (ast-path ast))
             (setf conflicted
                   (replace-ast conflicted ast
                                (aget option (conflict-ast-child-alist ast))
                                ;; Needs :literal t to avoid undefined
                                ;; recontextualization invoked on JS
                                ;; object.  Could also just define
                                ;; recontextaulization for JS.
                                :literal t))
             #+debug
             (to-file conflicted (format nil "/tmp/resolve-to-~d.c" counter))
             #+debug
             (to-file cp (format nil "/tmp/resolve-cp-~d.c" counter))
             #+debug (incf counter)))
     ;; Modify conflict nodes in reverse to work up the tree.
     (reverse (remove-if-not [{subtypep _ 'conflict-ast} #'type-of]
                             (asts conflicted))))
    ;; Maybe the problem in `resolve-to-selects-alternatives-of-conflicts'
    ;; relates to the conflict nodes at:
    ;; 5 3 1
    ;; 5 3 4
    conflicted))


;;; Actual population and evolution of resolution.
(defgeneric populate (conflicted)
  (:documentation "Build a population from MERGED and UNSTABLE chunks.
NOTE: this is exponential in the number of conflict ASTs in CONFLICTED.")
  (:method ((conflicted software))
    (nest
     ;; Initially population is just a list of the base object.
     (let ((pop (resolve-to conflicted :old))))
     (prog1 pop)
     (mapc
      (lambda (chunk)
        (setf pop
              (mappend (lambda (el)
                         ;; TODO: New variants for each possible resolution:
                         ;; 1. mine
                         ;; 2. your
                         ;; 3. mine+your
                         ;; 4. your+mine
                         ;; 5. neither
                         el)
                       pop)))
      ;; Conflicted chunks.
      (remove-if-not [{subtypep _ 'conflict-ast} #'type-of] (asts conflicted))))))

(defgeneric resolve (my old your test &key &allow-other-keys)
  (:documentation
   "Resolve merge conflicts between software versions MY OLD and YOUR.")
  (:method (test (my software) (old software) (your software)
            &key &allow-other-keys)
    (let ((*population* (multiple-value-call #'populate
                          (apply #'converge (list my old your)))))
      (evolve test))))

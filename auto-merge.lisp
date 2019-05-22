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
  (:export :resolve :run-resolve))
(in-package :resolve/auto-merge)
(in-readtable :curry-compose-reader-macros)


;;; Utility functions
(defgeneric resolve-to (conflicted option)
  (:documentation "Resolve every conflict in CONFLICTED to OPTION.")
  (:method ((conflicted software) option)
    (nest
     ;; Modify the parent of all conflict nodes to replace with OPTION.
     (mapc (lambda (conflict)
             (setf (ast-children (get-ast-parent conflict))
                   (aget option (conflict-ast-children conflict)))))
     ;; Modify conflict nodes in reverse to work up the tree.
     (reverse (remove-if-not [{subtypep _ 'conflict-ast} #'type-of] (asts cnf))))))

(defgeneric resolve-to-base (conflicted)
  (:documentation "Resolve every conflict in CONFLICTED to :BASE.")
  (:method ((conflicted software))
    ;; (resolve-to conflicted :base)
    (resolve-to conflicted 1))) ; FIXME: This is a temporary stand in.


;;; Actual population and evolution of resolution.
(defgeneric populate (conflicted)
  (:documentation "Build a population from MERGED and UNSTABLE chunks.
NOTE: this is exponential in the size of UNSTABLE.")
  (:method ((conflicted software))
    (nest
     ;; Initially population is just a list of the base object.
     (let ((pop (resolve-to-base ))))
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
      (remove-if-not [{subtypep _ 'conflict-ast} #'type-of] (asts cnf))))))

(defgeneric resolve (my old your test &key &allow-other-keys)
  (:documentation
   "Resolve merge conflicts between software versions MY OLD and YOUR.")
  (:method (test (my software) (old software) (your software)
            &key &allow-other-keys)
    (let ((*population* (multiple-value-call #'populate
                          (apply #'converge (list my old your)))))
      (evolve test))))

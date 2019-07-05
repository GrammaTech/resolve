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
        :software-evolution-library/software/source
        :software-evolution-library/software/clang
        :software-evolution-library/software/javascript
        :software-evolution-library/software/json
        :software-evolution-library/software/simple
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
           :resolve-to
           :resolve-conflict))
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
     (mapc
      (lambda (ast)
        #+debug (format t "Replacing conflict at ~S~%" (ast-path ast))
        (setf (get-ast conflicted (ast-path ast))
              (aget option (conflict-ast-child-alist ast)))
        #+debug (to-file conflicted (format nil "/tmp/resolve-to-~d.c" counter))
        #+debug (to-file cp (format nil "/tmp/resolve-cp-~d.c" counter))
        #+debug (incf counter)))
     ;; Modify conflict nodes in reverse to work up the tree.
     (reverse (remove-if-not #'conflict-ast-p
                             (asts conflicted))))
    conflicted))

(defgeneric resolve-conflict (conflicted conflict strategy
                              &key fodder &allow-other-keys)
  (:documentation
   "Resolve CONFLICT in CONFLICTED with STRATEGY.
Keyword argument FODDER may be used to provide a source of novel code.
See the empirical study _On the Nature of Merge Conflicts: a Study of
2,731 Open Source Java Projects Hosted by GitHub_ for the source of
the strategies.")
  (:method ((conflicted software) (conflict ast) (strategy symbol)
            &key (fodder (resolve-to (copy conflicted) :old)) &allow-other-keys)
    (nest
     (flet ((literal-replace (obj conflict replacement)
              (replace-ast obj (ast-path conflict) replacement :literal t))))
     (setf conflicted)
     (literal-replace conflicted conflict)
     (let ((options (conflict-ast-child-alist conflict))))
     (flet ((generate-novel-code ()
              (repeatedly (random (+ (length (aget :my options))
                                     (length (aget :your options))))
                (pick-good fodder)))))
     ;; Six ways of resolving a conflict:
     (case strategy
       ;; 1. (V1) version 1
       (:V1 (aget :my options))
       ;; 2. (V2) version 2
       (:V2 (aget :your options))
       ;; 3. (CC) concatenate versions (either order)
       (:C1 (append (aget :my options) (aget :your options)))
       (:C2 (append (aget :your options) (aget :my options)))
       ;; 4. (CB) interleaving subset of versions
       (:CB (shuffle (append (aget :my options) (aget :your options))))
       ;; 5. (NC) mix interleaving subset with novel code
       (:NC (shuffle (append (generate-novel-code)
                             (aget :my options) (aget :your options))))
       ;; 6. (NN) select the base version
       (:NN (aget :old options))))))


;;; Actual population and evolution of resolution.
(defgeneric populate (conflicted &key strategies &allow-other-keys)
  (:documentation "Build a population from MERGED and UNSTABLE chunks.
NOTE: this is exponential in the number of conflict ASTs in CONFLICTED.")
  (:method ((conflicted software)
            &key (strategies `(:V1 :V2 :C1 :C2 :CB :NC :NN))
            &aux (pop (list (copy conflicted))))
    ;; Initially population is just a list of the base object.
    (let ((chunks (remove-if-not #'conflict-ast-p (asts conflicted))))
      (assert chunks (chunks) "Software ~S must have conflict ASTs" conflicted)
      ;; Warn if we're about to do something really expensive.
      (when (> (expt (length strategies) (length chunks))
               (or *max-population-size* (expt 2 10)))
        (warn "About to generate ~d possible resolutions from ~d chunks"
              (expt (length strategies) (length chunks)) (length chunks)))
      (mapc (lambda (chunk)
              (setf pop
                    (mappend
                     (lambda (variant)
                       (mapcar
                        (lambda (strategy)
                          (resolve-conflict (copy variant) chunk strategy))
                        strategies))
                     pop)))
            (reverse chunks)))
    pop))

(defgeneric resolve (my old your test &rest rest &key target &allow-other-keys)
  (:documentation
   "Resolve merge conflicts between software versions MY OLD and YOUR.
Keyword argument TARGET specifies the target fitness.
Extra keys are passed through to EVOLVE.")
  (:method (test (my software) (old software) (your software)
            &rest rest &key (target nil target-supplied-p) &allow-other-keys)
    (let ((*target-fitness-p* (when target-supplied-p
                                {= target}
                                (constantly nil))))
      (note 2 "Populate")
      (setf *population* (populate (converge my old your :conflict t)))
      (setf *fitness-evals* 0)
      (note 2 "Evaluate ~d population members" (length *population*))
      (mapc (lambda (variant)
              (incf *fitness-evals*)
              (evaluate test variant)
              (when (funcall *target-fitness-p* (fitness variant))
                (note 2 "Resolution found")
                (return-from resolve variant)))
            *population*)
      (note 2 "Best fitness ~d" (extremum (mapcar #'fitness *population*) #'>))
      (note 2 "Evolve conflict resolution")
      (eval `(evolve test ,@rest)))))

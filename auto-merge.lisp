;;;; auto-merge.lisp -- Main automatic merge command-line driver
(defpackage :resolve/auto-merge
  (:documentation "Main resolve command-line driver")
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :bordeaux-threads
        :uiop
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/components/lexicase
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
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
  (:method ((conflicted parseable) option &aux #+debug (cp (copy conflicted)))
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

(defgeneric resolve-conflict (conflicted conflict strategy)
  (:documentation
   "Resolve CONFLICT in CONFLICTED with STRATEGY.
Keyword argument FODDER may be used to provide a source of novel code.
See the empirical study _On the Nature of Merge Conflicts: a Study of
2,731 Open Source Java Projects Hosted by GitHub_ for the source of
the strategies.")
  (:method ((conflicted parseable) (conflict ast) (strategy symbol)
            &aux (options (conflict-ast-child-alist conflict)))
    (replace-ast conflicted
                 (ast-path conflict)
                 ;; Five ways of resolving a conflict:
                 (case strategy
                   ;; 1. (V1) version 1
                   (:V1 (aget :my options))
                   ;; 2. (V2) version 2
                   (:V2 (aget :your options))
                   ;; 3. (CC) concatenate versions (either order)
                   (:C1 (append (aget :my options) (aget :your options)))
                   (:C2 (append (aget :your options) (aget :my options)))
                   ;; 4. (NN) select the base version
                   (:NN (aget :old options)))
                 :literal t)))


;;; Generation of the initial population.
(defgeneric populate (conflicted &key strategies &allow-other-keys)
  (:documentation "Build a population from MERGED and UNSTABLE chunks.
NOTE: this is exponential in the number of conflict ASTs in CONFLICTED.")
  (:method ((conflicted parseable)
            &key (strategies `(:V1 :V2 :C1 :C2 :NN))
            &aux (pop (list (copy conflicted)))
              (pop-size (or *max-population-size* (expt 2 10))))
    ;; Initially population is just a list of the base object.
    (let ((chunks (remove-if-not #'conflict-ast-p (asts conflicted))))
      (assert chunks (chunks) "Software ~S must have conflict ASTs" conflicted)
      ;; Create all conflict resolution variants if the total number
      ;; is less than POP-SIZE.  Otherwise, create a random sample.
      (if (< (expt (length strategies) (length chunks)) pop-size)
          (mapc (lambda (chunk)
                  (setf pop
                        (mappend
                         (lambda (variant)
                           (mapcar
                            (lambda (strategy)
                              (resolve-conflict (copy variant) chunk strategy))
                            strategies))
                         pop)))
                (reverse chunks))
          (progn
            (warn "Randomly sampling ~d possible resolutions from ~
                   ~d possibilities."
                  pop-size (expt (length strategies) (length chunks)))
            (setf pop
                  (iter (for i below pop-size)
                        (collect
                          (reduce (lambda (variant chunk)
                                    (resolve-conflict (copy variant) chunk
                                                      (random-elt strategies)))
                                  (reverse chunks)
                                  :initial-value (copy conflicted))))))))
    pop)
  (:method ((conflicted parseable-project) &rest rest
            &aux (pop-size (or *max-population-size* (expt 2 10))))
    (iter (for (file . obj) in (evolve-files conflicted))
          (collect (if (some #'conflict-ast-p (asts obj))
                       (mapcar {cons file} (apply #'populate obj rest))
                       (list (cons file (copy obj)))) into resolutions)
          (finally
            ;; Return all conflict resolution variants if the total number
            ;; is less than POP-SIZE.  Otherwise, return a random sample.
            (if (< (reduce #'* resolutions :key #'length) pop-size)
                (return (mapcar (lambda (resolution)
                                  (copy conflicted :evolve-files resolution))
                                (cartesian resolutions)))
                (return (iter (for i below pop-size)
                              (collect (copy conflicted :evolve-files
                                             (mapcar (lambda (objs)
                                                       (random-elt objs))
                                                     resolutions))))))))))


;;; Evolution of a merge resolution.
(defgeneric resolve (my old your test &rest rest
                     &key evolve? target num-threads
                       strings base-cost wrap max-wrap-diff &allow-other-keys)
  (:documentation
   "Resolve merge conflicts between software versions MY OLD and YOUR.
Keyword argument EVOLVE? is a boolean specifying whether to attempt evolution
Keyword argument MAX-EVALS specifies maximum number of evals run in evolution
Keyword argument MAX-TIME specifies maximum number of seconds to run evolution
Keyword argument TARGET specifies the target fitness.
Keyword argument NUM-THREADS specifies the number of threads to utilize
Keyword argument STRINGS specifies if the diff should descend into strings
Keyword argument BASE COST specifies the basic cost of a diff
Keyword argument WRAP specifies if wrap/unwrap actions should appear in diffs
Keyword argument MAX-WRAP-DIFF specifies the max size difference for wrap/unwrap
Extra keys are passed through to EVOLVE.")
  (:method ((my software) (old software) (your software) test
            &key (evolve? nil) (max-evals nil) (max-time nil)
              (target nil target-supplied-p) (num-threads 1)
              ((:strings *strings*) *strings*)
              ((:base-cost *base-cost*) *base-cost*)
              ((:wrap *wrap*) *wrap*)
              ((:max-wrap-diff *max-wrap-diff*) *max-wrap-diff*))
    (note 2 "Populate candidate merge resolutions.")

    (let ((*population* (populate (converge my old your :conflict t)))
          (*target-fitness-p* (if target-supplied-p
                                  [{equalp target} #'fitness]
                                  «and #'fitness [{every #'zerop} #'fitness]»))
          (*worst-fitness-p* [{every {equalp most-positive-fixnum}} #'fitness])
          (*fitness-evals* 0)
          (*fitness-predicate* #'<))

      ;; Evaluate the fitness of the initial population
      (note 2 "Evaluate ~d population members." (length *population*))
      (task-map num-threads
                (lambda (variant)
                  (unless (some {funcall *target-fitness-p*} *population*)
                    (evaluate test variant)))
                *population*)

      ;; Update global vars after evaluating initial population
      (setf *population* (remove-if [#'null #'fitness] *population*))
      (incf *fitness-evals* (length *population*))

      ;; Determine if a soluation has been found and if so, return.  Otherwise,
      ;; report the best initial fitness.
      (let ((best (extremum *population* #'fitness-better-p :key #'fitness)))
        (if (funcall *target-fitness-p* best)
            (progn
              (note 2 "Merge resolution found.")
              (return-from resolve best))
            (note 2 "Best fitness: ~a." (fitness best))))

      ;; Perform the evolutionary search
      (when evolve?
        (note 2 "Evolve conflict resolution.")
        (generational-evolve #'simple-reproduce
                             {simple-evaluate test}
                             #'lexicase-select
                             :filter [#'not {funcall *worst-fitness-p*}]
                             :max-time max-time
                             :max-evals max-evals))

      ;; Return the best variant
      (extremum *population* #'fitness-better-p :key #'fitness))))

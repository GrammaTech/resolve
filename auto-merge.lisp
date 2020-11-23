;;;; auto-merge.lisp -- Main automatic merge command-line driver
(defpackage :resolve/auto-merge
  (:documentation "Main resolve command-line driver")
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/task
        :software-evolution-library/utility/debug
        :software-evolution-library/command-line
        :software-evolution-library/components/lexicase
        :software-evolution-library/components/test-suite
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/simple
        :resolve/core
        :resolve/ast-diff
        :resolve/alist
        :resolve/software/project
        :resolve/software/auto-mergeable
        :resolve/software/parseable)
  (:import-from :resolve/ast-diff)
  (:import-from :software-evolution-library/components/file
                :file-w-attributes)
  (:export :resolve
           :auto-merge-test
           :populate
           :resolve-to
           :resolve-conflict
           :try-reconcile-conflicts))
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
        #+debug (format t "Replacing conflict at ~S~%" (ast-path conflicted ast))
        (prog1
            (with conflicted ast (aget option (conflict-ast-child-alist ast)))
          #+debug (to-file conflicted (format nil "/tmp/resolve-to-~d.c" counter))
          #+debug (to-file cp (format nil "/tmp/resolve-cp-~d.c" counter))
          #+debug (incf counter))))
     ;; Modify conflict nodes in reverse to work up the tree.
     (reverse (remove-if-not {typep _ 'conflict-ast} (asts conflicted))))
    conflicted))

(defgeneric resolve-conflict (conflicted conflict &key strategy)
  (:documentation "Resolve CONFLICT in CONFLICTED using STRATEGY.")
  (:method ((conflicted auto-mergeable) (conflict conflict-ast)
            &key (strategy (random-elt (get-conflict-strategies conflict))))
    (apply-mutation conflicted
                    (make-instance 'new-conflict-resolution
                      :targets conflict
                      :strategy strategy))))


;;; Generation of the initial population.
(defgeneric populate (conflicted)
  (:documentation "Return a population suitable for evolution from MERGED
and UNSTABLE chunks in CONFLICTED.  The number of software objects
returned is limited by the *MAX-POPULATION-SIZE* global variable.")
  (:method ((conflicted auto-mergeable)
            &aux (pop (list (copy conflicted)))
              (pop-size (or *max-population-size* (expt 2 10))))
    ;; Initially population is just a list of the base object.
    (let* ((chunks (get-conflicts conflicted))
           (num-solutions (reduce #'* chunks
                                  :key [#'length #'get-conflict-strategies])))
      ;; Create all conflict resolution variants if the total number
      ;; is less than POP-SIZE.  Otherwise, create a random sample.
      (if (< num-solutions pop-size)
          (mapc (lambda (chunk)
                  (setf pop
                        (mappend
                         (lambda (variant)
                           (mapcar
                            (lambda (strategy)
                              (resolve-conflict (copy variant) chunk
                                                :strategy strategy))
                            (get-conflict-strategies chunk)))
                         pop)))
                (reverse chunks))
          (setf pop
                (iter (for i below pop-size)
                      (collect
                       (reduce (lambda (variant chunk)
                                 (resolve-conflict (copy variant) chunk))
                               (reverse chunks)
                               :initial-value (copy conflicted)))))))
    pop)
  (:method ((conflicted auto-mergeable-project)
            &aux (pop-size (or *max-population-size* (expt 2 10))))
    (iter (for (file . obj) in (evolve-files conflicted))
          (collect (if (get-conflicts obj)
                       (mapcar {cons file} (populate obj))
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


;;; Fitness testing
(defmethod auto-merge-test ((obj software) (tests test-suite))
  "Determine the fitness of OBJ against TESTS."
  (with-temporary-file (:pathname bin)
    (if (ignore-phenome-errors (phenome obj :bin bin))
        (mapcar (lambda (test-case)
                  (nth-value 2 (run-test bin test-case)))
                (test-cases tests))
        (make-list (length (test-cases tests))
                   :initial-element most-positive-fixnum))))

(defmethod auto-merge-test :around ((obj clang-project) (tests test-suite))
  "Setup environment so the fitness of OBJ can be evaluated against TESTS."
  ;; Bind *build-dir* so multiple builds can occur in a multi-threaded
  ;; environment.
  (with-temporary-directory (:pathname dir)
    (let ((*build-dir* dir))
      (call-next-method))))


;;; Evolution of a merge resolution.

(defun map-project-files (fn project)
  (flet ((map-files (files)
           (mapcar (lambda (file)
                     (cons (car file)
                           (mapcar fn (cdr file))))
                   files)))
    (copy project
          :evolve-files (map-files (evolve-files project))
          :other-files (map-files (other-files project)))))

(defun try-reconcile-conflicts (sw &aux reconciliations)
  "Attempt to reconcile conflict nodes in SW assuming they represent conflicting branches of a Git repository."
  (labels ((get-reconciliations-by-intent (node)
             (typecase node
               (auto-mergeable-project
                (map-project-files #'get-reconciliations-by-intent node))
               (auto-mergeable
                (copy node
                      :genome
                      (mapcar #'get-reconciliations-by-intent
                              (genome node))))
               (conflict-ast
                ;; Note that the child-alist slot of conflict-asts is
                ;; no longer really an alist: it used to be that the
                ;; cdr was a list of children, but now the cdr is
                ;; always a list of a single element.
                (match (conflict-ast-child-alist node)
                  ((alist (:my . (list my))
                          (:your . (list your))
                          (:old . (list old)))
                   (cond ((equal? my your)
                          ;; If both branches make the same change, we
                          ;; should keep it.
                          (push (cons node :v1) reconciliations))
                         ;; If one branch has changed and the other
                         ;; has not, we should keep the change.
                         ((equal? old my)
                          (push (cons node :v2) reconciliations))
                         ((equal? old your)
                          (push (cons node :v1) reconciliations))
                         (t node)))
                  ((alist (:my . (list my))
                          (:your . (list your)))
                   ;; If both branches have made the same addition we
                   ;; should keep it.
                   (cond ((equal? my your)
                          (push (cons node :v1) reconciliations))
                         (t node)))
                  (otherwise node)))
               (otherwise node))))
    (etypecase sw
      (auto-mergeable-project
       (copy sw
             :evolve-files
             (iter (for (file . obj) in (evolve-files sw))
                   (collect (cons file (try-reconcile-conflicts obj))))))
      (auto-mergeable
       (get-reconciliations-by-intent sw)
       (reduce (lambda (variant node.strategy)
                 (resolve-conflict (copy variant)
                                   (car node.strategy)
                                   :strategy (cdr node.strategy)))
               reconciliations
               :initial-value sw)))))

(defgeneric resolve (my old your test &rest rest
                     &key evolve? target num-threads
                       strings base-cost wrap wrap-sequences max-wrap-diff &allow-other-keys)
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
Keyword argument WRAP-SEQUENCES specifies if sequence wrap/unwrap actions should appear in diffs
Keyword argument MAX-WRAP-DIFF specifies the max size difference for wrap/unwrap
Extra keys are passed through to EVOLVE.")
  (:method ((my software) (old software) (your software) test
            &key (evolve? nil) (max-evals nil) (max-time nil)
              (target nil target-supplied-p) (num-threads 1)
              ((:strings *strings*) *strings*)
              ((:base-cost *base-cost*) *base-cost*)
              ((:wrap *wrap*) *wrap*)
              ((:wrap-sequences *wrap-sequences*) *wrap-sequences*)
              ((:max-wrap-diff *max-wrap-diff*) *max-wrap-diff*))
    (note 2 "Populate candidate merge resolutions.")
    (let* ((initial (converge my old your :conflict t))
           (converged (try-reconcile-conflicts initial))
           *population*
           ;; Because each variant is tested against a test suite,
           ;; where the test script may be repeated over an index, the
           ;; return value of `fitness' is not a number but a list of
           ;; numbers. A list of zeros means all the tests have passed
           ;; (exited 0).
           (*target-fitness-p* (if target-supplied-p
                                   [{equalp target} #'fitness]
                                   «and #'fitness [{every #'zerop} #'fitness]»))
           (failed? «and #'fitness [{every #'plusp} #'fitness]»)
           (*worst-fitness-p* [{every {equalp most-positive-fixnum}} #'fitness])
           (*fitness-evals* 0)
           (*fitness-predicate* #'<))

      (note 2 "AST merging eliminated ~d/~d conflict~:p"
            (- (length (get-conflicts initial))
               (length (get-conflicts converged)))
            (length (get-conflicts initial)))

      ;; Special case - there are no conflicts to be resolved
      (unless (get-conflicts converged)
        (note 2 "No conflicts found.")
        (return-from resolve converged))

      (setf *population* (populate converged))

      ;; Evaluate the fitness of the initial population
      (note 2 "Number of conflicts found: ~d."
            (length (get-conflicts converged)))
      (note 2 "Evaluate ~d population members."
              (length *population*))
      (task-map num-threads
                (lambda (variant)
                  (unless (some {funcall *target-fitness-p*} *population*)
                    (evaluate test variant)))
                *population*)

      (note 2 "Evaluated ~a variant~:p" (count-if #'fitness *population*))

      ;; Update global vars after evaluating initial population. If we
      ;; hit the target fitness early there may be variants that were
      ;; never evaluated (whose fitness is null). These must be
      ;; removed.
      (setf *population* (remove-if [#'null #'fitness] *population*))
      (incf *fitness-evals* (length *population*))

      ;; Determine if a solution has been found and if so, return.  Otherwise,
      ;; report the best initial fitness.
      (let ((best (extremum *population* #'fitness-better-p :key #'fitness)))
        (if (funcall *target-fitness-p* best)
            (progn
              (note 2 "Merge resolution found.")
              (return-from resolve best))
            (note 2 "Best fitness: ~a." (fitness best))))
      (when (and (not evolve?)
                 (every failed? *population*))
        (error "No variant has survived selection."))

      ;; Perform the evolutionary search if one was requested
      (labels ((best ()
                 (extremum *population* #'fitness-better-p :key #'fitness))
               (periodic ()
                 (note 2 "~16a ~16a ~a"
                       *generations* *fitness-evals* (fitness (best)))))
        (when evolve?
          (handler-bind
              ((no-mutation-targets
                (lambda (c)
                  (declare (ignorable c))
                  (invoke-restart 'try-another-mutation)))
               (mutate
                (lambda (c)
                  (declare (ignorable c))
                  (invoke-restart 'try-another-mutation))))
            (note 2 "Evolve conflict resolution.")
            (note 2 "~16a ~16a ~a"
                  "Generations" "Evaluations" "Best-fitness")
            (generational-evolve
             #'simple-reproduce
             {simple-evaluate test}
             #'lexicase-select
             :max-time max-time
             :max-evals max-evals
             :period 1
             :period-fn #'periodic)))

        ;; Print notes
        (if (funcall *target-fitness-p* (best))
            (progn
              (periodic)
              (note 2 "Merge resolution found."))
            (note 2 "Evolution complete, returning best variant."))

        ;; Return the best variant
        (best)))))

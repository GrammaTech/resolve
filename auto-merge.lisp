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
        :software-evolution-library/software/simple
        :resolve/core
        :resolve/ast-diff
        :resolve/alist
        :resolve/software/project
        :resolve/software/auto-mergeable
        :resolve/software/parseable)
  (:import-from :resolve/ast-diff
                :standardized-children
                :copy-with-standardized-children)
  (:shadow :function-body)
  (:import-from :software-evolution-library/components/file
                :file-w-attributes)
  (:import-from :software-evolution-library/software/parseable
                :skip-mutation)
  (:import-from :software-evolution-library/software/tree-sitter
                :rule-matching-error)
  (:export :resolve
           :auto-merge-test
           :populate
           :resolve-to
           :resolve-conflict
           :try-reconcile-conflicts
           :*auto-merge-meta*))
(in-package :resolve/auto-merge)
(in-readtable :curry-compose-reader-macros)


;;; Utility functions
(defgeneric resolve-to (conflicted option &key)
  (:documentation "Resolve every conflict in CONFLICTED to OPTION.")
  (:method ((conflicted parseable) option &key root
            ;; TODO: counter will need to be special and passed in as a keyword
            ;;       arg for it to properly name files.
            &aux #+debug (counter 0))
    (labels ((replacing-splice-at (position new-sequence sequence)
               "Replace POSITION in SEQUENCE with the items in NEW-SEQUENCE."
               (iter
                 (for traversal-sequence first sequence
                      then (cdr traversal-sequence))
                 (for i upfrom 0)
                 (until (= i position))
                 (collect (car traversal-sequence) into initial-split)
                 (finally
                  (return
                    (append
                     initial-split new-sequence (cdr traversal-sequence))))))
             (splice-with (conflicted ast)
               "Replace the conflict node AST with the revelant resolution."
               (let* ((path (ast-path conflicted ast))
                      (parent (lookup conflicted (butlast path)))
                      (last-path (lastcar path))
                      (slot (car last-path))
                      (new-values (aget option (conflict-ast-child-alist ast)))
                      (new-slot-value
                        (replacing-splice-at
                         (cdr last-path)
                         new-values
                         (slot-value parent slot)))
                      (replacement (copy parent
                                         (make-keyword slot) new-slot-value
                                         :stored-hash nil)))
                 ;; NOTE: this handles nested conflict ASTs which aren't
                 ;;       handled by collect-if.
                 (reduce (lambda (conflicted root)
                           (resolve-to conflicted option :root root))
                         new-values
                         :initial-value (with conflicted parent replacement)))))
      #+debug (to-file conflicted (format nil "/tmp/resolve-original.c"))
      ;; Modify the parent of all conflict nodes to replace with OPTION.
      (reduce
       (lambda (conflicted ast)
         #+debug (format t "Replacing conflict at ~S~%" (ast-path conflicted ast))
         (let ((new-conflicted (splice-with conflicted ast)))
           #+debug (to-file new-conflicted (format nil "/tmp/resolve-to-~d.c" counter))
           #+debug (to-file conflicted (format nil "/tmp/resolve-cp-~d.c" counter))
           #+debug (incf counter)
           new-conflicted))
       ;; Modify conflict nodes in reverse to work up the tree.
       (reverse (collect-if (of-type 'conflict-ast) (or root (genome conflicted))))
       :initial-value conflicted))))

(defgeneric printable? (software)
  (:method (software) t)
  (:method ((software auto-mergeable-parseable))
    (values
     (ignore-errors
      (source-text (genome software) :stream (make-broadcast-stream))
      t)))
  (:method ((project auto-mergeable-project))
    (every #'printable? (evolve-files project))))

(defun check-printable (software)
  (unless (printable? software)
    (error "Cannot be printed: ~a" software)))

(defun check-remove-ast-stubs (software)
  (let ((software (remove-ast-stubs software)))
    (if (genome software) software
        (error "AST stub at root: ~a" software))))

(defgeneric resolve-conflict (conflicted conflict &key strategy)
  (:documentation "Resolve CONFLICT in CONFLICTED using STRATEGY.")
  (:method-combination standard/context)
  (:method :context ((conflicted auto-mergeable)
                     (conflict conflict-ast)
                     &key strategy)
    "Catch rule matching errors."
    (declare (ignore strategy))
    (handler-bind
        ((rule-matching-error
           (lambda (c)
             (declare (ignorable c))
             (note 3 "Skipping mutation due to ~a" c)
             (maybe-invoke-restart 'skip-mutation)
             (return-from resolve-conflict nil))))
      (call-next-method)))
  (:method :around ((conflicted auto-mergeable-parseable)
                    (conflict conflict-ast)
                    &key strategy)
    "Check that the result is printable before returning it."
    (declare (ignore strategy))
    (lret ((result (call-next-method)))
      (when result
        (check-printable result)
        (remove-ast-stubs result))))
  (:method ((conflicted auto-mergeable) (conflict conflict-ast)
            &key (strategy (random-elt (get-conflict-strategies conflict))))
    (apply-mutation conflicted
                    (make-instance 'new-conflict-resolution
                                   :targets conflict
                                   :strategy strategy))))

(defgeneric remove-ast-stubs (variant)
  (:documentation "Remove AST stubs from the genome of VARIANT.

AST stubs must be removed lest their presence cause extra commas or
other delimiters to be inserted.")
  (:method ((variant software))
    variant)
  (:method ((variant parseable))
    (let ((genome (genome variant)))
      (copy variant
            :genome (if (typep genome 'ast-stub)
                        genome
                        (remove-ast-stubs (genome variant))))))
  (:method ((genome ast))
    (let ((ast-stubs (collect-if (of-type 'ast-stub) genome)))
      (reduce (lambda (genome stub)
                (ematch (children stub)
                  ((list)
                   (less genome stub))
                  ((list child)
                   (with genome stub child))))
              ast-stubs
              :initial-value genome))))


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
                        (remove
                         nil
                         (mappend
                          (lambda (variant)
                            (mapcar
                             (lambda (strategy)
                               (resolve-conflict (copy variant) chunk
                                                 :strategy strategy))
                             (get-conflict-strategies chunk)))
                          pop))))
                (reverse chunks))
          (nest
           (setf pop)
           (iter (for i below pop-size))
           (collect)
           (reduce (lambda (variant chunk)
                     ;; Try all the available strategies in random
                     ;; order until you get a valid one.
                     (or (some
                          (lambda (strategy)
                            (resolve-conflict (copy variant) chunk
                                              :strategy strategy))
                          (reshuffle
                           (get-conflict-strategies chunk)))
                         (error "Cannot resolve ~a" chunk)))
                   (reverse chunks)
                   :initial-value (copy conflicted)))))
    (lret ((pop (mapcar #'remove-ast-stubs pop)))
      (assert (notany #'get-conflicts pop))
      (assert (every #'printable? pop))
      (when (emptyp pop)
        (error "Could not generate a population"))))
  (:method ((conflicted auto-mergeable-project)
            &aux (pop-size (or *max-population-size* (expt 2 10))))
    (iter (for (file . obj) in (evolve-files conflicted))
          (collect (if (get-conflicts obj)
                       (mapcar {cons file} (populate obj))
                       (list (cons file (copy obj))))
            into resolutions)
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
  "Attempt to reconcile conflict nodes in SW assuming they represent conflicting
branches of a Git repository."
  (labels ((propagate-alist-keys (alist)
             "Propagate the keys in ALIST to each item in
              their associated value."
             ;; ((:key val1 val2 ...) ...)
             ;;  ->
             ;; (((:key val1) (:key val2) ...) ...)
             (iter (for association in alist)
               (collect (mapcar {list (car association)}
                                (cdr association)))))
           (expand-conflict-asts (children)
             "Expand all conflict-asts in CHILDREN such that no conflict-ast
              represents more than one conflict each."
             (symbol-macrolet ((child-alist (conflict-ast-child-alist child)))
               (iter
                 (for child in children)
                 (appending
                  (cond
                    ((typep child '(not conflict-ast)) (list child))
                    ;; If there is only one item in the :my list,
                    ;; the conflict AST hasn't been merged.
                    ((= 1 (length (aget :my child-alist)))
                     (list child))
                    (t
                     (apply
                      #'mapcar
                      (op (make-instance 'conflict-ast :child-alist (list _*)))
                      (propagate-alist-keys child-alist))))))))
           (expand-merged-conflict-asts (node)
             "Expand all merged conflict-asts in NODE such that they only
              represent one conflict each."
             (typecase node
               (ast
                (let* ((standardized-children (standardized-children node))
                       (expanded-children
                         (mapcar #'expand-merged-conflict-asts
                                 (expand-conflict-asts standardized-children))))
                  (copy-with-standardized-children node expanded-children)))
               (t node)))
           (handle-3-way-conflict (node my your old)
             "Handle a 3-way conflict between MY, YOUR, and OLD at NODE."
             ;; TODO: don't use node and create a new one instead?
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
                   ;; TODO: is this needed?
                   (t node)))
           (handle-2-way-conflict (node my your)
             "Handle a 2-way conflict between MY and YOUR at NODE."
             ;; If both branches have made the same addition we
             ;; should keep it.
             (cond ((equal? my your)
                    (push (cons node :v1) reconciliations))
                   ;; TODO: is this needed?
                   (t node)))
           (get-conflict-reconciliations (node)
             (match (conflict-ast-child-alist node)
               ((alist (:my . my)
                       (:your . your)
                       (:old . old))
                (mapcar {handle-3-way-conflict node} my your old))
               ((alist (:my . my)
                       (:your . your))
                (mapcar {handle-2-way-conflict node} my your))
               (otherwise node)))
           (get-reconciliations-by-intent (node)
             (typecase node
               (auto-mergeable-project
                (map-project-files #'get-reconciliations-by-intent node))
               (auto-mergeable
                (copy node
                      :genome
                      (mapcar #'get-reconciliations-by-intent
                              (genome node))))
               (conflict-ast
                (get-conflict-reconciliations node))
               (otherwise node)))
           (reconcile-expanded-conflicts (sw)
             (etypecase sw
               (auto-mergeable-project
                (copy sw
                      :evolve-files
                      (iter
                       (for (file . obj) in (evolve-files sw))
                       (collect (cons file (try-reconcile-conflicts obj))))))
               (auto-mergeable
                (let ((expanded-sw
                       (copy sw :genome (expand-merged-conflict-asts (genome sw)))))
                  (get-reconciliations-by-intent expanded-sw)
                  (reduce (lambda (variant node.strategy)
                            (destructuring-bind (node . strategy)
                                node.strategy
                              (note 4 "Resolved by strategy ~a: ~%~a"
                                    strategy
                                    (mapcar (lambda (cons)
                                              (cons (car cons)
                                                    (source-text (cdr cons))))
                                            (conflict-ast-child-alist node)))
                              (resolve-conflict (copy variant) node
                                                :strategy strategy)))
                          reconciliations
                          :initial-value expanded-sw))))))
    (let ((reconciliation (reconcile-expanded-conflicts sw)))
      ;; Use the original if we end up with more conflicts.
      (if (< (length (get-conflicts reconciliation))
             (length (get-conflicts sw)))
          reconciliation
          (progn
            (note 3 "Reconciling made ~a worse, using original." sw)
            (check-printable sw)
            sw)))))

(defvar-unbound *auto-merge-meta*
  "Optional hash table to use to store metadata about the run.")

(defgeneric resolve (my old your test &rest rest
                     &key evolve? target num-threads
                       strings base-cost wrap wrap-sequences max-wrap-diff
                       meta
                     &allow-other-keys)
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
              ((:max-wrap-diff *max-wrap-diff*) *max-wrap-diff*)
              (meta (or (bound-value '*auto-merge-meta*)
                        (make-hash-table))))
    (note 2 "Merge ASTs")
    (let* ((initial (converge my old your :conflict t))
           (converged (try-reconcile-conflicts initial))
           ;; Metadata.
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

      (dict* meta
             :initial-conflict-count (length (get-conflicts initial))
             :converged-conflict-count (length (get-conflicts converged)))
      (note 2 "AST merging eliminated ~d/~d conflict~:p"
            (- (gethash :initial-conflict-count meta)
               (gethash :converged-conflict-count meta))
            (gethash :initial-conflict-count meta))

      ;; Special case - there are no conflicts to be resolved
      (unless (get-conflicts converged)
        (note 2 "No conflicts found.")
        (return-from resolve converged))

      (note 2 "Populate candidate merge resolutions.")
      (setf *population* (populate converged))

      ;; Evaluate the fitness of the initial population
      (note 2 "Number of conflicts found: ~d."
            (gethash :converged-conflict-count meta))
      (setf (gethash :popcount meta) (length *population*))
      (note 2 "Evaluate ~d population members."
            (gethash :popcount meta))
      (task-map num-threads
                (lambda (variant)
                  (unless (some {funcall *target-fitness-p*} *population*)
                    (evaluate test variant)))
                *population*)

      (setf (gethash :evalcount meta) (count-if #'fitness *population*))
      (note 2 "Evaluated ~a variant~:p" (gethash :evalcount meta))

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
            (progn
              (setf (gethash :best-fitness meta) (fitness best))
              (note 2 "Best fitness: ~a." (gethash :best-fitness meta)))))
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

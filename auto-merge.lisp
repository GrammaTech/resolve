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
        :software-evolution-library/software/source
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/simple
        :resolve/core
        :resolve/ast-diff
        :resolve/alist
        :resolve/software/project
        :resolve/software/auto-mergeable
        :resolve/software/parseable)
  (:export :resolve
           :auto-merge-test
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

(defgeneric get-conflict-strategies (ast)
  (:documentation "Return a list of strategies which may be utilized
to resolve the conflict AST.")
  (:method ((ast conflict-ast))
    (if (aget :top-level (ast-aux-data ast))
        '(:V1 :NN)
        '(:V1 :V2 :C1 :C2 :NN))))

(defgeneric resolve-conflict-ast (conflict &key strategy)
  (:documentation "Return a concrete resolution of CONFLICT AST
using STRATEGY.")
  (:method ((conflict ast)
            &key (strategy (random-elt (get-conflict-strategies conflict)))
            &aux (options (conflict-ast-child-alist conflict)))
    (labels ((normalize (children)
               "Normalize CHILDREN by adding the conflict AST
               to each child AST's aux-data.  If there are no children,
               create a NullStmt AST with this aux-data.  The aux-data
               is required for the `new-conflict-resolution` mutation."
               (if children
                   (mapcar (lambda (child)
                             (if (ast-p child)
                                 (copy child :aux-data
                                             `((:conflict-ast . ,conflict)))
                                 (make-raw-ast :children (list child)
                                               :aux-data
                                               `((:conflict-ast . ,conflict)))))
                           children)
                   (list (make-raw-ast :aux-data
                                       `((:conflict-ast . ,conflict)))))))
      ;; Five ways of resolving a conflict:
      (case strategy
        ;; 1. (V1) version 1
        (:V1 (normalize (aget :my options)))
        ;; 2. (V2) version 2
        (:V2 (normalize (aget :your options)))
        ;; 3. (CC) concatenate versions (either order)
        (:C1 (normalize (append (aget :my options) (aget :your options))))
        (:C2 (normalize (append (aget :your options) (aget :my options))))
        ;; 4. (NN) select the base version
        (:NN (normalize (aget :old options)))))))

(defgeneric resolve-conflict (conflicted conflict &key strategy)
  (:documentation "Resolve CONFLICT in CONFLICTED using STRATEGY.")
  (:method ((conflicted auto-mergeable) (conflict conflict-ast)
            &key (strategy (random-elt (get-conflict-strategies conflict))))
    (apply-mutation conflicted
                    (make-instance 'new-conflict-resolution
                      :targets conflict
                      :strategy strategy))))


;;; Mutations for the auto-merge evolutionary loop
(define-mutation new-conflict-resolution (parseable-mutation)
  ((targeter :initform #'find-resolved-conflict)
   (strategy :initarg :strategy :reader strategy :initform nil))
  (:documentation "Replace a conflict AST resolution with an alternative
option."))

(defgeneric find-resolved-conflict (obj)
  (:documentation "Return a conflict previously resolved in OBJ.")
  (:method ((obj auto-mergeable))
    (if-let ((conflicts (nest (remove-duplicates)
                              (mapcar [{aget :conflict-ast} #'ast-aux-data])
                              (get-resolved-conflicts obj))))
      (random-elt conflicts)
      (error (make-condition 'no-mutation-targets
                             :obj obj :text "No resolved conflict asts to pick from.")))))

(defmethod build-op ((mutation new-conflict-resolution)
                     (software auto-mergeable-parseable))
  "Return a list of parseable mutation operations to replace an existing
conflict AST resolution with an alternative option."
  (let* ((conflict-ast (targets mutation))
         (strategy (or (strategy mutation)
                       (random-elt (get-conflict-strategies conflict-ast))))
         (prior-resolution (or (remove-if-not [{eq conflict-ast}
                                               {aget :conflict-ast}
                                               #'ast-aux-data]
                                              (ast-to-list (ast-root software)))
                               (list conflict-ast)))
         (new-resolution (resolve-conflict-ast conflict-ast
                                               :strategy strategy))
         (conflict-path (ast-path (car prior-resolution)))
         (parent (get-ast software (butlast conflict-path))))
    ;; Replace the prior resolution children with the new resolution
    (if (null conflict-path)
        `((:set (:stmt1 . ,conflict-path)
                (:literal1 . ,(car new-resolution))))
        `((:set (:stmt1 . ,parent)
                (:literal1 .
                           ,(copy parent :children
                                  (append (subseq (ast-children parent)
                                                  0
                                                  (lastcar conflict-path))
                                          new-resolution
                                          (subseq (ast-children parent)
                                                  (+ (lastcar conflict-path)
                                                     (length prior-resolution)))))))))))

(defmethod apply-mutation ((software auto-mergeable-simple)
                           (mutation new-conflict-resolution))
  "Apply the NEW-CONFLICT-RESOLUTION mutation to SOFTWARE."
  ;; Simple software objects are a more primitive representation and therefore,
  ;; we need to override `apply-mutation` instead of `build-op`.
  (let* ((conflict-ast (targets mutation))
         (strategy (or (strategy mutation)
                       (random-elt (get-conflict-strategies conflict-ast))))
         (prior-resolution (or (remove-if-not «and [#'ast-p {aget :code}]
                                                   [{eq conflict-ast}
                                                    {aget :conflict-ast}
                                                    #'ast-aux-data
                                                    {aget :code}]»
                                              (genome software))
                               `(((:code . ,conflict-ast)))))
         (new-resolution (mapcar [#'list {cons :code}]
                                 (resolve-conflict-ast conflict-ast
                                                       :strategy strategy)))
         (index (search prior-resolution (genome software) :test #'equalp)))
    (setf (genome software)
          (append (subseq (genome software) 0 index)
                  new-resolution
                  (subseq (genome software)
                          (+ index (length prior-resolution)))))
    software))


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

    (let ((*population* (populate (converge my old your :conflict t)))
          (*target-fitness-p* (if target-supplied-p
                                  [{equalp target} #'fitness]
                                  «and #'fitness [{every #'zerop} #'fitness]»))
          (*worst-fitness-p* [{every {equalp most-positive-fixnum}} #'fitness])
          (*fitness-evals* 0)
          (*fitness-predicate* #'<)
          (*parseable-mutation-types* '((new-conflict-resolution . 1)))
          (*simple-mutation-types* '((new-conflict-resolution . 1))))

      (if (null (remove-if-not #'get-resolved-conflicts *population*))
          ;; Special case - there are no conflicts to be resolved
          (progn
            (note 2 "No conflicts found.")
            (return-from resolve (first *population*)))
          ;; Normal case - print the number of conflicts
          (note 2 "Number of conflicts found: ~d."
                (length (get-resolved-conflicts (first *population*)))))

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

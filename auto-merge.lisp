;;;; auto-merge.lisp -- Main automatic merge command-line driver
(defpackage :resolve/auto-merge
  (:documentation "Main resolve command-line driver")
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :bordeaux-threads
        :metabang-bind
        :uiop
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/components/lexicase
        :software-evolution-library/components/test-suite
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

(defgeneric resolve-conflict-ast (strategy conflict)
  (:documentation "Return a concrete resolution of CONFLICT AST
using STRATEGY.")
  (:method ((strategy symbol) (conflict ast)
            &aux (options (conflict-ast-child-alist conflict)))
    (labels ((normalize (children)
               "Normalize CHILDREN by adding the conflict AST
               to each child AST's aux-data.  If there are no children,
               create a NullStmt AST with this aux-data.  The aux-data
               is required for the `new-conflict-resolution` mutation."
               (if children
                   (mapcar (lambda (child)
                             (if (ast-p child)
                                 (copy child :aux-data `((:conflict-ast .
                                                                        ,conflict)))
                                 (make-raw-ast :children (list child)
                                               :aux-data `((:conflict-ast .
                                                                          ,conflict)))))
                           children)
                   (list (make-raw-ast :aux-data `((:conflict-ast .
                                                                  ,conflict)))))))
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

(defgeneric resolve-conflict (conflicted conflict strategy)
  (:documentation
   "Resolve CONFLICT in CONFLICTED with STRATEGY.
Keyword argument FODDER may be used to provide a source of novel code.
See the empirical study _On the Nature of Merge Conflicts: a Study of
2,731 Open Source Java Projects Hosted by GitHub_ for the source of
the strategies.")
  (:method ((conflicted parseable) (conflict ast) (strategy symbol))
    (replace-ast conflicted
                 (ast-path conflict)
                 (resolve-conflict-ast strategy conflict)
                 :literal t)))


;;; Mutations for the auto-merge evolutionary loop
(define-mutation new-conflict-resolution (parseable-mutation)
  ((targeter :initform #'find-resolved-conflict-ast))
  (:documentation "Replace a conflict AST resolution with an alternative
option."))

(defun find-resolved-conflict-ast (obj)
  "Return a conflict AST previously resolved in OBJ."
  (if-let ((conflict-asts (nest (remove-duplicates)
                                (remove-if #'null)
                                (mapcar [{aget :conflict-ast} #'ast-aux-data])
                                (asts obj))))
    (random-elt conflict-asts)
    (error (make-condition 'no-mutation-targets
             :obj obj :text "No resolved conflict asts to pick from"))))

(defmethod build-op ((mutation new-conflict-resolution) (software t)
                     &aux (strategies '(:V1 :V2 :C1 :C2 :NN)))
  (let* ((conflict-ast (targets mutation))
         (prior-resolution (remove-if-not [{eq conflict-ast}
                                           {aget :conflict-ast}
                                           #'ast-aux-data] (asts software)))
         (new-resolution (resolve-conflict-ast (random-elt strategies)
                                               conflict-ast))
         (conflict-path (ast-path (car prior-resolution)))
         (parent (get-ast software (butlast conflict-path))))
    ;; Replace the prior resolution children with the new resolution
    `((:set (:stmt1 . ,parent)
            (:literal1 .
             ,(copy parent
                    :children
                    (append (subseq (ast-children parent)
                                    0
                                    (lastcar conflict-path))
                            new-resolution
                            (subseq (ast-children parent)
                                    (+ (lastcar conflict-path)
                                       (length prior-resolution))))))))))


;;; Auto-merge crossover implementation.
(defmethod crossover ((a parseable) (b parseable))
  "Crossover two parseable software objects.  This implementation
performs a 2-pt, homologous crossover of 'top-level' ASTs in A and B.

As an example, consider the following software objects:

   A       B
-------+--------
foo()  | foo()
bar()  | bar()
my1 () | yours1()
baz()  | baz()
my2 () | yours2()
bag()  | bag()

A 2-pt homologous crossover in this situation may be the following:
foo()       <- From A (1)
bar()       <- From A (1)
my1()       <- From A (1)
baz()       <- From A (1)
yours2()    <- From B (2)
bag()       <- From A (3)

In this case we, took foo(), bar(), my1(), and baz() from A,
yours2() from B, and baz() again from A.  In other words,
yours2() was crossed over from the genome of B into A."
  (multiple-value-bind (a-begin a-end b-begin b-end)
      (select-crossover-points a b)
    (if (and a-begin a-end b-begin b-end)
        (let ((a-children (ast-children (ast-root a)))
              (b-children (ast-children (ast-root b))))
          (nest (copy a :ast-root)
                (copy (ast-root a) :children)
                (append (subseq a-children 0 a-begin)     ;; (1)
                        (subseq b-children b-begin b-end) ;; (2)
                        (subseq a-children a-end))))      ;; (3)
        (copy a))))

(defmethod crossover ((a clang-base) (b clang-base))
  "Redefinition of crossover for clang software objects to utilize the
auto-merge crossover."
  (call-next-method))

(defmethod select-crossover-points ((a parseable) (b parseable))
  "Select 2-pt homologous crossover points in A and B.  Four indices,
A-BEGIN, A-END, B-BEGIN, and B-END will be returned; the crossover product
will contain the top-level children of A from 0 to A-END, the top-level
children of B from B-BEGIN to B-END, and the remaining top-level ASTs
of A from A-END. If no suitable points are found, return nil."
  ;; Because we know A and B are software objects representing the same
  ;; underlying source code with the only differences due to resolution
  ;; of conflict ASTs, we can assume the selection of any two non-conflict
  ;; homologous points at the top-level of A and B will result in a
  ;; reasonable crossover.
  (let ((ast-pool (iter (for ast in (nest (remove-if [{aget :conflict-ast}
                                                      #'ast-aux-data])
                                          (get-immediate-children a)
                                          (ast-root a)))
                        (when (find ast (ast-children (ast-root b))
                                    :test #'ast-equal-p)
                          (collect ast)))))
    ;; AST pool contains those ASTs at the top-level common to A and B.
    (when (<= 2 (length ast-pool))
      (bind ((a-children (ast-children (ast-root a)))
             (b-children (ast-children (ast-root b)))
             ((:values begin end) (select-begin-and-end ast-pool)))
        (values (position begin a-children :test #'ast-equal-p)
                (position end a-children :test #'ast-equal-p)
                (position begin b-children :test #'ast-equal-p)
                (position end b-children :test #'ast-equal-p))))))

(defmethod select-crossover-points ((a clang-base) (b clang-base))
  "Redefinition of select-crossover-points for clang software objects to
utlize the auto-merge crossover."
  (call-next-method))

(defun select-begin-and-end (pool)
  "Return two ordered ASTs from POOL."
  (let* ((ast1 (random-elt pool))
         (ast2 (random-elt (remove ast1 pool))))
    (values (if (ast-later-p ast2 ast1) ast1 ast2)
            (if (ast-later-p ast2 ast1) ast2 ast1))))


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


;;; Fitness testing
(defmethod auto-merge-test ((obj software) (tests test-suite))
  "Determine the fitness of OBJ against TESTS."
  (with-temp-file (bin)
    (if (ignore-phenome-errors (phenome obj :bin bin))
        (mapcar (lambda (test-case)
                  (nth-value 2 (run-test bin test-case)))
                (test-cases tests))
        (make-list (length (test-cases tests))
                   :initial-element most-positive-fixnum))))

(defmethod auto-merge-test :around ((obj project) (tests test-suite))
  "Setup environment so the fitness of OBJ can be evaluated against TESTS."
  ;; Bind *build-dir* so multiple builds can occur in a multi-threaded
  ;; environment.
  (with-temp-dir (dir)
    (let ((*build-dir* dir))
      (call-next-method))))


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
          (*fitness-predicate* #'<)
          (*parseable-mutation-types* '((new-conflict-resolution . 1)))
          (*clang-mutation-types* '((new-conflict-resolution . 1))))

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
      (note 2 "Evolve conflict resolution.")
      (note 2 "~16a ~16a ~a"
            "Generations" "Evaluations" "Best-fitness")
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

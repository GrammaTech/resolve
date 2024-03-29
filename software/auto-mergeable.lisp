;;;; auto-mergeable.lisp - Mixin for auto-merge software objects.
;;;
;;;  This mixin allows us to define custom behavior for auto-merge
;;;  simple, parseable, and project representations.
;;;
;;;  For simple representations, we define a custom 2-pt crossover
;;;  to avoid breaking up existing conflict resolutions.  Further,
;;;  we add support for handling ASTs (stubs and conflict ASTs)
;;;  within the simple software object representation.
;;;
;;;  For parseable representations, we define a custom 2-pt
;;;  crossover on top-level ASTs.  This bypasses the error-prone
;;;  clang crossover implementation and allows for exchange
;;;  of conflict AST resolutions.
;;;
;;;  For project representations, we override the `pick-file`
;;;  interface to select for mutation those files with the
;;;  most conflict ASTs proportionally.  Further, we override
;;;  the `ast-patch` interface to file handle insertions and
;;;  deletions.
(defpackage :resolve/software/auto-mergeable
  (:use :gt/full
        :metabang-bind
        :resolve/alist
        :resolve/ast-diff
        :resolve/software/parseable
        :software-evolution-library
        :software-evolution-library/components/file
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/lisp
        :software-evolution-library/software/project
        :software-evolution-library/software/c-project
        :software-evolution-library/software/cpp-project
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/lisp-project
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/javascript
        :software-evolution-library/software/typescript)
  (:local-nicknames (:ts :software-evolution-library/software/tree-sitter))
  (:import-from :resolve/ast-diff :ast-diff* :ast-patch*)
  (:import-from :software-evolution-library/utility/debug
                :note)
  (:import-from :functional-trees :slot-specifier :slot-specifier-slot
   :slot-specifier-arity :slot-specifier-for-slot)
  (:import-from :software-evolution-library/utility/task :task-map)
  (:export ;; auto-mergeable data structures
           :auto-mergeable
           :auto-mergeable-simple
           :auto-mergeable-parseable
           :auto-mergeable-c
           :auto-mergeable-cpp
           :auto-mergeable-javascript
           :auto-mergeable-typescript
           :auto-mergeable-lisp
           :auto-mergeable-project
           :auto-mergeable-c-project
           :auto-mergeable-cpp-project
           :auto-mergeable-javascript-project
           :auto-mergeable-typescript-project
           :auto-mergeable-lisp-project
           :create-auto-mergeable
           ;; auto-mergeable mutations
           :get-conflicts
           :get-resolved-conflicts
           :get-conflict-strategies
           :new-conflict-resolution
           :apply-new-conflict-resolution))
(in-package :resolve/software/auto-mergeable)
(in-readtable :curry-compose-reader-macros)


;;; Auto-merge mixin and data structures.
;;
;;  Note: subclasses of parseable and project (e.g. clang,
;;  javascript-project) override methods outside of project creation
;;  (e.g. phenome).  Because of this, we need to retain the specific
;;  subclass of the software object when creating the auto-mergeable
;;  mixin variant to ensure proper operation when calling overridden
;;  routines.

;;; TODO: refactor to auto generate tree-sitter stuff?
(define-software auto-mergeable () ())
(define-software auto-mergeable-simple (auto-mergeable simple) ())
(define-software auto-mergeable-parseable (auto-mergeable parseable) ())
(define-software auto-mergeable-javascript (auto-mergeable-parseable javascript) ())
(define-software auto-mergeable-typescript (auto-mergeable-parseable typescript) ())
(define-software auto-mergeable-c (auto-mergeable-parseable c) ())
(define-software auto-mergeable-cpp (auto-mergeable-parseable c) ())
(define-software auto-mergeable-json (auto-mergeable-parseable json) ())
(define-software auto-mergeable-lisp (auto-mergeable-parseable lisp) ())
(define-software auto-mergeable-project (auto-mergeable parseable-project) ())
(define-software auto-mergeable-c-project
    (auto-mergeable-project c-project) ())
(define-software auto-mergeable-cpp-project
    (auto-mergeable-project cpp-project) ())
(define-software auto-mergeable-javascript-project
    (auto-mergeable-project javascript-project) ())
(define-software auto-mergeable-typescript-project
    (auto-mergeable-project typescript-project) ())
(define-software auto-mergeable-lisp-project
    (auto-mergeable-project lisp-project) ())


;;; Creation routine
(defgeneric create-auto-mergeable (soft &key &allow-other-keys)
  (:documentation "Create an auto-mergeable software object from SOFT.")
  (:method ((obj simple) &key)
    (change-class (copy obj) 'auto-mergeable-simple))
  (:method ((obj javascript) &key)
    (change-class (copy obj) 'auto-mergeable-javascript))
  (:method ((obj typescript) &key)
    (change-class (copy obj) 'auto-mergeable-typescript))
  (:method ((obj c) &key)
    (change-class (copy obj) 'auto-mergeable-c))
  (:method ((obj cpp) &key)
    (change-class (copy obj) 'auto-mergeable-cpp))
  (:method ((obj json) &key)
    (change-class (copy obj) 'auto-mergeable-json))
  (:method ((obj lisp) &key)
    (change-class (copy obj) 'auto-mergeable-lisp))
  (:method ((obj project) &key (threads 1) (already-merged (empty-set)))
    (labels ((task-filter (fn objects)
               (nest
                (map 'list #'cdr)
                ((lambda (xs) (remove nil xs :key #'car)))
                (fbind fn)
                (task-map threads
                          (lambda (obj)
                            (cons (fn obj) obj))
                          objects)))
             (necessary? (file-obj-pair)
               (string-case (car file-obj-pair)
                 ("package.json" t)
                 ("tsconfig.json" t)
                 (t nil)))
             (already-merged? (file-obj-pair)
               (contains? already-merged (car file-obj-pair)))
             (ignorable? (file.obj)
               (and (already-merged? file.obj)
                    (not (necessary? file.obj))))
             (parseable-file-p (file-obj-pair)
               "Filter files which can be parsed into ASTs."
               (nest
                (when (typep (cdr file-obj-pair) 'parseable))
                (unless (ignorable? file-obj-pair))
                (handler-case
                    (genome (cdr file-obj-pair))
                  (mutate (c) (declare (ignorable c)) nil))))
             (evolve-files ()
               "Return a list of `auto-mergeable-parseable` software objects
               representing source files which have been parsed into ASTs."
               (mapcar (lambda-bind ((file . obj))
                         (cons file (create-auto-mergeable obj)))
                       (task-filter #'parseable-file-p
                                    (remove-if #'ignorable?
                                               (all-files obj)))))
             (other-files ()
               "Return a list of `auto-mergeable-simple` software objects
               representing flat text files."
               (mapcar (lambda-bind ((file . obj))
                         (declare (ignorable obj))
                         (cons file
                               (from-file (make-instance 'auto-mergeable-simple)
                                          (original-path obj))))
                       (remove-if #'parseable-file-p
                                  (remove-if #'ignorable?
                                             (all-files obj))))))
      (change-class (copy obj :evolve-files (evolve-files)
                              :other-files (other-files))
                    (symbol-cat 'auto-mergeable (type-of obj))))))


;;; Software interface overrides for auto-merge specific behavior
(defmethod crossover ((a auto-mergeable-parseable)
                      (b auto-mergeable-parseable))
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
        (let* ((a-children (children (genome a)))
               (b-children (children (genome b)))
               (children (append (subseq a-children 0 a-begin)     ;; (1)
                                 (subseq b-children b-begin b-end) ;; (2)
                                 (subseq a-children a-end))))      ;; (3)
          ;; TODO: there's likely an issue with everything not being copied here
          ;;       with named children.
          (copy a :genome (copy (genome a) :children children)))
        (copy a))))

(defmethod select-crossover-points ((a auto-mergeable-parseable)
                                    (b auto-mergeable-parseable))
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
                                                      #'ast-annotations])
                                          (child-asts)
                                          (genome a)))
                        (when (find ast (child-asts (genome b))
                                    :test #'equal?)
                          (collect ast)))))
    ;; AST pool contains those ASTs at the top-level common to A and B.
    (when (<= 2 (length ast-pool))
      (bind ((a-children (children (genome a)))
             (b-children (children (genome b)))
             ((:values begin end) (select-begin-and-end ast-pool)))
            (values (position begin a-children :test #'equal?)
                    (position end a-children :test #'equal?)
                    (position begin b-children :test #'equal?)
                    (position end b-children :test #'equal?))))))

(defun select-begin-and-end (pool)
  "Return two ordered ASTs from POOL."
  (let* ((ast1 (random-elt pool))
         (ast2 (random-elt (remove ast1 pool)))
         (index1 (position ast1 pool))
         (index2 (position ast2 pool)))
    (values (if (< index1 index2) ast1 ast2)
            (if (< index1 index2) ast2 ast1))))

(defmethod size ((obj auto-mergeable-parseable))
  "Override size to allow include the AST root in the calculation as
it may be mutated by auto-merge."
  (length (child-asts (genome obj) :recursive t)))

(defmethod to-file ((obj auto-mergeable-parseable) path)
  "Override to-file to avoid writing to disk those objects with an
AST stub root indicating they were deleted from the project."
  (if (typep (genome obj) 'ast-stub)
      (when (probe-file path)
        (delete-file path))
      (call-next-method)))

(defmethod phenome ((obj auto-mergeable-simple) &key bin)
  "Override phenome to write the simple text software object to disk
at BIN."
  (to-file obj bin)
  (values bin 0 "" ""))

(defmethod lines ((obj auto-mergeable-simple) &key &allow-other-keys)
  "Override lines to allow for conflict and stub ASTs to appear in the
genome."
  (nest (remove nil)
        (mapcar (lambda (line)
                  (if (typep line 'ast)
                      (source-text line)
                      line)))
        (mapcar {aget :code} (genome obj))))

(defmethod to-file ((obj auto-mergeable-simple) path)
  "Override to-file to avoid writing to disk those objects with an
AST stub root indicating they were deleted from the project."
  (if (and (typep (aget :code (car (genome obj))) 'ast-stub)
           (emptyp (genome-string obj)))
      (when (probe-file path)
        (delete-file path))
      (call-next-method)))

(defmethod crossover ((a auto-mergeable-simple) (b auto-mergeable-simple))
  "Crossover two simple software objects.  This implementation
performs a 2-pt, homologous crossover of lines in A and B, as described
above in the `auto-mergeable-parseable` implementation.  We assume A
and B are identical with the exception of conflicts, and we take
care to avoid splitting existing conflict resolutions which would interfere
with the auto-merge evolutionary loop."
  (labels ((non-conflict-indices (obj)
             "Return a list of integer indices into the genome of OBJ which
             are non-conflict points."
             (nest (mapcar #'car)
                   (remove-if [{typep _ 'ast} {aget :code} #'cadr])
                   (indexed (genome obj)))))
    (let ((pool (iter (for a-index in (non-conflict-indices a))
                      (for b-index in (non-conflict-indices b))
                      (collect (list (cons :a-index a-index)
                                     (cons :b-index b-index))))))
      (if (<= 2 (length pool))
          (let ((points (sort (list (random-elt pool) (random-elt pool)) #'<
                              :key {aget :a-index})))
            (values (copy a :genome
                          (append (subseq (genome a) 0
                                          (aget :a-index (first points)))
                                  (subseq (genome b)
                                          (aget :b-index (first points))
                                          (aget :b-index (second points)))
                                  (subseq (genome a)
                                          (aget :a-index (second points)))))
                    points))
          (values (copy a) nil)))))

(defmethod pick-file ((obj auto-mergeable-project))
  "Randomly pick one evolve file proportionally based on the number
of conflict ASTs."
  (proportional-pick (evolve-files obj)
                     (lambda-bind ((name . obj))
                                  (declare (ignorable name))
                                  (length (get-resolved-conflicts obj)))))


;;; Auto-merge mutations

(defmethod pick-mutation-type ((obj auto-mergeable))
  'new-conflict-resolution)

(define-mutation new-conflict-resolution (parseable-mutation)
  ((targeter :initform #'get-resolved-conflict)
   (strategy :initarg :strategy :reader strategy :initform nil))
  (:documentation "Replace a conflict AST resolution with an alternative
option."))

(defmethod apply-mutation ((software auto-mergeable)
                           (mutation new-conflict-resolution))
  (multiple-value-call #'apply-new-conflict-resolution
    software
    (targets mutation)
    (if-let (strategy (strategy mutation))
      (values :strategy strategy)
      (values))))

(defun random-strategy (conflict-ast)
  (random-elt (get-conflict-strategies conflict-ast)))

(defgeneric apply-new-conflict-resolution (software conflict-ast &key strategy))

(defmethod apply-new-conflict-resolution
    ((software auto-mergeable-parseable)
     conflict-ast
     &key (strategy (random-strategy conflict-ast)))
  "Replace an existing conflict AST resolution with an alternative option."
  (declare (keyword strategy))
  (labels ((new-slot-value (parent slot arity index new-resolution prior-resolution)
             (if (eql arity 1)
                 (car new-resolution)
                 (splice-seq
                  (slot-value parent slot)
                  :start index
                  :new new-resolution
                  :end (+ index (length prior-resolution)))))
           (insert-into-parent-slot
               (parent slot arity index new-resolution prior-resolution)
             "Return ops to insert NEW-RESOLUTION at INDEX of SLOT in PARENT."
             (let* ((new-slot-value
                     (new-slot-value parent slot arity index
                                     new-resolution prior-resolution))
                    (slot-keyword (make-keyword slot))
                    (new-parent
                     (copy parent slot-keyword new-slot-value)))
               ;; Check if the resulting AST is valid by trying to get
               ;; its list of children. If it is invalid, and if we
               ;; are inserting multiple ASTs, it's probably invalid
               ;; because we are inserting multiple children into a
               ;; slot that doesn't actually allow them \(the slot
               ;; might be variadic but still have a rule that doesn't
               ;; allow two ASTs of the same kind a row.) Instead, we
               ;; move up the tree to the grandparent, wrap each AST
               ;; in new-resolution in a copy of the parent, and
               ;; insert them as children of the grandparent.
               (handler-bind
                   ((rule-matching-error
                     (nest
                      (lambda (e) (declare (ignore e)))
                      (when (and (member strategy '(:c1 :c2))
                                  (or (rest new-slot-value))
                                  (not (typep parent 'root-ast))))
                      (return-from apply-new-conflict-resolution)
                       (insert-normalizing-path
                        (ast-path software parent)
                        (get-parent-ast software parent)
                        (mapcar
                         (lambda (nr)
                           (tree-copy (copy parent
                                            slot-keyword
                                            (new-slot-value
                                             parent
                                             slot
                                             arity
                                             index
                                             (list nr)
                                             prior-resolution))))
                         new-resolution)
                        (list parent)))))
                 (children new-parent))
               #+nil
               (let ((pos (position (@ parent lccp) (children parent))))
                 ;; TODO: does this assertion still make sense to have?
                 (assert pos))
               (with software parent new-parent)))
           (insert-normalizing-path (conflict-path parent new-resolution prior-resolution)
             (nlet retry ((lccp (lastcar conflict-path)))
               (cond
                 ((null conflict-path)
                  (with software
                        conflict-path
                        (car new-resolution)))
                 ((symbolp lccp)
                  ;; Retry after normalizing lccp.
                  (retry (cons lccp 0)))
                 ((typep lccp '(cons symbol integer))
                  (retry (cons (slot-specifier-for-slot parent (car lccp))
                               (cdr lccp))))
                 ((typep lccp '(cons (or slot-specifier symbol) integer))
                  (let* ((slot-specifier (car lccp))
                         (index (cdr lccp))
                         (arity (slot-specifier-arity slot-specifier))
                         (slot (slot-specifier-slot slot-specifier)))
                    #+auto-mergeable-debug
                    (progn
                      (format t "slot = ~a~%" slot)
                      (format t "arity = ~a~%" arity)
                      (format t "slot-value = ~a~%" (slot-value parent slot)))
                    (insert-into-parent-slot
                     parent slot arity index new-resolution prior-resolution)))
                 (t
                  (insert-into-parent-slot
                   parent :children 0 lccp
                   new-resolution prior-resolution))))))
    (let* ((prior-resolution (or (remove-if-not [{eq conflict-ast}
                                                 {aget :conflict-ast}
                                                 #'ast-annotations]
                                                (get-resolved-conflicts software))
                                 (list conflict-ast)))
           (new-resolution (resolve-conflict-ast conflict-ast
                                                 :strategy strategy))
           (conflict-path (ast-path software (car prior-resolution)))
           (parent (@ software (butlast conflict-path))))
      (declare (list new-resolution))
      (assert (find (car prior-resolution) (genome software)))
      #+auto-mergeable-debug
      (progn
        (format t "conflict-ast = ~a~%" conflict-ast)
        (format t "strategy = ~a~%" strategy)
        (format t "prior-resolution = ~a~%" prior-resolution)
        (format t "new-resolution = ~a~%" new-resolution)
        (format t "conflict-path = ~a~%" conflict-path)
        (format t "parent = ~a~%" parent))
      ;; Replace the prior resolution children with the new resolution
      (insert-normalizing-path conflict-path parent new-resolution prior-resolution))))

(defmethod apply-new-conflict-resolution
    ((software auto-mergeable-simple) conflict-ast
     &key (strategy (random-strategy conflict-ast)))
  "Apply the NEW-CONFLICT-RESOLUTION mutation to SOFTWARE."
  ;; Simple software objects are a more primitive representation and therefore,
  ;; we need to override `apply-mutation` instead of `build-op`.
  (declare (keyword strategy))
  (let* ((prior-resolution (or (remove-if-not «and [{typep _ 'ast} {aget :code}]
                                                   [{eq conflict-ast}
                                                    {aget :conflict-ast}
                                                    #'ast-annotations
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

(defgeneric get-conflict-strategies (ast)
  (:documentation "Return a list of strategies which may be utilized
to resolve the conflict AST.")
  (:method ((ast conflict-ast))
    (if (aget :top-level (ast-annotations ast))
        '(:V1 :NN)
        '(:V1 :V2 :C1 :C2 :NN))))

(defgeneric resolve-conflict-ast (conflict &key strategy)
  (:documentation "Return a concrete resolution of CONFLICT AST
using STRATEGY.")
  (:method ((conflict ast)
            &key (strategy (random-elt (get-conflict-strategies conflict)))
            &aux (options (conflict-ast-child-alist conflict)))
    (labels ((normalize (children &key tree-copy)
               "Normalize CHILDREN by adding the conflict AST
               to each child AST's annotations.  If there are no children,
               create a NullStmt AST with this annotations.  The annotations
               are required for the `new-conflict-resolution` mutation.

               If TREE-COPY is non-nil, then the children are copied
               to ensure that there are no conflicts in their serial
               numbers."
               (if children
                   (mapcar (lambda (child)
                             (if (typep child 'ast)
                                 (copy child :annotations
                                       (append `((:conflict-ast . ,conflict))
                                               (ast-annotations child)))
                                 (make-instance 'ast-stub
                                  :children (list child)
                                  :annotations `((:conflict-ast . ,conflict)))))
                           ;; Tree copy avoids possible interval collisions.
                           (if tree-copy
                               (mapcar #'tree-copy children)
                               children))
                   (list (make-instance 'ast-stub
                          :annotations `((:conflict-ast . ,conflict)))))))
      ;; Five ways of resolving a conflict:
      (case strategy
        ;; 1. (V1) version 1
        (:V1 (normalize (aget :my options)))
        ;; 2. (V2) version 2
        (:V2 (normalize (aget :your options)))
        ;; 3. (CC) concatenate versions (either order)
        (:C1 (normalize (append (aget :my options) (aget :your options))
                        :tree-copy t))
        (:C2 (normalize (append (aget :your options) (aget :my options))
                        :tree-copy t))
        ;; 4. (NN) select the base version
        (:NN (normalize (aget :old options)))))))


;;; Auto-mergeable specific methods
(defgeneric get-conflicts (obj)
  (:documentation "Return the conflicts in OBJ.")
  (:method ((obj t)) nil)
  (:method ((obj auto-mergeable-simple))
    (remove-if-not {typep _ 'conflict-ast}
                   (mapcar {aget :code} (genome obj))))
  (:method ((obj auto-mergeable-parseable))
    (remove-if-not {typep _ 'conflict-ast}
                   (cons (genome obj) (child-asts (genome obj) :recursive t))))
  (:method ((obj auto-mergeable-project))
    (mappend [#'get-conflicts #'cdr] (all-files obj))))

(defgeneric get-resolved-conflicts (obj)
  (:documentation "Return the resolved conflicts in OBJ.")
  (:method ((obj t)) nil)
  (:method ((obj auto-mergeable-simple))
    (remove-if-not «and {typep _ 'ast} [{aget :conflict-ast} #'ast-annotations]»
                   (mapcar {aget :code} (genome obj))))
  (:method ((obj auto-mergeable-parseable))
    (remove-if-not [{aget :conflict-ast} #'ast-annotations]
                   (cons (genome obj) (child-asts (genome obj) :recursive t))))
  (:method ((obj auto-mergeable-project))
    (mappend [#'get-resolved-conflicts #'cdr] (all-files obj))))

(defgeneric get-resolved-conflict (obj)
  (:documentation "Return a conflict previously resolved in OBJ.")
  (:method ((obj t)) nil)
  (:method ((obj auto-mergeable))
    (if-let ((conflicts (nest (remove-duplicates)
                              (mapcar [{aget :conflict-ast} #'ast-annotations])
                              (get-resolved-conflicts obj))))
      (random-elt conflicts)
      (error (make-condition 'no-mutation-targets
                             :obj obj :text "No resolved conflict asts.")))))


;;; AST diff interface overrides
(defmethod ast-patch* ((project auto-mergeable-project) (diff t)
                       &rest args &key &allow-other-keys)
  "Override for auto-mergeable-projects supporting the addition and
deletion of files in the project in a manner amendable to the auto-merge
evolutionary loop."
  (assert (eq :alist (car diff)) (diff)
          "ast-patch cannot be applied to a project with a malformed diff.")
  (labels ((handle-inserted-and-deleted-files (patch-files)
             "Create conflict ASTs for files which have been inserted
             or deleted wholesale."
             (reduce
              (lambda (patch-files single-diff)
                (let ((diff-type (car single-diff))
                      (file (cadr single-diff))
                      (obj (cddr single-diff)))
                  (cond ((eq :insert-alist diff-type)
                         (areplace file
                                   (create-top-level-conflict obj :my)
                                   patch-files))
                        ((eq :delete-alist diff-type)
                         (areplace file
                                   (create-top-level-conflict obj :old)
                                   patch-files))
                        (t patch-files))))
              (cadr diff)
              :initial-value patch-files))
           (get-patch-files ()
             "Return a list of all files in the project with conflict
             ASTs inserted as appropriate."
             (nest (handle-inserted-and-deleted-files)
                   (alist-of-alist-for-diff)
                   (apply #'ast-patch*
                          (make-instance 'alist-for-diff
                            :alist (all-files project))
                          diff args)))
           (evolve-file? (file-obj-pair)
             "Return true if the cons pair (file . obj) represents a
             file which should be included in the project's evolve-files
             list."
             (get-conflicts (cdr file-obj-pair)))
           (evolve-files ()
             "Return those files which should be included in the
             project's evolve-files list for subsequent mutation."
             (remove-if-not #'evolve-file? (get-patch-files)))
           (other-files ()
             "Return those files which should be included in the
             project's other-files list and not subsequently mutated."
             (remove-if #'evolve-file? (get-patch-files))))
    (copy project
          :evolve-files (evolve-files)
          :other-files (other-files))))

(defgeneric create-top-level-conflict (obj conflict-child)
  (:documentation "Create a conflict AST at the 'top-level' of OBJ representing
an insertion or deletion of the OBJ.")
  (:method ((obj auto-mergeable-simple) (conflict-child symbol))
    (nest (copy obj :genome)
          (list)
          (list)
          (cons :code)
          (make-instance 'conflict-ast :annotations (list (cons :top-level t))
                                       :child-alist)
          (list (cons conflict-child (mapcar {aget :code} (genome obj))))))
  (:method ((obj auto-mergeable-parseable) (conflict-child symbol))
    (nest (copy obj :genome)
          (make-instance 'conflict-ast :annotations (list (cons :top-level t))
                                       :child-alist)
          (list (list conflict-child (genome obj))))))

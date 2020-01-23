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
  (:use :common-lisp
        :alexandria
        :iterate
        :named-readtables
        :closer-mop
        :curry-compose-reader-macros
        :metabang-bind
        :resolve/alist
        :resolve/ast-diff
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/clang
        :software-evolution-library/software/file
        :software-evolution-library/software/simple
        :software-evolution-library/software/source
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/lisp-project)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:import-from :resolve/ast-diff :ast-diff* :ast-patch*)
  (:import-from :uiop :nest)
  (:export :auto-mergeable
           :auto-mergeable-simple
           :auto-mergeable-parseable
           :auto-mergeable-project
           :auto-mergeable-clang-project
           :auto-mergeable-javascript-project
           :auto-mergeable-lisp-project
           :create-auto-mergeable
           :get-conflicts
           :get-resolved-conflicts))
(in-package :resolve/software/auto-mergeable)
(in-readtable :curry-compose-reader-macros)


;;; Auto-merge mixin and data structures.
;;
;;  Note: Some subclasses of project (e.g. javascript-project)
;;  override methods outside of project creation (e.g. phenome).
;;  Because of this, we need to retain the specific subclass of
;;  the project when creating the auto-mergeable mixin variant
;;  to ensure proper operation when calling overridden routines.
(define-software auto-mergeable () ())
(define-software auto-mergeable-simple (auto-mergeable simple) ())
(define-software auto-mergeable-parseable (auto-mergeable parseable) ())
(define-software auto-mergeable-project (auto-mergeable parseable-project) ())

(define-software auto-mergeable-clang-project
    (auto-mergeable-project clang-project) ())
(define-software auto-mergeable-javascript-project
    (auto-mergeable-project javascript-project) ())
(define-software auto-mergeable-lisp-project
    (auto-mergeable-project lisp-project) ())


;;; Creation routine
(defgeneric create-auto-mergeable (soft)
  (:documentation "Create an auto-mergeable software object from SOFT.")
  (:method ((obj software))
    obj)
  (:method ((obj simple))
    (change-class (copy obj) 'auto-mergeable-simple))
  (:method ((obj parseable))
    (change-class (copy obj) 'auto-mergeable-parseable))
  (:method ((obj project))
    (flet ((auto-mergeable-files (files)
             (mapcar (lambda-bind ((file . obj))
                                  (cons file (create-auto-mergeable obj)))
                     files)))
      (change-class (copy obj :evolve-files
                          (auto-mergeable-files (evolve-files obj))
                          :other-files
                          (auto-mergeable-files (other-files obj)))
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
        (let ((a-children (ast-children (ast-root a)))
              (b-children (ast-children (ast-root b))))
          (nest (copy a :ast-root)
                (copy (ast-root a) :children)
                (append (subseq a-children 0 a-begin)     ;; (1)
                        (subseq b-children b-begin b-end) ;; (2)
                        (subseq a-children a-end))))      ;; (3)
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

(defun select-begin-and-end (pool)
  "Return two ordered ASTs from POOL."
  (let* ((ast1 (random-elt pool))
         (ast2 (random-elt (remove ast1 pool))))
    (values (if (ast-later-p ast2 ast1) ast1 ast2)
            (if (ast-later-p ast2 ast1) ast2 ast1))))

(defmethod size ((obj auto-mergeable-parseable))
  "Override size to allow include the AST root in the calculation as
it may be mutated by auto-merge."
  (length (ast-to-list (ast-root obj))))

(defmethod to-file ((obj auto-mergeable-parseable) path)
  "Override to-file to avoid writing to disk those objects with an
AST stub root indicating they were deleted from the project."
  (if (typep (ast-root obj) 'ast-stub)
      (when (probe-file path)
        (delete-file path))
      (call-next-method)))

(defmethod phenome ((obj auto-mergeable-simple) &key bin)
  "Override phenome to write the simple text software object to disk
at BIN."
  (to-file obj bin)
  (values bin 0 "" ""))

(defmethod lines ((obj auto-mergeable-simple))
  "Override lines to allow for conflict and stub ASTs to appear in the
genome."
  (nest (remove nil)
        (mapcar (lambda (line)
                  (if (ast-p line)
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
                   (remove-if [#'ast-p {aget :code} #'cadr])
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


;;; Auto-mergeable specific methods
(defgeneric get-conflicts (obj)
  (:documentation "Return the conflicts in OBJ.")
  (:method ((obj t)) nil)
  (:method ((obj auto-mergeable-simple))
    (remove-if-not #'conflict-ast-p
                   (mapcar {aget :code} (genome obj))))
  (:method ((obj auto-mergeable-parseable))
    (remove-if-not #'conflict-ast-p
                   (ast-to-list (ast-root obj)))))

(defgeneric get-resolved-conflicts (obj)
  (:documentation "Return the resolved conflicts in OBJ.")
  (:method ((obj t)) nil)
  (:method ((obj auto-mergeable-simple))
    (remove-if-not «and #'ast-p [{aget :conflict-ast} #'ast-aux-data]»
                   (mapcar {aget :code} (genome obj))))
  (:method ((obj auto-mergeable-parseable))
    (remove-if-not [{aget :conflict-ast} #'ast-aux-data]
                   (ast-to-list (ast-root obj)))))


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
          (make-conflict-ast :aux-data (list (cons :top-level t)) :child-alist)
          (list (cons conflict-child (mapcar {aget :code} (genome obj))))))
  (:method ((obj auto-mergeable-parseable) (conflict-child symbol))
    (nest (copy obj :ast-root)
          (make-conflict-ast :aux-data (list (cons :top-level t)) :child-alist)
          (list (list conflict-child (ast-root obj))))))

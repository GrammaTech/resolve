(defpackage :resolve/software/parseable
  (:use :gt/full
        :resolve/ast-diff
        :software-evolution-library
        :software-evolution-library/software/parseable)
  (:import-from :resolve/ast-diff :ast-diff* :ast-patch*)
  (:import-from :software-evolution-library/software/tree-sitter
                :source-text-fragment-variation-point)
  (:export ast-stub))
(in-package :resolve/software/parseable)
(in-readtable :curry-compose-reader-macros)

;;; TODO: using source-text-fragment-variation-point here is a bit of
;;;       a hack, but it isn't strictly incorrect. Maybe find a better
;;;       way to do this in the future. NOTE: This used to inherit
;;;       directly from source-text-fragment, but that's no longer
;;;       considered a wildcard, so we use
;;;       source-text-fragment-variation-point, which is.
(defclass ast-stub (source-text-fragment-variation-point functional-tree-ast)
  ((children :type list
             :initarg :children
             :initform nil
             :documentation "The list of children of the node,
which may be more nodes, or other values.")
   (child-slots :initform '(children) :allocation :class))
  (:documentation "Minimal concrete subclass of AST useful a placeholder
where an AST is expected.  ast-stub is used to represent a conflict
resolution where the AST is removed.  A stub is inserted and the
original conflict AST is stored as an annotation; in future mutations,
we can try a different conflict solution by examining the ast stub's
conflict AST annotation and selecting a different resolution"))

(defmethod ast-diff* ((parseable-a parseable) (parseable-b parseable))
  #+debug (format t "ast-diff[PARSEABLE]~%")
  (ast-diff* (genome parseable-a) (genome parseable-b)))

(defmethod create-edit-tree ((source parseable) (target parseable) script
                             &rest args &key &allow-other-keys)
  (apply #'create-edit-tree (genome source) (genome target) script args))

(defmethod ast-patch* ((obj parseable) (diff list)
                       &rest keys &key &allow-other-keys)
  (setf obj (copy obj))
  (setf (genome obj) (apply #'ast-patch* (genome obj) diff keys))
  obj)

(defmethod converge ((obj2 parseable) (obj1 parseable) (obj3 parseable)
                     &rest args &key &allow-other-keys)
  (copy obj1
        :genome (apply #'converge
                       (genome obj2)
                       (genome obj1)
                       (genome obj3)
                       args)))

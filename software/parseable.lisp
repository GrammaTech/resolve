(defpackage :resolve/software/parseable
  (:use :gt/full
        :resolve/ast-diff
        :software-evolution-library
        :software-evolution-library/software/parseable)
  (:import-from :resolve/ast-diff :ast-diff* :ast-patch*)
  (:export ast-stub))
(in-package :resolve/software/parseable)
(in-readtable :curry-compose-reader-macros)

(defclass ast-stub (functional-tree-ast)
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
  (ast-diff* (ast-root parseable-a) (ast-root parseable-b)))

(defmethod create-edit-tree ((source parseable) (target parseable) script
                             &rest args &key &allow-other-keys)
  (apply #'create-edit-tree (ast-root source) (ast-root target) script args))

(defmethod ast-patch* ((obj parseable) (diff list)
                       &rest keys &key &allow-other-keys)
  (setf obj (copy obj))
  (setf (ast-root obj) (apply #'ast-patch* (ast-root obj) diff keys))
  obj)

(defmethod converge ((obj2 parseable) (obj1 parseable) (obj3 parseable)
                     &rest args &key &allow-other-keys)
  (nest (copy obj1 :genome nil :ast-root)
        (populate-fingers)
        (apply #'converge (ast-root obj2) (ast-root obj1) (ast-root obj3) args)))

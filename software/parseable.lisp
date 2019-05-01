(defpackage :resolve/software/parseable
  (:use :common-lisp
        :alexandria
        :iterate
        :named-readtables
        :curry-compose-reader-macros
        :resolve/ast-diff
        :software-evolution-library
        :software-evolution-library/software/parseable))
(in-package :resolve/software/parseable)
(in-readtable :curry-compose-reader-macros)

(defmethod ast-diff ((parseable-a parseable) (parseable-b parseable) &rest args
                     &key &allow-other-keys)
  (apply #'ast-diff (ast-root parseable-a) (ast-root parseable-b) args))

(defmethod create-edit-tree ((source parseable) (target parseable) script
                             &rest args &key &allow-other-keys)
  (apply #'create-edit-tree (ast-root source) (ast-root target) script args))

(defmethod ast-patch ((obj parseable) (diff list)
                      &rest keys &key &allow-other-keys)
  (setf obj (copy obj))
  (setf (ast-root obj) (apply #'ast-patch (ast-root obj) diff keys))
  obj)

(defmethod converge ((obj2 parseable) (obj1 parseable) (obj3 parseable)
                     &rest args &key &allow-other-keys)
  (let ((root1 (ast-root obj1))
	(root2 (ast-root obj2))
	(root3 (ast-root obj3)))
    (multiple-value-bind (merged-root problems)
	(apply #'converge root2 root1 root3 args)
      (declare (ignorable problems))
      (make-instance (class-of obj1) :genome nil :ast-root merged-root))))

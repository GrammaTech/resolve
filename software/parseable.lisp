(defpackage :resolve/software/parseable
  (:use :common-lisp
        :alexandria
        :iterate
        :named-readtables
        :curry-compose-reader-macros
        :resolve/ast-diff
        :software-evolution-library
        :software-evolution-library/software/parseable)
  (:import-from :resolve/ast-diff :ast-diff* :ast-patch*))
(in-package :resolve/software/parseable)
(in-readtable :curry-compose-reader-macros)

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
  (make-instance (class-of obj1)
    :genome nil
    :ast-root (apply #'converge (ast-root obj2)
                     (ast-root obj1)
                     (ast-root obj3)
                     args)))

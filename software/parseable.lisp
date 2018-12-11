(defmethod ast-diff ((parseable-a parseable) (parseable-b parseable))
  (ast-diff (ast-root parseable-a) (ast-root parseable-b)))

(defmethod ast-patch ((obj parseable) (diff list) &rest keys &key &allow-other-keys)
  (setf obj (copy obj))
  (setf (ast-root obj) (apply #'ast-patch (ast-root obj) diff keys))
  obj)

(defmethod converge ((obj1 parseable) (obj2 parseable) (obj3 parseable) &rest args &key &allow-other-keys)
  (let ((root1 (ast-root obj1))
	(root2 (ast-root obj2))
	(root3 (ast-root obj3)))
    (multiple-value-bind (merged-root problems)
	(apply #'converge root1 root2 root3 args)
      (declare (ignorable problems))
      (make-instance (class-of obj1) :genome nil :ast-root merged-root))))



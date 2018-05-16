(defmethod ast-diff ((parseable-a parseable) (parseable-b parseable))
  (ast-diff (ast-root parseable-a) (ast-root parseable-b)))

(defmethod ast-patch ((obj parseable) (diff list))
  (setf (ast-root obj) (ast-patch (ast-root obj) diff))
  obj)

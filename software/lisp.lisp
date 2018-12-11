(defmethod ast-can-recurse ((ast-a lisp-ast) (ast-b lisp-ast))
  (and (eq (ast-class ast-a) (ast-class ast-b))
       (or (not (eql :expression (ast-class ast-a)))
           ;; Special handling for lisp expression ASTs.
           ;; Don't descend into atomic expressions.
           (not (or (atom (aget :expression (ast-aux-data ast-a)))
                    (atom (aget :expression (ast-aux-data ast-b))))))))

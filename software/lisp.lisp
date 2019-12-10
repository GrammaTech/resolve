(defpackage :resolve/software/lisp
  (:use :common-lisp
        :alexandria
        :iterate
        :named-readtables
        :curry-compose-reader-macros
        :resolve/ast-diff
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/lisp))
(in-package :resolve/software/lisp)
(in-readtable :curry-compose-reader-macros)

(defmethod ast-can-recurse ((ast-a sel/sw/lisp::lisp-ast) (ast-b sel/sw/lisp::lisp-ast))
  (and (eq (ast-class ast-a) (ast-class ast-b))
       (or (not (eql :expression (ast-class ast-a)))
           ;; Special handling for lisp expression ASTs.
           ;; Don't descend into atomic expressions.
           (not (or (atom (aget :expression (ast-aux-data ast-a)))
                    (atom (aget :expression (ast-aux-data ast-b))))))))

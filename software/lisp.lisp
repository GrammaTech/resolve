(defpackage :resolve/software/lisp
  (:use :gt/full
        :resolve/ast-diff
        :software-evolution-library
        :software-evolution-library/software/parseable
        :software-evolution-library/software/lisp)
  (:import-from :resolve/ast-diff :ast-class))
(in-package :resolve/software/lisp)
(in-readtable :curry-compose-reader-macros)

(defmethod ast-can-recurse ((ast-a sel/sw/lisp::lisp-ast) (ast-b sel/sw/lisp::lisp-ast))
  (and (eq (ast-class ast-a) (ast-class ast-b))
       (or (not (eql :expression (ast-class ast-a)))
           ;; Special handling for lisp expression ASTs.
           ;; Don't descend into atomic expressions.
           (not (or (atom (aget :expression (ast-annotations ast-a)))
                    (atom (aget :expression (ast-annotations ast-b))))))))

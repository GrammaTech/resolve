;;;; auto-merge.lisp -- Main auto-merge command-line driver
(defpackage :auto-merge/auto-merge
  (:documentation "Main auto-merge command-line driver")
  (:use :common-lisp
        :alexandria
        :command-line-arguments
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :uiop/image
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :auto-merge/core
        :swank)
  (:export :auto-merge :run-auto-merge))
(in-package :auto-merge/auto-merge)
(in-readtable :curry-compose-reader-macros)

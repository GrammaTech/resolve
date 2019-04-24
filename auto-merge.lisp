;;;; resolve.lisp -- Main automatic merge command-line driver
(defpackage :resolve/resolve
  (:documentation "Main resolve command-line driver")
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
        :resolve/core
        :swank)
  (:export :resolve :run-resolve))
(in-package :resolve/resolve)
(in-readtable :curry-compose-reader-macros)


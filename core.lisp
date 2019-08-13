;;;; core.lisp -- Core software search replace implementation.
(defpackage :resolve/core
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :split-sequence
        :software-evolution-library/utility
        :software-evolution-library/software-evolution-library)
  (:export :+resolve-dir+
           :+resolve-version+
           :+resolve-branch+
           :define-pattern-matching-weakening-mutation))
(in-package :resolve/core)
(in-readtable :curry-compose-reader-macros)


;;;; RESOLVE Constants.
(defvar +resolve-dir+
  (pathname-directory
   #.(or *compile-file-truename*
         *load-truename*
         *default-pathname-defaults*))
  "Path to directory holding RESOLVE.")

(defvar +resolve-version+
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (current-git-commit +resolve-dir+))
  "Current version of the RESOLVE library.")

(defvar +resolve-branch+
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (current-git-branch +resolve-dir+))
  "Current branch of the RESOLVE library.")

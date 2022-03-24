;;;; core.lisp -- Core software search replace implementation.
(defpackage :resolve/core
  (:use :gt/full
        :software-evolution-library/utility/git)
  (:import-from :fare-quasiquote)
  (:export :+resolve-dir+
           :+resolve-version+
           :+resolve-branch+
           :define-pattern-matching-weakening-mutation
           :resolve-readtable))
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

(defreadtable resolve-readtable
  (:fuse
   :standard
   :curry-compose-reader-macros
   :fare-quasiquote))

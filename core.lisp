;;;; core.lisp -- Core software search replace implementation.
(defpackage :auto-merge/core
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :split-sequence
        :software-evolution-library/utility
        :software-evolution-library/software-evolution-library)
  (:export :+auto-merge-dir+
           :+auto-merge-version+
           :+auto-merge-branch+
           :define-pattern-matching-weakening-mutation))
(in-package :auto-merge/core)
(in-readtable :curry-compose-reader-macros)


;;;; AUTO-MERGE Constants.
(defvar +auto-merge-dir+
  (pathname-directory
   #.(or *compile-file-truename*
         *load-truename*
         *default-pathname-defaults*))
  "Path to directory holding AUTO-MERGE.")

(defvar +limit-program-path+
  (make-pathname :name "limit"
                 :directory (append +auto-merge-dir+ (list "bin")))
  "Path to a program used to limit resources of subsequent commands.")

(defvar +auto-merge-version+
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (current-git-commit +auto-merge-dir+))
  "Current version of the AUTO-MERGE library.")

(defvar +auto-merge-branch+
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (current-git-branch +auto-merge-dir+))
  "Current branch of the AUTO-MERGE library.")

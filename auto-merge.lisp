;;;; auto-merge.lisp -- Main automatic merge command-line driver
(defpackage :resolve/auto-merge
  (:documentation "Main resolve command-line driver")
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :uiop
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :resolve/core
        :resolve/ast-diff
        :resolve/alist
        :resolve/software/project
        :resolve/software/parseable
        :resolve/software/lisp)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :resolve :run-resolve))
(in-package :resolve/auto-merge)
(in-readtable :curry-compose-reader-macros)

(defgeneric populate (merged unstable)
  (:documentation "Build a population from MERGED and UNSTABLE chunks.
NOTE: this is exponential in the size of UNSTABLE.")
  (:method ((merged software) unstable &aux (pop merged))
    (mapc (lambda (chunk)
            (setf pop
                  (mappend (lambda (el)
                             ;; TODO: New variants for each possible resolution:
                             ;; 1. mine
                             ;; 2. your
                             ;; 3. mine+your
                             ;; 4. your+mine
                             ;; 5. neither
                             el)
                           pop)))
          unstable)
    pop))

(defgeneric resolve (my old your test &key &allow-other-keys)
  (:documentation
   "Resolve merge conflicts between software versions MY OLD and YOUR.")
  (:method (test (my software) (old software) (your software)
            &key &allow-other-keys)
    (let ((*population* (multiple-value-call #'populate
                          (apply #'converge (list my old your)))))
      (evolve test))))

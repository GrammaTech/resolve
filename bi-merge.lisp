;;;; bi-merge.lisp -- command line interface for three way merging
(defpackage :bug-injector/bi-merge
  (:nicknames :bi/bi-merge)
  (:documentation "Command line interface for three way merging of software")
  (:use :common-lisp
        :alexandria
        :command-line-arguments
        :metabang-bind
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :cl-store
        :bordeaux-threads
        :uiop/filesystem
        :software-evolution-library/utility
        :software-evolution-library
        :trace-db
        :bug-injector/utility
        :bug-injector/weakening-mutators
        :bug-injector/scion
        :bug-injector/scions/clang
        :bug-injector/scions/java)
  (:export :bi-merge :run-bi-merge))
(in-package :bi/bi-merge)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +bi-merge-command-line-options+
    (append '((("language" #\L) :type string :initial-value "c"
               :documentation "language of input files (e.g. c, c++, or java)"))
            +common-command-line-options+)
    "Command line options for bi-merge"))

(define-bi-command bi-merge
    (original version1 version2
              &spec +bi-merge-command-line-options+
              &aux original-soft version1-soft version2-soft project-name
              software-store)
  "Merge two variants of an original software"
  (declare (ignorable help num-threads interactive manual swank profile
                      load eval save-original read-seed save-seed quiet
                      verbose))
  (setf original (truename original)
        version1 (truename version1)
        version2 (truename version2)
        out-dir (or out-dir (resolve-out-dir-from-source original))
        project-name (resolve-name-from-source original)
        software-store
        (unless no-store-software
          (resolve-store-path-from-out-dir-and-name out-dir project-name)))

  (note 1  "Parameters:~%~S~%"
        `(;; (+bi--version+ . ,+bug-injector-version+)
          (original . ,original)
          (version1 . ,version1)
          (version2 . ,version2)
          (out-dir . ,out-dir)
          (*note-level* . ,*note-level*)))

  (note 1 "Creating software objects after processing options.")
  (flet ((%create (s)
           (create-software s
                            :store-path software-store
                            :language language
                            :compiler compiler
                            :flags flags
                            :build-command build-command
                            :artifacts artifacts
                            :compilation-database compilation-database)))
    (setf original-soft (%create original))
    (setf version1-soft (%create version1))
    (setf version2-soft (%create version2))

    ;; Create styled versions of the input files
    (save-styled-to original-soft out-dir "original")
    (save-styled-to version1-soft out-dir "v1")
    (save-styled-to version2-soft out-dir "v2")

    ;; Perform merge

    (multiple-value-bind (new-merged unstable)
        (sel/ast-diff:converge original-soft version1-soft version2-soft)
      (save-styled-to new-merged out-dir "merged")
      (if (not unstable)
          (format t "No merge conflicts~%")
          (format t "Merge conflicts:~%~a~%" unstable)))))

(defun save-styled-to (soft out-dir sub)
  (let ((dest (get-output-path soft out-dir sub)))
    (unless (probe-file dest)
      (to-file (format-genome (copy soft)) dest))))

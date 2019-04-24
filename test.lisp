;;;; test.lisp --- Tests for software search and replace.
(defpackage :resolve/test
  (:use :common-lisp
        :alexandria
        :metabang-resolvend
        :iterate
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :split-sequence
        :trivia
        :software-evolution-library/utility
        :software-evolution-library/stefil-plus
        :resolve/core
        :resolve/resolve)
  (:shadowing-import-from :arrow-macros :<>)
  (:shadowing-import-from :trivia :match :guard)
  (:export :test :batch-test))
(in-package :resolve/test)
(in-readtable :curry-compose-reader-macros)

#-gt
(progn
  (defvar *success* nil "Variable indicating test success or failure.")
  (defun batch-test (test project branch &optional args)
    "Run tests in 'batch' mode, printing results as a string."
    (declare (ignorable project branch args))

    (let* ((stefil::*test-progress-print-right-margin* (expt 2 20))
           (failures (coerce (stefil::failure-descriptions-of
                              (without-debugging (funcall test)))
                             'list)))
      (setf *success*
            (if failures
                (prog1 nil
                  (format *error-output* "FAILURES~%")
                  (mapc [{format *error-output* "  ~a~%"}
                         #'stefil::name-of
                         #'stefil::test-of
                         #'car #'stefil::test-context-backtrace-of]
                        failures))
                (prog1 t
                  (format *error-output* "SUCCESS~%")))))))

(defun run-batch (&rest a)
  (declare (ignorable a))
  (batch-test #'test "RESOLVE" +resolve-branch+))

(defroot test)

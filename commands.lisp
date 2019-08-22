;;; commands.lisp --- Calculate and render AST diffs at the command line
;;;
;;; The following git configuration will register ast-diff as a tool
;;; to be used with @code{git difftool} (see
;;; @url{https://git-scm.com/docs/git-difftool}).
;;;
;;;     # Set ast-diff as the default difftool.
;;;     [diff]
;;;     	tool = ast-diff
;;;
;;;     # Command-line to use with ast-diff.  Piping through
;;;     # colordiff is optional but nice to highlight diffs.
;;;     [difftool "ast-diff"]
;;;     	cmd = "ast-diff $LOCAL $REMOTE|colordiff"
;;;
;;;     # Optionally don't prompt.
;;;     [difftool]
;;;     	prompt = false
;;;
;;; For clang language differences and merges, to help clang resolve
;;; includes, it may be necessary to add include paths to the
;;; invocation of ast-diff.  E.g., putting the following in the
;;; .git/config of a particular repo with headers in a "src/"
;;; subdirectory will ensure clang can find those headers.  (By
;;; default -I takes "." passing the working directory to clang.)
;;;
;;;     [difftool "ast-diff"]
;;;     	cmd = "ast-diff -I .,src $LOCAL $REMOTE"
;;;
;;; @texi{ast-diff-commands}
(defpackage :resolve/commands
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :uiop/pathname
        :resolve/core
        :resolve/ast-diff
        :resolve/auto-merge
        :resolve/software/parseable
        :resolve/software/project
        :resolve/software/lisp
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/software/ast
        :software-evolution-library/software/simple
        :software-evolution-library/software/project
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/java
        :software-evolution-library/software/java-project
        :software-evolution-library/software/javascript
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/json
        :software-evolution-library/software/lisp
        :software-evolution-library/components/test-suite)
  (:import-from :uiop :writeln :truenamize :nest)
  (:shadow :merge :ast-diff)
  (:export :ast-diff :ast-merge))
(in-package :resolve/commands)
(in-readtable :curry-compose-reader-macros)
;;; TODO: Work on clang-diff tool's git configuration (or maybe this
;;;       has to be implemented in clang-diff itself) to limit
;;;       application by extension.


;;; Command-line interface to ast differencing.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append +common-command-line-options+
            +interactive-command-line-options+
            +clang-command-line-options+
            +project-command-line-options+
            +clang-project-command-line-options+
            `((("language" #\L) :type string :optional t
               :documentation "language to use for source")
              (("raw" #\r) :type boolean :optional t
               :documentation "output diff as raw ASTs (default is as text)")
              (("no-color" #\C) :type boolean :optional t
               :documentation "inhibit color printing")
              (("edit-tree" #\T) :type boolean :optional t
               :documentation "Print edit tree")
              (("print-asts") :type boolean :optional t
               :documentation
               "Also print a representation of the edit tree ASTs")
              (("strings" #\S) :type boolean :optional t
               :documentation "Diff descends into AST leaf strings")
              (("coherence") :type string :optional t
               :documentation
               "Bound used to find relevant nodes in the edit tree")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +ast-merge-only-command-line-options+
    `((("conflict") :type boolean :optional t
       :documentation
       "Generate conflict nodes rather than resolving conflicts"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +auto-merge-only-command-line-options+
    `((("evolve") :type boolean :optional t :initial-value nil
       :documentation "attempt to use evolution to resolve conflicts")
      (("num-tests") :type integer :optional t :initial-value 1
       :documentation "number of test cases to execute"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun argument-multiplier (&rest multipliers)
    "Return a function to multiply command-line arguments across MULTIPLIERS.
Every element of MULTIPLIERS results in a another multiple of the
command-line options processed by the returned function."
    (lambda (arg-spec)
      (destructuring-bind
            (name &key type optional initial-value action documentation)
          arg-spec
        (mapcar (lambda (which)
                  (append
                   (list (list (concatenate 'string which "-" (car name))))
                   (when type (list :type type))
                   (when optional (list :optional optional))
                   (when action (list :action action))
                   (when initial-value (list :initial-value initial-value))
                   (nest
                    (when documentation)
                    (list :documentation)
                    (concatenate 'string documentation " for " which " file"))))
                multipliers))))
  (nest
   (defparameter +ast-diff-command-line-options+)
   (append +command-line-options+)
   (mappend (argument-multiplier "old" "new"))
   (append +clang-command-line-options+
           +project-command-line-options+
           +clang-project-command-line-options+))

  (nest
   (defparameter +ast-merge-command-line-options+)
   (append +command-line-options+)
   (append +ast-merge-only-command-line-options+)
   (mappend (argument-multiplier "my" "old" "your"))
   (append +clang-command-line-options+
           +project-command-line-options+
           +clang-project-command-line-options+))

  (nest
   (defparameter +auto-merge-command-line-options+)
   (append +command-line-options+
           +evolutionary-command-line-options+)
   (append +auto-merge-only-command-line-options+)
   (mappend (argument-multiplier "my" "old" "your"))
   (append +clang-command-line-options+
           +project-command-line-options+
           +clang-project-command-line-options+)))

(defmacro expand-options-for-which-files (language which)
  "Expand the options for WHICH calling `create-software' appropriately."
  `(create-software
    ,@(let ((pairs (mapcar
                    «list [#'intern {concatenate 'string which "-"}
                                    #'symbol-name]
                          #'identity»
                    '(file compiler flags build-command
                      artifacts compilation-database))))
        (list* (caar pairs)
               :language language
               (mappend «list [#'make-keyword #'second] {cons 'or}»
                        (cdr pairs))))))

(defmacro drop-dead-date ((&key (times 100) (cutoff (random times))
                                (day 1) (month 1) (year 2020))
                          &body body)
  ;; TODO: Also break if before a certain date.
  (with-gensyms (this that random-form-arg)
    (labels ((plus-or-times () (if (zerop (random 2)) '+ '*))
             (many-plus-or-times (counter base)
               (if (zerop counter)
                   base
                   `(,(plus-or-times) ,(1+ (random 20))
                      ,(many-plus-or-times (- counter 1) base)))))
      (let ((randoms-list (loop :for i :below times :collect (1+ (random 100))))
            (random-form (many-plus-or-times 20 random-form-arg)))
        `(let ((,this (get-universal-time)))
           ,@(loop :for i :from 0 :below cutoff :collect `(incf ,this ,(elt randoms-list i)))
           (let ((,that ,(+ (encode-universal-time 0 0 0 day month year) (reduce #'+ randoms-list))))
             ,@(loop :for i :from cutoff :below times :collect `(incf ,this ,(elt randoms-list i)))
             (setf ,this (funcall (lambda (,random-form-arg) ,random-form) ,this))
             (when (> ,this ,(mapt (lambda (el) (if (eql el random-form-arg) that el)) random-form))
               ,@body)))))))

#+drop-dead
(progn
  (defmacro drop-dead-method (method-name)
    (cons 'progn
          (mapcar
           (lambda (method)
             `(defmethod ,method-name ,(if (zerop (random 2)) :before :after)
                ,(closer-mop:method-lambda-list method)
                (drop-dead-date () (quit))))
           (closer-mop:generic-function-methods (eval `(function ,method-name))))))
  (defmacro drop-dead-method-all ()
    (cons 'progn (mapcar (lambda (method) `(drop-dead-method ,method))
                         '(genome from-file mutate print-object create-edit-tree
                           map-edit-tree ast-patch merge-diffs-on-syms))))
  (drop-dead-method-all))

(define-command ast-diff (old-file new-file &spec +ast-diff-command-line-options+)
  "Compare source code in OLD-FILE and NEW-FILE by AST."
  #.(format nil
            "~%Built from SEL ~a, Resolve ~a, and ~a ~a on ~a.~%"
            +software-evolution-library-version+
            +resolve-version+
            (lisp-implementation-type) (lisp-implementation-version)
            (multiple-value-bind (second minute hour date month year)
                (get-decoded-time)
              (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                      year month date hour minute second)))
  (declare (ignorable quiet verbose split-lines
                      old-split-lines new-split-lines))
  #+drop-dead
  (drop-dead-date ()
    (exit-command ast-diff 2
                  (progn (format *error-output* "Software no longer valid.~%")
                         (finish-output *error-output*)
                         (quit 2))))
  (when help (show-help-for-ast-diff))
  (setf *note-out* (list *error-output*))
  (unless (every #'resolve-file (list old-file new-file))
    (exit-command ast-diff 2 (error "Missing source.")))
  (unless language
    (setf language (guess-language old-file new-file)))
  ;; Create the diff.

  (let* ((old-sw (expand-options-for-which-files language "OLD"))
         (new-sw (expand-options-for-which-files language "NEW"))
         (softwares (list old-sw new-sw))
         (diff (resolve/ast-diff:ast-diff old-sw new-sw :strings strings)))

    ;; Print according to the RAW option.
    (cond
      (raw (writeln (ast-diff-elide-same diff) :readably t))
      (edit-tree
       (when coherence
         (let ((n (let ((*read-eval* nil))
                    (read-from-string coherence))))
           (unless (typep n '(real 0 1))
             (error "coherence must be a number in range [0.0,1.0]"))
           (setf coherence n)))
       (create-and-print-edit-tree
        softwares diff
        :print-asts print-asts
        :coherence coherence))
      (t (print-diff diff :no-color no-color)))
    ;; Only exit with 0 if the two inputs match.
    (wait-on-manual manual)
    (exit-command ast-diff
                  (if (every [{eql :same} #'car] diff) 0 1)
                  diff)))

(define-command ast-merge (my-file old-file your-file
                                   &spec +ast-merge-command-line-options+)
  "Merge changes from old-file->my-file and old-file->your-file."
  #.(format nil
            "~%Built from SEL ~a, Resolve ~a, and ~a ~a on ~a.~%"
            +software-evolution-library-version+
            +resolve-version+
            (lisp-implementation-type) (lisp-implementation-version)
            (multiple-value-bind (second minute hour date month year)
                (get-decoded-time)
              (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                      year month date hour minute second)))
  (declare (ignorable quiet verbose raw no-color edit-tree
                      print-asts coherence split-lines
                      my-split-lines your-split-lines old-split-lines))
  #+drop-dead
  (drop-dead-date ()
    (exit-command ast-merge 2
                  (progn (format *error-output* "Software no longer valid.~%")
                         (finish-output *error-output*)
                         (quit 2))))
  (when help (show-help-for-ast-merge))
  (setf *note-out* (list *error-output*))
  (unless (every #'resolve-file (list old-file my-file your-file))
    (exit-command ast-merge 2 (error "Missing source.")))
  (setf old-file (truenamize old-file)
	my-file (truenamize my-file)
	your-file (truenamize your-file)
        language (or language (guess-language old-file my-file your-file)))
  ;; Force OUT-DIR when running as a command line utility and merging
  ;; whole directories.  We can't write multiple files to STDOUT.
  (when (and (not uiop/image:*lisp-interaction*)
             (not out-dir)
             (directory-p old-file))
    (setf out-dir (resolve-out-dir-from-source old-file))
    (note 0 "Merging directories, out-dir set to ~a." out-dir))
  ;; Don't write notes when we're writing merge results to STDOUT.
  (unless out-dir (setf *note-level* 0))

  (multiple-value-bind (new-merged unstable)
      (converge
       (expand-options-for-which-files language "MY")
       (expand-options-for-which-files language "OLD")
       (expand-options-for-which-files language "YOUR")
       :strings strings
       :conflict conflict)
    ;; Write the result, either to out-dir or to STDOUT.
    (if out-dir
        (if (directory-p old-file)
            (to-file new-merged
                     (make-pathname
                      :directory (append out-dir (list "merged"))))
            (to-file new-merged
                     (resolve-store-path-from-out-dir-and-name
                      out-dir
                      (pathname-name old-file) "merged"
                      (pathname-type old-file))))
        (genome-string new-merged *standard-output*))

    (wait-on-manual manual)
    (exit-command ast-merge (if unstable 1 0) new-merged)))

(defmethod test ((obj software) (tests test-suite))
  "Determine the fitness of OBJ against TESTS."
  (with-temp-file (bin)
    (if (ignore-phenome-errors (phenome obj :bin bin))
        (mapcar (lambda (test-case)
                  (nth-value 2 (run-test bin test-case)))
                (test-cases tests))
        (make-list (length (test-cases tests))
                   :initial-element most-positive-fixnum))))

(define-command auto-merge (my-file old-file your-file test-script
                                    &spec +auto-merge-command-line-options+
                                    &aux tests)
  "Merge MY-FILE and YOUR-FILE, from OLD-FILE, with TEST-SCRIPT.

* MY-FILE, YOUR-FILE, OLD-FILE
  Files or projects to compare and merge.
* TEST-SCRIPT
  The command line utilized to evaluate the success of the merge.  If substring
  \"~~a\" is present it will be replaced with the name of the executable.
  If substring \"~~d\" is present it will be replaced with the test number
  and the test script will be invoked NUM-TESTS times for values 0 to
  NUM-TESTS - 1."
  #.(format nil "~%Built from SEL ~a, Resolve ~a, and ~a ~a on ~a.~%"
            +software-evolution-library-version+
            +resolve-version+
            (lisp-implementation-type) (lisp-implementation-version)
            (multiple-value-bind (second minute hour date month year)
                (get-decoded-time)
              (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                      year month date hour minute second)))
  (declare (ignorable manual quiet verbose raw no-color edit-tree
                      print-asts coherence strings split-lines
                      my-split-lines your-split-lines old-split-lines))
  #+drop-dead
  (drop-dead-date ()
    (exit-command auto-merge 2
                  (progn (format *error-output* "Software no longer valid.~%")
                         (finish-output *error-output*)
                         (quit 2))))
  (when help (show-help-for-ast-merge))
  (unless (every #'resolve-file (list old-file my-file your-file))
    (exit-command auto-merge 2 (error "Missing source.")))
  (setf out-dir (or out-dir (resolve-out-dir-from-source old-file))
        old-file (truenamize old-file)
        my-file (truenamize my-file)
        your-file (truenamize your-file)
        language (or language (guess-language old-file my-file your-file))
        num-tests (resolve-num-tests-from-num-tests num-tests)
        tests (create-test-suite test-script num-tests))
  (to-file (apply #'resolve
                  (expand-options-for-which-files language "MY")
                  (expand-options-for-which-files language "OLD")
                  (expand-options-for-which-files language "YOUR")
                  {test _ tests}
                  (append (when evolve (list :evolve? evolve))
                          (when max-evals (list :max-evals max-evals))
                          (when max-time (list :max-time max-time))))
           (if (directory-pathname-p my-file)
               (make-pathname :directory (append out-dir (list "auto-merged")))
               (make-pathname :directory out-dir
                              :name "auto-merged"
                              :type (pathname-type my-file)))))

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
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :resolve/core
        :resolve/ast-diff
        :resolve/auto-merge
        :resolve/software/parseable
        :resolve/software/project
        :resolve/software/lisp
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
  (defun argument-multiplier (&rest multipliers)
    "Return a function to multiply command-line arguments across MULTIPLIERS.
Every element of MULTIPLIERS results in a another multiple of the
command-line options processed by the returned function."
    (lambda (arg-spec)
      (destructuring-bind
            ((long short) &key type optional initial-value action documentation)
          arg-spec
        (declare (ignorable short))
        (mapcar (lambda (which)
                  (append
                   (list (list (concatenate 'string which "-" long)))
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
   (mappend (argument-multiplier "my" "old" "your"))
   (append +clang-command-line-options+
           +project-command-line-options+
           +clang-project-command-line-options+))

  (nest
   (defparameter +auto-merge-command-line-options+)
   (append +command-line-options+
           +evolutionary-command-line-options+)
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

(define-command ast-diff (old-file new-file &spec +ast-diff-command-line-options+)
  "Compare source code in OLD-FILE and NEW-FILE by AST."
  #.(format nil
            "~%Built from SEL ~a, Resolve ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            +resolve-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
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
            "~%Built from SEL ~a, Resolve ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            +resolve-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose raw no-color edit-tree
                      print-asts coherence))
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
  "Direct fitness of OBJ against TESTS.
If the tests fail then infinity, otherwise diversity."
  (with-temp-file (bin)
    (if (ignore-phenome-errors (phenome obj :bin bin))
        (evaluate bin tests)
        0)))

(define-command auto-merge (my-file old-file your-file test-script
                                    &spec +auto-merge-command-line-options+
                                    &aux tests)
  "Merge MY-FILE and YOUR-FILE, from OLD-FILE, with TEST-SCRIPT passing."
  #.(format nil "~%Built from SEL ~a, Resolve ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            +resolve-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable manual quiet verbose raw no-color edit-tree
                      print-asts coherence strings))
  (when help (show-help-for-ast-merge))
  (unless (every #'resolve-file (list old-file my-file your-file))
    (exit-command auto-merge 2 (error "Missing source.")))
  (setf out-dir (resolve-out-dir-from-source old-file)
        old-file (truenamize old-file)
	my-file (truenamize my-file)
	your-file (truenamize your-file)
        language (or language (guess-language old-file my-file your-file))
        tests (create-test test-script))
  (to-file (apply #'resolve
                  (expand-options-for-which-files language "MY")
                  (expand-options-for-which-files language "OLD")
                  (expand-options-for-which-files language "YOUR")
                  {test _ tests}
                  (append (when max-evals (list max-evals))
                          (when max-time (list max-time))))
           (make-pathname :directory (append out-dir (list "auto-merged")))))

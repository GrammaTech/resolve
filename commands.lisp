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
(defpackage :software-evolution-library/ast-diff/commands
  (:nicknames :sel/ast-diff/commands
              :software-evolution-library/ast-diff/commands)
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        ;; TODO: Drop next two when importing compilation-db functions.
        :split-sequence
        :cl-ppcre
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/ast-diff/ast-diff
        :software-evolution-library/software/ast
        :software-evolution-library/software/simple
        :software-evolution-library/software/project
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/java
        :software-evolution-library/software/java-project
        :software-evolution-library/software/javascript
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/lisp)
  (:import-from :uiop :writeln :truenamize :absolute-pathname-p :nest)
  (:shadow :merge :ast-diff)
  (:export :ast-diff :ast-merge))
(in-package :software-evolution-library/ast-diff/commands)
(in-readtable :curry-compose-reader-macros)
;;; TODO: Work on clang-diff tool's git configuration (or maybe this
;;;       has to be implemented in clang-diff itself) to limit
;;;       application by extension.

(defun guess-language-from (&rest sources)
  (flet ((%guess-language-from (source)
           (unless (directory-p source)
             (second (find-if (lambda (pair)
                                (member (pathname-type source) (car pair)
                                        :test #'equalp))
                              '((("lisp") lisp)
                                (("java") java)
                                (("js") javascript)
                                (("c" "cpp" "h" "hpp" "cc" "cxx" "hxx")
                                 clang)))))))
    (let ((guesses (mapcar #'%guess-language-from sources)))
      (when (= 1 (length (remove-duplicates guesses)))
        (car guesses)))))


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
               :documentation "inhibit color printing")))))

;;; Next two taken from clang-project.lisp.  Should be exported there
;;; once we are able to remove the `project-path' nonsense.
(defun compilation-db-entry-compiler (entry)
  "Return the compiler in the compilation database ENTRY."
  (or (first (split-sequence #\Space (aget :command entry)))
      (first (aget :arguments entry))))

(defun compilation-db-entry-flags (entry)
  "Return the compiler flags in the compilation database ENTRY."
  (flet ((flag-list-helper (entry)
           "Get a massaged list of compiler flags from the ENTRY."
           (nest
            (mappend (lambda (arg) ; Split leading "-L".
                       (split-sequence #\Space
                         (replace-all arg "-L" "-L ")
                         :remove-empty-subseqs t)))
            (mappend (lambda (arg) ; Split leading "-I".
                       (split-sequence #\Space
                         (replace-all arg "-I" "-I ")
                         :remove-empty-subseqs t)))
            (or (mapcar (lambda (arg) ; Wrap quotes for the shell.
                          (regex-replace
                           "\"([^\"]*)\"" arg "'\"\\1\"'"))
                        ;; Drop the first element of
                        ;; arguments which is the compiler.
                        (cdr (aget :arguments entry)))
                (cdr (split-sequence
                         #\Space (or (aget :command entry) "")
                         :remove-empty-subseqs t))))))
    (nest
     ;; Remove the file being built from the flags.
     (remove-if {equalp (aget :file entry)})
     (iter (for f in (flag-list-helper entry))
           (for p previous f)
           (collect (if (or (string= p "-I") (string= p "-L"))
                        ;; Ensure include/library paths
                        ;; point to the correct location
                        ;; and not a temporary build directory.
                        (if (absolute-pathname-p f)
                            f
                            (merge-pathnames-as-directory
                             (make-pathname :directory
                                            (aget :directory entry))
                             (make-pathname :directory
                                            (list :relative f))))
                        ;; Pass the flag thru.
                        f))))))

(defun create-software-from
    (path language
     &optional flags compiler compilation-database build-command)
  "Build software object of type LANGUAGE from PATH."
  (from-file (apply
              #'make-instance
              language
              (append
               (when flags
                 (list :flags flags))
               (when (and compiler (member language '(clang #| clang-project |#)))
                 (list :compiler compiler))
               (when compilation-database
                 (case language
                   (clang-project
                    (list :compilation-database compilation-database))
                   (clang
                    (list :compiler (compilation-db-entry-compiler
                                     (first compilation-database))
                          :flags (compilation-db-entry-flags
                                  (first compilation-database))))
                   (t
                    (error "language ~s doesn't support a compilation database"
                           language))))
               (when (and compiler (subtypep language 'project))
                 (list :build-command build-command))))
             path))

(define-command ast-diff (source1 source2 &spec +command-line-options+)
  "Compare source code in SOURCE1 and SOURCE2 by AST."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (when help (show-help-for-ast-diff))
  (setf *note-out* (list *error-output*))
  (unless (every #'resolve-file (list source1 source2))
    (exit-command ast-diff 2 (error "Missing source.")))
  (unless language
    (setf language (guess-language-from source1 source2)))
  ;; Create the diff.
  (let ((diff
         (apply #'sel/ast-diff/ast-diff:ast-diff
                (mapcar
                 {create-software-from
                  _ language flags compiler compilation-database build-command}
                 (list source1 source2)))))
    ;; Print according to the RAW option.
    (if raw
        (writeln (ast-diff-elide-same diff) :readably t)
        (if no-color
            (print-diff diff :no-color t)
            (print-diff diff)))
    ;; Only exit with 0 if the two inputs match.
    (wait-on-manual manual)
    (exit-command ast-diff
                  (if (every [{eql :same} #'car] diff) 0 1)
                  diff)))

(define-command ast-merge (my-file old-file your-file
                                   &spec +command-line-options+)
  "Merge changes from old-file->my-file and old-file->your-file."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose raw no-color))
  (when help (show-help-for-ast-merge))
  (setf *note-out* (list *error-output*))
  (unless (every #'resolve-file (list old-file my-file your-file))
    (exit-command ast-merge 2 (error "Missing source.")))
  (setf old-file (truenamize old-file)
	my-file (truenamize my-file)
	your-file (truenamize your-file)
        language (or language (guess-language-from old-file my-file your-file)))
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
      (apply #'converge
	     (mapcar
	      {create-software-from
	       _ language flags compiler compilation-database build-command}
	      (list old-file my-file your-file)))
    ;; Write the result, either to out-dir or to STDOUT.
    (if out-dir
        (if (directory-p old-file)
            (to-file new-merged
                     (make-pathname :directory (append out-dir "merged")))
	    (to-file new-merged
                     (resolve-store-path-from-out-dir-and-name
                      out-dir
                      (pathname-name old-file) "merged"
                      (pathname-type old-file))))
        (genome-string new-merged *standard-output*))

    (wait-on-manual manual)
    (exit-command ast-merge (if unstable 1 0) new-merged)))
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
        :resolve/software/auto-mergeable
        :resolve/software/parseable
        :resolve/software/project
        :resolve/software/lisp
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/command-line-rest
        :software-evolution-library/software/parseable
        :software-evolution-library/software/simple
        :software-evolution-library/software/project
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/java
        :software-evolution-library/software/java-project
        :software-evolution-library/software/javascript
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/json
        :software-evolution-library/software/lisp)
  (:shadowing-import-from :software-evolution-library/view
                          :+color-RED+ :+color-GRN+ :+color-CYA+ :+color-RST+)
  (:import-from :spinneret :with-html)
  (:import-from :cl-ppcre :scan)
  (:import-from :split-sequence :split-sequence)
  (:import-from :uiop :nest)
  (:import-from :uiop/stream :println :writeln)
  (:import-from :uiop/filesystem :truenamize)
  (:import-from :md5 :md5sum-string)
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
            `((("profile" #\P) :type string
               :action #'pathname
               :documentation "profile and write report to FILE")
              (("strings" #\S) :type boolean :optional t
               :documentation "Diff descends into AST leaf strings")
              (("wrap" #\W) :type boolean :optional t
               :documentation
               "diff searches for wrap/unwrap operations")
              (("wrap-sequences") :type boolean :optional t
               :documentation
               "diff searches for wrap/unwrap operations on sequences of AST nodes")
              (("max-wrap") :type integer
               :initial-value #.*max-wrap-diff*
               :documentation "max size diff of wrapping/unwrapping")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +ast-diff-and-merge-only-command-line-options+ nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +ast-diff-only-command-line-options+
    `((("raw" #\r) :type boolean :optional t
       :documentation "output diff as raw ASTs (default is as text)")
      (("no-color" #\C) :type boolean :optional t
       :documentation "inhibit color printing")
      (("edit-tree" #\T) :type boolean :optional t
       :documentation "Print edit tree")
      (("json" #\J):type boolean :optional t
       :documentation "Print results as JSON")
      (("unified" #\U) :type integer :initial-value 3
       :documentation "output NUM lines of unified context")
      (("print-asts") :type boolean :optional t
       :documentation
       "Also print a representation of the edit tree ASTs")
      (("coherence") :type string :optional t
       :documentation
       "Bound used to find relevant nodes in the edit tree")
      (("base-cost") :type integer :initial-value #.*base-cost*
       :documentation "Base edit operation cost"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +ast-merge-only-command-line-options+
    `((("conflict") :type boolean :optional t
       :documentation
       "Generate conflict nodes rather than resolving conflicts")
      (("base-cost") :type integer :initial-value #.*base-cost*
       :documentation "Base edit operation cost"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +auto-merge-only-command-line-options+
    `((("evolve") :type boolean :optional t :initial-value nil
       :documentation "attempt to use evolution to resolve conflicts")
      (("num-threads" #\n) :type integer :initial-value 1
       :documentation "number of threads to utilize")
      (("num-tests") :type integer :optional t :initial-value 1
       :documentation "number of test cases to execute")
      (("base-cost") :type integer :initial-value 10
       :documentation "Base edit operation cost")
      (("randomize") :type boolean :optional t
       :documentation "Utilize a non-fixed random seed"))))

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
   (append +ast-diff-only-command-line-options+)
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
               :ignore-other-paths '(list #P"compile_commands.json")
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

(define-command-rest ast-diff
    (old-file new-file &spec +ast-diff-command-line-options+
              &aux diff old-file-temp-path new-file-temp-path)
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
  (declare (ignorable quiet verbose))
  #+drop-dead
  (drop-dead-date ()
    (exit-command ast-diff 2
                  (progn (format *error-output* "Software no longer valid.~%")
                         (finish-output *error-output*)
                         (quit 2))))
  (when help (show-help-for-ast-diff))
  (setf *note-out* (list *error-output*))
  (unwind-protect
       (progn
         (if (and (find #\Newline old-file) (find #\Newline new-file))
             ;; We have text with newlines so it is probably the raw text.
             (progn
               (unless language
                 (error "Must specify language when differencing strings."))
               (setf language
                     (resolve-language-from-language-and-source language))
               (let ((type (case language
                             (java "java")
                             (javascript "js")
                             (json "json")
                             (clang "cxx")
                             (lisp "lisp")
                             (simple "txt"))))
                 (setf old-file-temp-path (temp-file-name type)
                       new-file-temp-path (temp-file-name type))
                 (string-to-file old-file old-file-temp-path)
                 (string-to-file new-file new-file-temp-path)
                 (setf old-file old-file-temp-path
                       new-file new-file-temp-path)))
             ;; We have paths to resources, files, directories, or repositories.
             (progn
               (unless (every #'resolve-file (list old-file new-file))
                 (exit-command ast-diff 2 (error "Missing source.")))
               (unless language
                 (setf language (guess-language old-file new-file)))))
         (with-prof profile
         ;; Create the diff.
         (let* ((old-sw (expand-options-for-which-files language "OLD"))
                (new-sw (expand-options-for-which-files language "NEW"))
                (softwares (list old-sw new-sw)))
           (setf diff (resolve/ast-diff:ast-diff
                       old-sw new-sw
                       :wrap wrap :max-wrap-diff max-wrap
                       :wrap-sequences wrap-sequences
                       :base-cost base-cost
                       :strings strings))
           ;; Special handling for SIMPLE diffs, which don't have newlines.
           (when (and (eql 'simple (type-of old-sw))
                      (eql 'simple (type-of new-sw)))
             (setf diff
                   (mapt (lambda (element)
                           (if (stringp element)
                               (concatenate 'string element (list #\Newline))
                               element))
                         diff)))
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
             (json (println (encode-json-to-string
                             (mapcar «list #'car #'cdr» diff))))
             (t
              (if (zerop unified)
                  (print-diff diff :no-color no-color)
                  (let ((diff-lines
                         (split-sequence
                          #\Newline
                          (with-output-to-string (str)
                            (print-diff diff :no-color no-color :stream str))))
                        (in-diff-p nil)
                        (trailing-context 0)
                        (context-buffer nil)
                        (skipped-last-p nil)
                        (line-counter 0))
                    ;; TODO: Find a solution to noticing diff lines that doesn't
                    ;;       rely on patterns in the text.
                    (flet ((diff-start-p (line)
                             (if no-color
                                 (scan "({\\+|\\[-)" line)
                                 (or (search +color-GRN+ line)
                                     (search +color-RED+ line))))
                           (diff-end-p (line)
                             (if no-color
                                 (scan "(\\+}|-])" line)
                                 (search +color-RST+ line))))
                      ;; Print with a buffer of size UNIFIED
                      ;; before/after every diff line.
                      (dolist (line diff-lines)
                        (incf line-counter)
                        (cond
                          ((diff-start-p line)
                           (when skipped-last-p
                             (println
                              (format nil "~aline: ~d~a"
                                      (if no-color "" +color-CYA+)
                                      (- line-counter
                                         (min unified (length context-buffer)))
                                      (if no-color "" +color-RST+))))
                           (setf skipped-last-p nil)
                           (setf trailing-context unified)
                           (when context-buffer
                             (mapc #'println
                                   (nreverse (take unified context-buffer)))
                             (setf context-buffer nil))
                           (setf in-diff-p
                                 (let ((start-point (diff-start-p line))
                                       (end-point (diff-end-p line)))
                                   (or (not end-point)
                                       (< end-point start-point))))
                           (println line))
                          ((diff-end-p line)
                           (setf skipped-last-p nil)
                           (setf in-diff-p nil)
                           (println line))
                          (in-diff-p
                           (println line))
                          ((> trailing-context 0)
                           (decf trailing-context)
                           (println line))
                          (t (setf skipped-last-p t)
                             (push line context-buffer)))))))))
           ;; Only exit with 0 if the two inputs match.
           (wait-on-manual manual)))
         (exit-command ast-diff
                       (if (every [{eql :same} #'car] diff) 0 1)
                       diff))
    (when old-file-temp-path (delete-file old-file-temp-path))
    (when new-file-temp-path (delete-file new-file-temp-path))))

(defun md5string (text)
  (format nil "~{~X~}" (coerce (md5:md5sum-string text) 'list)))

(defvar *json-diffs* (make-hash-table :test #'equalp))

(defun rest-diff (&rest additional-ast-diff-params)
  (destructuring-bind (&key old-file new-file language link)
      (alist-plist (decode-json-from-string (payload-as-string)))
    (flet ((process (&rest rest)
             (let ((*lisp-interaction* t))
               (handler-case (apply #'ast-diff old-file new-file :language language
                                    rest)
                 (error (e) (http-condition 500 "Error: ~a" e))))))
      (if link
          (let* ((json (with-output-to-string (*standard-output*)
                         (process :json t)))
                 (key (md5string json)))
            (setf (gethash key *json-diffs*) json)
            (format t "http://~a~:[~;:~]~a/show/~a"
                    *address* *port* *port* key))
          (apply #'process additional-ast-diff-params)))))

(defroute diff (:post :application/json)
  (with-output-to-string (*standard-output*)
    (rest-diff :json t)))

(defroute diff (:post :text/plain)
  (with-output-to-string (*standard-output*)
    (rest-diff)
    (format t "~&")))

(defun render-json-diff-to-html (json)
  (with-output-to-string (*standard-output*)
    ;; HTML Header, CSS, and JavaScript to handle display.
    (with-html
        (:doctype)
      (:html (:head
              (:title "AST-Diff")
              (:script :id "diff" :type "text/json" json)
              (:script :type "text/javascript" :src "/javascript/ast-diff.js")
              (:link :rel "stylesheet" :href "/css/ast-diff.css"))
             (:body :onload "renderDiff()" (:pre (:code :id "page")))))))

(defroute show (:get :text/html hash)
  (render-json-diff-to-html (gethash (symbol-name hash) *json-diffs* "\"MISSING DIFF\"")))

(defroute diff (:post :text/html)
  (render-json-diff-to-html (with-output-to-string (*standard-output*)
                              (rest-diff :json t))))

(define-constant +javascript-directory+
    (append (pathname-directory
             #.(or *compile-file-truename*
                   *load-truename*
                   *default-pathname-defaults*))
            (list "js"))
  :test #'equalp
  :documentation "Path to directory holding javascript files.")

(defroute javascript (:get :text/javascript filename)
  (file-to-string (make-pathname :name (string-downcase (symbol-name filename))
                                 :type "js"
                                 :directory +javascript-directory+)))

(define-constant +css-directory+
    (append (pathname-directory
             #.(or *compile-file-truename*
                   *load-truename*
                   *default-pathname-defaults*))
            (list "css"))
  :test #'equalp
  :documentation "Path to directory holding css files.")

(defroute css (:get :text/css filename)
  (file-to-string (make-pathname :name (string-downcase (symbol-name filename))
                                 :type "css"
                                 :directory +css-directory+)))

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
  (declare (ignorable quiet verbose))
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
  (when (and (directory-p old-file) (not out-dir))
    (setf out-dir (resolve-out-dir-from-source old-file))
    (note 0 "Merging directories, out-dir set to ~a."
          (namestring (make-pathname :directory out-dir))))
  ;; Don't write notes when we're writing merge results to STDOUT.
  (unless out-dir (setf *note-level* 0))

  (multiple-value-bind (new-merged unstable)
      (converge
       (expand-options-for-which-files language "MY")
       (expand-options-for-which-files language "OLD")
       (expand-options-for-which-files language "YOUR")
       :strings strings
       :conflict conflict
       :wrap wrap
       :wrap-sequences wrap-sequences
       :max-wrap-diff max-wrap
       :base-cost base-cost)
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
  (declare (ignorable manual quiet fault-loc))
  #+drop-dead
  (drop-dead-date ()
    (exit-command auto-merge 2
                  (progn (format *error-output* "Software no longer valid.~%")
                         (finish-output *error-output*)
                         (quit 2))))
  (when help (show-help-for-ast-merge))
  (unless (every #'resolve-file (list old-file my-file your-file))
    (exit-command auto-merge 2 (error "Missing source.")))
  (setf *random-state* (if randomize (make-random-state t) *random-state*)
        out-dir (or out-dir (resolve-out-dir-from-source old-file))
        old-file (truenamize old-file)
        my-file (truenamize my-file)
        your-file (truenamize your-file)
        language (or language (guess-language old-file my-file your-file))
        num-tests (resolve-num-tests-from-num-tests num-tests)
        tests (create-test-suite test-script num-tests))
  (to-file (apply #'resolve
                  (create-auto-mergeable
                   (expand-options-for-which-files language "MY"))
                  (create-auto-mergeable
                   (expand-options-for-which-files language "OLD"))
                  (create-auto-mergeable
                   (expand-options-for-which-files language "YOUR"))
                  {auto-merge-test _ tests}
                  :num-threads num-threads
                  :strings strings
                  :base-cost base-cost
                  :wrap wrap
                  :wrap-sequences wrap-sequences
                  :max-wrap-diff max-wrap
                  (append (when evolve (list :evolve? evolve))
                          (when max-evals (list :max-evals max-evals))
                          (when max-time (list :max-time max-time))))
           (if (directory-pathname-p my-file)
               (make-pathname :directory (append out-dir (list "auto-merged")))
               (make-pathname :directory out-dir
                              :name "auto-merged"
                              :type (pathname-type my-file)))))

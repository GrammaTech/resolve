;;; commands.lisp --- Calculate and render AST diffs at the command line
;;;
;;; TODO: add documentation for auto-merge.
(defpackage :resolve/commands
  (:use :gt/full
        :resolve/core
        :resolve/ast-diff
        :resolve/auto-merge
        :resolve/software/auto-mergeable
        :resolve/software/parseable
        :resolve/software/project
        :resolve/software/lisp
        :software-evolution-library
        :software-evolution-library/utility/debug
        :software-evolution-library/command-line
        :software-evolution-library/command-line-rest
        :software-evolution-library/software/parseable
        :software-evolution-library/software/simple
        :software-evolution-library/software/project
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/lisp
        :software-evolution-library/software/lisp)
  (:local-nicknames (:ts :software-evolution-library/software/tree-sitter))
  (:shadow :css)
  (:shadowing-import-from :software-evolution-library/terminal
                          :+color-RED+ :+color-GRN+ :+color-CYA+ :+color-RST+)
  (:import-from :spinneret :with-html)
  (:import-from :uiop/stream :println :writeln)
  (:import-from :md5 :md5sum-string)
  (:shadow :merge)
  (:export :ast-merge
           :rest-diff
           :render-json-diff-to-html
           :lookup-json-diff-hash
           :+javascript-directory+))
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
      (("ignore-paths") :type string :optional t
       :initial-value "compile_commands.json"
       :action #'handle-comma-delimited-argument
       :documentation "Comma-delimited list of paths to ignore"))))

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

(defun mapt (function tree)
  "Like `mapcar' but TREE is a cons tree instead of a proper list."
  (if (consp tree)
      (cons (mapt function (car tree))
            (mapt function (cdr tree)))
      (funcall function tree)))

(defmacro expand-options-for-which-files
    (language which &rest args &key &allow-other-keys)
  "Expand the options for WHICH calling `create-software' appropriately.
* LANGUAGE language of the software to create
* WHICH string indicating the variant being created (e.g. my,your,old)
* ARGS list of additional arguments to pass thru to `create-software'"
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
                        (cdr pairs))))
    ,@args))

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

(defun preferred-language (lang)
  "Given LANG, a guess or resolved language, return a preferred language.
At the moment that means /not/ using tree-sitter."
  (case lang
    (t lang)))

(defun md5string (text)
  (format nil "~{~X~}" (coerce (md5:md5sum-string text) 'list)))

(defvar *json-diffs* (make-hash-table :test #'equalp))

(defplace lookup-json-diff-hash (hash)
  (gethash hash *json-diffs* "\"MISSING DIFF\""))

(defun rest-diff (&rest additional-ast-diff-params)
  (destructuring-bind (&key old-file new-file language link)
      (alist-plist (decode-json-from-string (payload-as-string)))
    (flet ((process (&rest rest)
             (let ((*lisp-interaction* t))
               (handler-bind
                   ((error (lambda (e)
                             (describe e uiop:*stderr*)
                             (uiop:print-backtrace :condition e
                                                   :stream uiop:*stderr*)
                             (http-condition 500 "Error: ~a" e))))
                 (apply #'ast-diff old-file new-file :language language
                        rest)))))
      (if link
          (let* ((json (with-output-to-string (*standard-output*)
                         (process :json t)))
                 (key (md5string json)))
            (setf (lookup-json-diff-hash key) json)
            (format t "http://~a~:[~;:~]~a/show/~a"
                    *address* *port* *port* key))
          (apply #'process additional-ast-diff-params)))))

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
  (setf old-file (namestring (truename old-file))
        my-file (namestring (truename my-file))
        your-file (namestring (truename your-file))
        language (preferred-language
                  (or language (guess-language old-file my-file your-file))))
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

(defun common-files-by (getter soft1 soft2)
  (fbind (getter)
    (filter (lambda (relative-file)
              (ignore-some-conditions (file-error)
                (file= (path-join (project-dir soft1) relative-file)
                       (path-join (project-dir soft2) relative-file))))
            (intersection (convert 'set (mapcar #'car (getter soft1)))
                          (convert 'set (mapcar #'car (getter soft2)))))))

(defgeneric common-evolve-files (soft1 soft2)
  (:method ((soft1 t) (soft2 t)) nil)
  (:method ((soft1 project) (soft2 project))
    (common-files-by #'evolve-files soft1 soft2)))

(defgeneric common-other-files (soft1 soft2)
  (:method ((soft1 t) (soft2 t)) nil)
  (:method ((soft1 project) (soft2 project))
    (common-files-by #'other-files soft1 soft2)))

(define-command-rest auto-merge
    (my-file old-file your-file test-script
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
  (declare (ignorable manual quiet))
  #+drop-dead
  (drop-dead-date ()
    (exit-command auto-merge 2
                  (progn (format *error-output* "Software no longer valid.~%")
                         (finish-output *error-output*)
                         (quit 2))))
  (with-prof profile
    (when help (show-help-for-ast-merge))
    (setf *note-out* (list *error-output*))
    (unless (every #'resolve-file (list old-file my-file your-file))
      (exit-command auto-merge 2 (error "Missing source.")))
    (setf *random-state* (if read-seed *random-state*  (make-random-state t))
          out-dir (or out-dir (resolve-out-dir-from-source old-file))
          old-file (namestring (truename old-file))
          my-file (namestring (truename my-file))
          your-file (namestring (truename your-file))
          language (preferred-language
                    (or language (guess-language old-file my-file your-file)))
          num-tests (resolve-num-tests-from-num-tests num-tests)
          tests (create-test-suite test-script num-tests))
    (note 2 "Create software objects.")
    (let* ((my-soft
            (expand-options-for-which-files
             language "MY"
             :ignore-paths ignore-paths
             :ignore-other-paths ignore-paths))
           (old-soft
            (expand-options-for-which-files
             language "OLD"
             :ignore-paths ignore-paths
             :ignore-other-paths ignore-paths))
           (your-soft
            (expand-options-for-which-files
             language "YOUR"
             :ignore-paths ignore-paths
             :ignore-other-paths ignore-paths))
           (already-merged
            (union
             (intersection (common-evolve-files my-soft old-soft)
                           (common-evolve-files old-soft your-soft))
             (intersection (common-other-files my-soft old-soft)
                           (common-other-files old-soft your-soft))))
           (my
            (create-auto-mergeable my-soft
                                   :threads num-threads
                                   :already-merged already-merged))
           (old
            (create-auto-mergeable old-soft
                                   :threads num-threads
                                   :already-merged already-merged))
           (your
            (create-auto-mergeable your-soft
                                   :threads num-threads
                                   :already-merged already-merged)))
      (note 2 "Resolve differences")
      (let ((result
             (apply #'resolve
                    my old your
                    {auto-merge-test _ tests}
                    :num-threads num-threads
                    :strings strings
                    :base-cost base-cost
                    :wrap wrap
                    :wrap-sequences wrap-sequences
                    :max-wrap-diff max-wrap
                    (append (when evolve (list :evolve? evolve))
                            (when max-evals (list :max-evals max-evals))
                            (when max-time (list :max-time max-time))))))
        (note 2 "Emit result")
        (to-file result
                 (if (directory-pathname-p my-file)
                     (make-pathname :directory (append out-dir '("auto-merged")))
                     (make-pathname :directory out-dir
                                    :name "auto-merged"
                                    :type (pathname-type my-file))))))))

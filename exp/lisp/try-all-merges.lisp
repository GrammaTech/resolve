(defpackage :merge-experiment
  (:nicknames)
  (:use
   :gt/all
   :command-line-arguments)
(in-package :merge-experiment)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +try-all-merges-command-line-options+
    `((("build" #\b) :type string :optional t
       :documentation "Shell command to build the project")
      (("workdir" #\w) :type string :optional t
       :documentation "Directory into which to place the project")
      (("local" #\l) :type boolean :optional t
       :documentation "pass the 'local' option to git clone")
      (("diff" #\d) :type string :optional t
       :documentation "diff tool to use to compute patches.  Defaults to diff3")
      (("diff-options" #\o) :type string :optional t
       :documentation "Options to be passed to the diff tool")
      ((#\i) :type boolean :optional t
       :documentation "Check out into a separate numbered working directory for each merge")
      (("conflict-file" #\f) :type string :optional t
       :documentation "File from which to read conflict information")
      (("conflicts" #\c) :type list :optional t
       :documentation "List of conflicts to be merged")
      (("help" #\h) :type boolean :optional t
       :documentation "Print help message and exit")
      )))

(define-command try-all-merges (repo-root &spec +try-all-merges-command-line-options+)
  "Perform merges in a git repo"
  #.(format nil
            "~%Built from ~a ~a.~%"
            (lisp-implementation-type) (lisp-implementation-version))
  (when help (show-help-for-try-all-merges))
  (unless (resolve-file repo-root)
    (exit-command try-all-merges 2 (error "Missing project directory")))
  (setf repo-root (namestring (ensure-directory-pathname repo-root)))
  (when workdir
    (unless (resolve-file workdir)
      (exit-command try-all-merges 3 (error "Working directory not found")))
    (setf workdir (namestring (ensure-directory-pathname workdir))))
  (let ((count 0))
    (flet ((%f (conflict)
             (unless (and (typep conflict 'list)
                          (= (length conflict) 4)
                          (every #'stringp conflict))
               (exit-command try-all-merges 4
                             (error "Invalid conflict list: ~a" conflict)))
             (destructuring-bind (program-name base left right)
                 conflict
               (let ((trueworkdir (if i
                                      (format nil "~a-~a" workdir (incf count))
                                      workdir)))
                 (try-merges (if (equal repo-root "")
                                 program-name
                                 (format nil "~a/~a" repo-root program-name))
                             program-name base left right
                             :diff diff
                             :diff-options diff-options
                             :local local
                             :workdir trueworkdir
                             :build build))))
           (%read-conflicts (stream)
             "reads conflict lines from a file, returning a list of tuples"
             (iter (let ((line (read-line stream nil nil)))
                     (while line)
                     (collect
                         (split-sequence #\Space line :remove-empty-subseqs t))))))
      ;; Obtain conflicts, handle them
      (mapc #'%f
            (cond
              (conflicts
               conflics)
              (conflict-file
               (with-open-file (s conflict-file :direction :input)
                 (%read-conflicts s)))
              (t
               (%read-conflicts *standard-input*))))))
  (exit 0))

(defun touch (n)
  (assert (stringp n))
  (uiop:run-program `("touch" ,n) :force-shell nil))
          
(defun try-merge (repo name base left right
                  &key diff diff-options local workdir build)
  ;; Clone repos, reset them
  (unless diff (setf diff "diff3"))
  (let ((workdirs (iter (for d in '("base" "left" "right"))
                        (collect (format nil "~a~a-~a" workdir name d))))
        (result-dir (format nil "~a/~a-~a-~a-~a/" workdir name base left right)))
    (ensure-directories-exist result-dir)
    (iter (for d in '("base" "left" "right"))
          (for wd in workdirs)
          (for hash in (list base left right))
          ;; should check if directory exists
          (uiop:run-program
           `("git" "clone" ,@(when local (list local)) ,repo
                   wd "-q"))
          (uiop:run-program
           (format nil "cd ~a; git reset --hard ~a"
                   wd hash)))
    (when build
      (iter (for wd in workdirs)
            (uiop:run-program
             (format nil "cd ~a; git checkout origin/master -- gt-harness.sh" wd)))
      (iter (for wd in workdirs)
            (uiop:run-program
             (format nil "cd ~a; ~a" build)))
      (let ((cd "compile_commmands.json"))
        (setf diff (format nil "~a --old-compilation-database ~a-base/~a" diff name cd))
        (setf diff (format nil "~a --my-compilation-database ~a-left/~a" diff name cd))
        (setf diff (format nil "~a --your-compilation-database ~a-right/~a" diff name cd))))

    (iter (for file in (split-sequence #\Newline (uiop:run-program (format nil "cd ~a; git diff --name-only ~a ~a --"
                                                                           (first workdirs) left right))))
          (let ((nleft (format nil "~a-left/~a" name file))
                (nright (format nil "~a-right/~a" name file))
                (nbase (format nil "~a-base/~a" name file))
                (filedir (namestring (make-pathname :directory (pathname-directory (pathname file))))))
            (ensure-directories-exist (format nil "~a~a" result-dir filedir))
            (cond
              ((not (probe-file nbase))
               (if (probe-file nleft)
                   (if (probe-file nright)
                       (touch (format nil "~a~a.no-base" result-dir file))
                       (touch (format nil "~a~a.just-left" result-dir file)))
                   (if (probe-file nright)
                       (touch (format nil "~a~a.just-right" result-dir file))
                       (warn "Not found in any version (should not happen): ~a" file))))
              ((not (probe-file nleft))
               (if (probe-file nright)
                   (touch (format nil "~a~a.no-left" result-dir file))
                   (touch (format nil "~a~a.just-base" result-dir file))))
              ((not (probe-file nright))
               (touch (format nil "~a~a.no-right" result-dir file)))
              (t ; all three files exist
               (let ((cmd (format nil "~a ~a ~a ~a ~a"
                                  diff (or diff-options "")
                                  nleft nbase nright)))
                 (multiple-value-bind (out x err)
                     (uiop:run-program cmd :ignore-error-status t :output :string)
                   (declare (ignore x))
                   (flet ((dump (n err)
                            (let ((n (format nil "~a~a.~a.err" result-dir diffname file err)))
                              (with-open-file (s n :direction :output :if-exists :supersede
                                                 :if-does-not-exist :create)
                                (write-sequence out s)))))
                     (case err
                       (1 (format t "~a~a" result-dir file))
                       (t (format t "~a~a*~a" result-dir file err)))
                     (dump err))))))))))

                      
                        
                
    
                     
                                                          
                                              
      

(defpackage :resolve/software/project
  (:use :common-lisp
        :alexandria
        :iterate
        :named-readtables
        :curry-compose-reader-macros
        :resolve/ast-diff
        :resolve/alist
        :software-evolution-library
        :software-evolution-library/software/project))
(in-package :resolve/software/project)
(in-readtable :curry-compose-reader-macros)

(defun confirm-files-are-same (alist1 alist2)
  (let ((files1 (mapcar #'car alist1))
	(files2 (mapcar #'car alist2)))
    (unless (equal files1 files2)
      (error "Two file alists do not reference the same files: ~A, ~A" files1 files2))))

(defun sort-file-alist (alist)
  (sort (copy-list alist) #'string< :key (lambda (x) (string (car x)))))

(defmethod ast-diff* ((project1 project) (project2 project))
  #+debug (format t "ast-diff[PROJECT]~%")
  (flet ((%obj (proj) (make-instance 'alist-for-diff :alist (all-files proj))))
    (ast-diff* (%obj project1) (%obj project2))))

(defun make-table-for-alist (alist &key (test #'eql))
  (let ((tab (make-hash-table :test test)))
    (iter (for p in alist)
	  (when p
	    (setf (gethash (car p) tab) p)))
    tab))

(defun remove-files-not-in (files1 files2)
  (let ((ntab (make-table-for-alist files2 :test #'equal)))
    (remove-if-not (lambda (p) (gethash (car p) ntab)) files1)))

(defmethod ast-patch ((project project) (diff t)
                      &rest args &key &allow-other-keys)
  (let* ((files-obj (make-instance 'alist-for-diff
		      :alist (all-files project)))
	 (new-files-obj (apply #'ast-patch files-obj diff args))
	 (new-project (copy project))
	 (evolve-files-table (make-table-for-alist (evolve-files new-project)
                                                   :test #'equal))
	 (result-alist (alist-of-alist-for-diff new-files-obj)))
    (flet ((evolve? (p) (gethash (car p) evolve-files-table)))
      (let ((new-evolve-files (remove-if-not #'evolve? result-alist))
	    (new-other-files (remove-if #'evolve? result-alist)))
	(setf (evolve-files new-project) new-evolve-files
	      (other-files new-project) new-other-files)))
    new-project))

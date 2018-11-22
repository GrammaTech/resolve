
(defun confirm-files-are-same (alist1 alist2)
  (let ((files1 (mapcar #'car alist1))
	(files2 (mapcar #'car alist2)))
    (unless (equal files1 files2)
      (error "Two file alists do not reference the same files: ~A, ~A" files1 files2))))

(defun sort-file-alist (alist)
  (sort (copy-list alist) #'string< :key (lambda (x) (string (car x)))))

#|
(defmethod ast-diff ((project1 project) (project2 project))
  (let ((files1 (all-files project1))
	(files2 (all-files project2))
	(ntab (make-hash-table :test #'equal)))
    (setf files1 (sort-file-alist files1))
    (setf files2 (sort-file-alist files2))
    (setf files1 (remove-files-not-in files1 files2))
    (setf files2 (remove-files-not-in files2 files1))
    (confirm-files-are-same files1 files2)
    (iter (for (name . file1) in files1)
	  (for (nil . file2) in files2)
	  (collect (cons name (ast-diff file1 file2))))))
|#

(defmethod ast-diff ((project1 project) (project2 project))
  (flet ((%obj (proj) (make-instance 'alist-for-diff :alist (all-files proj))))
    (list (ast-diff (%obj project1) (%obj project2)))))

(defun remove-files-not-in (files1 files2)
  (let ((ntab (make-hash-table :test #'equal)))
    (iter (for (n) in files2)
	  (setf (gethash n ntab) t))
    (remove-if-not (lambda (p) (gethash (car p) ntab)) files1)))

(defmethod ast-patch ((project project) (diff t) &rest args &key &allow-other-keys)
  (let* ((files-obj (make-instance 'alist-for-diff
				  :alist (all-files project)))
	 (new-files-obj (apply #'ast-patch files-obj diff args))
	 (new-project (copy project)))
    (setf (evolve-files new-project) (alist-of-alist-for-diff new-files-obj))
    new-project))



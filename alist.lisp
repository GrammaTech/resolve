;; Code for diffing alists (order not mattering)
;; Assumes the alists do not have repeating keys
;; The alist is wrapped in an object, otherwise
;; the standard lisp diff would apply, which we
;; do not want.

(defpackage :resolve/alist
  (:use :gt/full
        :software-evolution-library
        :resolve/ast-diff)
  (:import-from :resolve/ast-diff :ast-diff* :ast-patch*)
  (:export :alist-for-diff
           :alist-of-alist-for-diff
           :edit-tree-alist-node))

(in-package :resolve/alist)

(defclass alist-for-diff ()
  ((alist-of-alist-for-diff
    :accessor alist-of-alist-for-diff
    :initarg :alist
    :documentation "The actual alist"))
  (:documentation "A wrapped association list, for diffing respecting its alist structure"))

(defun alist-to-table (alist table)
  "Store the mapping given by an alist into a hash table.  Discard NIL (non-pair)
elements of the alist."
  (iter (for p in alist)
	(when p
	  (setf (gethash (car p) table) p)))
  table)

(defmethod ast-diff* ((al1-obj alist-for-diff) (al2-obj alist-for-diff))
  #+debug (format t "ast-diff[ALIST]~%")
  (let* ((test #'equal)
	 (table1 (make-hash-table :test test))
	 (table2 (make-hash-table :test test))
	 (only-in-1 nil)
	 (only-in-2 nil)
	 (in-both nil)
	 (al1 (alist-of-alist-for-diff al1-obj))
	 (al2 (alist-of-alist-for-diff al2-obj)))
    (alist-to-table al1 table1)
    (alist-to-table al2 table2)
    (iter (for p in al1)
	  (when p
	    (if (gethash (car p) table2)
		(collect p into common)
		(collect p into only)))
	  (finally (setf only-in-1 only
			 in-both common)))
    (setf only-in-2 (remove-if (lambda (p) (or (null p) (gethash (car p) table1))) al2))
    ;; At this point, we have divided the alist into three
    ;; parts.  only-in-1 gets deleted, only-in-2 gets inserted,
    ;; and the other parts are handled recursively.  All NIL
    ;; elements (that is, not cons cells) have been discarded.
    (list
     :alist
     (append
      (mapcar (lambda (p) `(:delete-alist . ,p)) only-in-1)
      (mapcar (lambda (p)
		(let ((p2 (gethash (car p) table2)))
		  (if (equal? (cdr p) (cdr p2))
		      `(:same-alist . ,p)
		      `(:recurse-alist
			,(car p)
                        ,@(ast-diff* (cdr p) (cdr p2))))))
	      in-both)
      (mapcar (lambda (p) `(:insert-alist . ,p)) only-in-2)))))

(defmethod ast-patch* ((al-obj alist-for-diff) script
                       &rest keys &key (delete? t) &allow-other-keys)
  (let ((al (alist-of-alist-for-diff al-obj))
	(table (make-hash-table)))
    ;; just in case, remove NIL elements
    (when (member nil al)
      (setf al (remove nil al)))
    (alist-to-table al table)
    (assert (eql (car script) :alist))
    (setf script (cadr script))
    (let ((new-alist
	   (iter (for x in script)
		 (case (car x)
		   (:insert-alist (collect (cdr x)))
		   (:delete-alist
                    (let ((cdrx (cdr x))
                          (lookup (gethash (cadr x) table)))
                      (assert (equal? cdrx lookup)
                              ()
                              ":DELETE-ALIST value not the same as the value in the alist: ~a, ~a, ~a"
                              (cadr x) cdrx lookup))
		    (unless delete? (collect (cdr x))))
		   (:same-alist
                    (let ((cdrx (cdr x))
                          (lookup (gethash (cadr x) table)))
                      (assert (equal? cdrx lookup)
                              ()
                              ":SAME value not the same as the value in the alist: ~a, ~a, ~a"
                              (cadr x) cdrx lookup))
		    (collect (cdr x)))
		   (:recurse-alist
		    (let ((sub (gethash (cadr x) table)))
		      (collect
			  (cons (cadr x)
                                (apply #'ast-patch*
                                       (cdr sub) (cddr x) keys)))))))))
      (make-instance (class-of al-obj) :alist new-alist))))

(defmethod merge-diffs-on-syms ((sym-a (eql :alist)) (sym-b (eql :alist)) o-a o-b)
  (let ((al-d-a (cadar o-a))
	(al-d-b (cadar o-b)))
    ;; al-d-a and al-d-b are the scripts for two alists
    ;; Each element should be a list (:<keyword> <key> . <element-script>)
    ;; We set up a table to handle the various keys, then loop through the pairs
    (let ((table (make-hash-table :test 'equal))
	  (keys nil))
      (iter (for x in al-d-a)
	    (let ((key (cadr x)))
	      (assert (null (gethash key table))
                      ()
                      "KEY already present in table: ~a" key)
	      (setf (gethash key table) (list x nil))
	      (push key keys)))
      (iter (for x in al-d-b)
	    (let* ((key (cadr x))
		   (entry (gethash key table)))
	      (if entry
		  (setf (cadr entry) x)
		  (setf keys (cons key keys)
			(gethash key table) (list nil x)))))
      #+nil
      (maphash (lambda (k e)
		 (format t "~A ==> ~A~%" k e))
	       table)
      ;; At this point, KEYS is a list of all keys that were
      ;; mentioned in the two alist scripts, and TABLE maps
      ;; these keys to a list containing the two element
      ;; scripts for the entry
      (values
       (list :alist
	     (iter (for k in keys)
		   (let* ((entry (gethash k table))
			  (e1 (car entry))
			  (e2 (cadr entry)))
		     (ecase (car e1)
		       (:insert-alist
			(ecase (car e2)
			  ((:same-alist :delete-alist nil) (collecting e1))
			  ((:insert-alist :recurse-alist)
			   (record-unstable e1 e2)
			   (collecting e1))))
		       (:delete-alist
			(ecase (car e2)
			  ((:same-alist :delete-alist nil)
			   (collecting e1))
			  ((:insert-alist :recurse-alist)
			   (record-unstable e1 e2)
			   (collecting e2))))
		       ((:same-alist nil)
			(ecase (car e2)
			  ((:same-alist nil) (collecting e1))
			  ((:insert-alist)
			   (when (car e1) (record-unstable e1 e2))
			   (collecting e2))
			  ((:delete-alist :recurse-alist)
			   (collecting e2))))
		       (:recurse-alist
			(ecase (car e2)
			  ((:same-alist nil) (collecting e1))
			  ((:insert-alist)
			   (record-unstable e1 e2)
			   (collecting e2))
			  ((:delete-alist)
			   (collecting e1))
			  ((:recurse-alist)
			   (collecting
			    (list* :recurse-alist k
				   (merge-diffs2 (cddr e1) (cddr e2)))))))))))
       (cdr o-a)
       (cdr o-b)))))

(defclass edit-tree-alist-node (edit-tree-node-base)
  ((key :accessor edit-tree-alist-node-key
        :initarg :key
        :initarg :pathname
        :documentation "Key (usually a pathname) of an alist")
   (children :accessor edit-tree-node-children))
  (:documentation "Edit tree node for an edit in an alist"))

(defclass edit-tree-alist-insert-node (edit-tree-alist-node)
  ((target :accessor edit-tree-alist-node-target
           :initarg :target
           :documentation "Object inserted by an :INSERT-ALIST action"))
  (:documentation "Object for an :INSERT-ALIST script action"))

(defclass edit-tree-alist-delete-node (edit-tree-alist-node)
  ((source :accessor edit-tree-alist-node-source
           :initarg :source
           :documentation "Object deleted by an :DELETE-ALIST action"))
  (:documentation "Object for an :DELETE-ALIST script action"))

(defclass edit-tree-alist-recurse-node (edit-tree-alist-node)
  ((source :accessor edit-tree-alist-node-source
           :initarg :source
           :documentation "Object deleted by a :RECURSE-ALIST action")
   (target :accessor edit-tree-alist-node-target
           :initarg :target
           :documentation "Object inserted by a :RECURSE-ALIST action"))
  (:documentation "Object for a :RECURSE-ALIST action"))

(defmethod create-edit-tree ((ad1 alist-for-diff) (ad2 alist-for-diff) script
                             &rest args &key &allow-other-keys)
  (let ((al1 (alist-of-alist-for-diff ad1))
        (al2 (alist-of-alist-for-diff ad2))
        (table1 (make-hash-table :test #'equal))
        (table2 (make-hash-table :test #'equal)))
    (alist-to-table al1 table1)
    (alist-to-table al2 table2)
    (assert (typep script '(cons (eql :alist) (cons list null))))
    ;; Build an edit
    (iter
      (for (action . rest) in (cadr script))
      (ecase action
        (:same-alist) ;; do nothing
        (:insert-alist
         (collect
             (make-instance 'edit-tree-alist-insert-node
                            :pathname (car rest)
                            :target (cdar rest))))
        (:delete-alist
         (collect
             (make-instance 'edit-tree-alist-delete-node
                            :pathname (car rest)
                            :source (cdar rest))))
        (:recurse-alist
         (collect
             (let* ((key (car rest))
                    (source (gethash key table1))
                    (target (gethash key table2)))
               (assert source)
               (assert target)
               (make-instance 'edit-tree-alist-recurse-node
                              :pathname key
                              :source source
                              :target target
                              :children (apply #'create-edit-tree
                                               source
                                               target
                                               (cdr rest)
                                               args)))))))))

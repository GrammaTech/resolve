;;; Snippets used to test ast-diff

;;; TODO: Move as much of this as reasonable into sel/test.lisp.

(in-package :resolve/ast-diff)

(defun make-test-input (n f l)
  (append (list f)
	  (iter (for i from 1 to n)
		(collect i))
	  (list l)))

(defun do-test (n)
  (let ((t1 (make-test-input n 'a 'b))
	(t2 (make-test-input n 'c 'd)))
    (time (ast-diff t1 t2))))

(defun remove-empty-strings-from-ast (ast)
  (copy ast :children (iter (for child in (children ast))
                            (cond ((typep child 'ast)
                                   (collect (remove-empty-strings-from-ast child)))
                                  ((not (emptyp child))
                                   (collect child))))))

;; (defun diff-asts-old (a1 a2)
;;   (time (ast-diff-on-lists a1 a2)))

(defun diff-asts (a1 a2 &rest args)
  (time (apply #'ast-diff a1 a2 args)))

;; (defun diff-files-old (f1 f2)
;;   (let* ((ast1 (sel/sw/parseable:ast-root (sel:from-file (make-instance 'sel/sw/clang:clang) f1)))
;;	 (ast2 (sel/sw/parseable:ast-root (sel:from-file (make-instance 'sel/sw/clang:clang) f2))))
;;    (time (ast-diff-on-lists ast1 ast2))))

(defun diff-files (f1 f2 &rest args)
  (let* ((ast1 (sel/sw/parseable:ast-root (sel:from-file (make-instance 'sel/sw/clang:clang) f1)))
	 (ast2 (sel/sw/parseable:ast-root (sel:from-file (make-instance 'sel/sw/clang:clang) f2))))
    (time (apply #'ast-diff
                 ast1 ast2 args))))

(defun load-octomap (version)
  (let* ((dir "/pdietz/quicklisp/local-projects/resolve")
         (flags (sel/command-line:handle-comma-delimited-argument
                 (format nil
                         "-I ~a/test/etc/octomap/octomap-~a/include,-I ~atest/etc/octomap/octomap-~a/src"
                         dir version dir version)))
         (file (format nil "~a/test/etc/octomap/octomap-~a/src/test_iterators.cpp" dir version)))
    (ast-root (sel/command-line:create-software
               file
               :flags flags
               :ignore-other-paths '(list #P"compile_commands.json")
               :language 'sel/sw/clang:clang))))

(defun diff-octomap (&rest args)
  (let* ((dir "/pdietz/quicklisp/local-projects/resolve")
         (old-flags (sel/command-line:handle-comma-delimited-argument
                     (format nil
                             "-I ~a/test/etc/octomap/octomap-1.7.2/include,-I ~atest/etc/octomap/octomap-1.7.2/src"
                             dir dir)))
         (new-flags (sel/command-line:handle-comma-delimited-argument
                     (format nil
                             "-I ~a/test/etc/octomap/octomap-1.8.0/include,-I ~atest/etc/octomap/octomap-1.8.0/src"
                             dir dir)))
         (ast1 (sel/sw/parseable:ast-root
                (sel/command-line:create-software "test/etc/octomap/octomap-1.7.2/src/test_iterators.cpp"
                                                  :flags old-flags
                                                  :ignore-other-paths '(list #P"compile_commands.json")
                                                  :language 'sel/sw/clang:clang)))
         (ast2 (sel/sw/parseable:ast-root
                (sel/command-line:create-software "test/etc/octomap/octomap-1.8.0/src/test_iterators.cpp"
                                                  :flags new-flags
                                                  :ignore-other-paths '(list #P"compile_commands.json")
                                                  :language 'sel/sw/clang:clang))))
    (setf ast1 (remove-empty-strings-from-ast ast1))
    (setf ast2 (remove-empty-strings-from-ast ast2))
    (time (apply #'ast-diff ast1 ast2 args))))


(defun diff-strings (s1 s2 &key (fn #'ast-diff))
  (flet ((%fs (s) (sel/sw/parseable:ast-root (sel:from-string (make-instance 'sel/sw/clang:clang) s))))
    (let ((ast1 (%fs s1))
	  (ast2 (%fs s2)))
      (time (funcall fn ast1 ast2)))))

;;; Generation of random sexprs and mutations thereof

(defun random-from-seq (seq)
  "Generate a random member of a sequence."
  (let ((len (length seq)))
    (assert (> len 0))
    (elt seq (random len))))

(defun random-from-pdf (seq)
  "Generate a random index into a vector, where the probability
of generating i is proportional to (elt seq i)"
  (let* ((sum (reduce #'+ seq))
         (r (random sum)))
    (iter (for i from 0)
          (for p in-sequence seq)
          (while (>= (decf r p) 0))
          (finally (return i)))))

(defun random-partition (n p)
  "Partition n into p numbers, each >= 1 (if possible.)"
  (cond
   ((<= n p)
    (make-list p :initial-element 1))
   (t (mapcar #'1+ (random-partition* (- n p) p)))))

(defun random-partition* (n p)
  "Partition n into p numbers, each >= 0.  Return list of numbers."
  (assert (<= 1 p))
  (cond
   ((= p 1) (list n))
   ((= n 0) (make-list p :initial-element 0))
   (t (let* ((r (random p))
             (n1 (random (1+ n))))
        (cond
         ((= r 0)
          (cons n1 (random-partition* (- n n1) (1- p))))
         ((= r (1- p))
          (append (random-partition* (- n n1) (1- p)) (list n1)))
         (t
          (let* ((n2 (random (1+ (- n n1))))
                 (n3 (- n n1 n2)))
            (append (random-partition* n2 r)
                    (list n1)
                    (random-partition* n3 (- p 1 r))))))))))

(defgeneric random-generate (generator &key size &allow-other-keys)
  (:documentation "Generate a random object using GENERATOR"))

(defclass random-sexpr ()
  ((leafs :accessor random-sexpr-leafs
          :initarg :leafs
          :type sequence
          :initform (required-argument 'leafs)
          :documentation "Non-empty sequence of values to use as leafs in sexpr")
   )
  (:documentation "Object for generating random sexprs"))

(defmethod random-generate ((obj random-sexpr) &key (size (required-argument 'size)))
  (if (= size 1)
      (random-leaf obj)
      (let ((n (random-child-number obj size)))
        (assert (<= 1 n size))
        (let ((child-sizes (random-partition size n)))
          (assert (= (length child-sizes) n))
          (mapcar (lambda (cs) (random-generate obj :size cs)) child-sizes)))))

(defgeneric random-leaf (obj)
  (:method ((obj random-sexpr))
    (case (random 10)
      ((0 1 2 3 4 5 6 7)
       (random-from-seq (random-sexpr-leafs obj)))
      (8 (format nil "~a" (random 100)))
      (9 (format nil " ~a " (random 100))))))

(defgeneric random-child-number (obj size)
  (:documentation "Generate a random # in the range [1,size].
The distribution depends on OBJ")
  (:method :around ((obj t) (size integer))
           (assert (>= size 1))
           (let ((n (call-next-method)))
             (assert (integerp n))
             (assert (<= 1 n size))
             n))
  (:method ((obj random-sexpr) (size integer))
    (1+ (random (min 10 size)))))

(defgeneric add-hoc-mutate (mutator val &key &allow-other-keys)
  (:documentation "Modify VAL according to a random
template MUTATOR."))

(defgeneric generated-size (generator obj)
  (:documentation "Size of an object, as yielded by generator")
  (:method ((generator t) (obj cons))
    (let ((l obj))
      (+ (iter (while (consp l))
               (summing (generated-size generator (pop l) )))
         (if (null l) 0 (generated-size generator l)))))
  (:method ((generator t) (obj t)) 1))

(defmethod add-hoc-mutate ((generator t) (val list) &key (insert-size 1))
  (let* ((len (length val))
         (sizes (mapcar (lambda (x) (generated-size generator x)) val))
         (i (random-from-pdf (cons len sizes))))
    (case i
      (0
       (case (random 2)
         (0 ;; insert
          (let ((n (random (1+ len)))
                (x (random-generate generator :size insert-size)))
            (append (subseq val 0 n)
                    (list x)
                    (subseq val n))))
         (t ;; delete
          (let ((n (random len)))
            (append (subseq val 0 n)
                    (subseq val (1+ n)))))))
      (t
       (append (subseq val 0 (1- i))
               (list (add-hoc-mutate generator (elt val (1- i))))
               (subseq val i))))))

(defmethod add-hoc-mutate ((generator t) (val t) &key (insert-size 1))
  (loop (let ((new-val (random-generate generator :size insert-size)))
          (unless (equal new-val val)
            (return new-val)))))

(defmethod add-hoc-mutate ((generator t) (val null) &key (insert-size 1))
  (list (random-generate generator :size insert-size)))

(defun random-test-patch (size reps &optional (n-mutations 1))
  (let ((g (make-instance 'random-sexpr :leafs #(a b c d e f))))
    (iter (repeat reps)
          (let* ((root (random-generate g :size size))
                 (my root))
            (iter (repeat n-mutations) (setf my (add-hoc-mutate g my)))
            (let ((patch (ast-diff root my :conflict t)))
              (ast-patch root patch))))))

(defun random-test-converge (size reps &optional (n-mutations 1))
  (let ((g (make-instance 'random-sexpr :leafs #(a b c d e f))))
    (iter (repeat reps)
          (let* ((root (random-generate g :size size))
                 (my root)
                 (your root))
            (iter (repeat n-mutations) (setf my (add-hoc-mutate g my)))
            (iter (repeat n-mutations) (setf your (add-hoc-mutate g your)))
            (collecting (converge my root your :conflict t))))))

;;; Utility function to find nice primes for hashing
;;; Used this when putting together ast-hash

(defun is-prime? (n)
  (assert (integerp n))
  (and
   (not (<= n 1))
   (or (= n 2)
       (and (= (mod n 2) 1)
	    (let ((s (isqrt n)))
	      (iter (for i from 3 to s by 2)
		    (never (= (mod n i) 0))))))))

(defun find-prime (n)
  (assert (typep n '(integer 3)))
  (when (evenp n) (decf n))
  (iter (until (is-prime? n))
	(decf n 2))
  n)

(defun find-prime-up (n)
  (assert (typep n '(integer 3)))
  (when (evenp n) (incf n))
  (iter (until (is-prime? n)) (incf n 2))
  n)

;;; Random testing of ast-diff-on-lists, ast-diff

(defun random-partition (n m)
  "Produce a random partition of the integer N into M positive integers."
  (assert (>= n m 1))
  (let ((a (make-array m :initial-element 1)))
    (iter (repeat (- n m))
	  (incf (aref a (random m))))
    (coerce a 'list)))

(defun make-random-diff-input (n &key top)
  "Generate a random s-expression for use as input to ast-diff.  When TOP
is true don't generate a non-list."
  (if (<= n 1)
      (let ((x (case (random 4)
		 (0 (random 5))
		 (1 (random 10))
		 (2 (elt #(a b c d e) (random 5)))
		 (3 (string (elt "abcdefghij" (random 10)))))))
	(if top (list x) x))
      (let ((p (random-partition n (1+ (random (min 20 n))))))
	(mapcar #'make-random-diff-input p))
      ))

(defun mutate-diff-input (x &key top)
  "Cause a single random change to a diff input produced
by MAKE-RANDOM-DIFF-INPUT"
  (if (consp x)
      (let ((pos (random (length x))))
	(setf x (copy-seq x))
	(setf (subseq x pos (1+ pos))
	      (list (mutate-diff-input (elt x pos))))
	x)
      (make-random-diff-input 1 :top top)))

#|
(defun random-test (n)
  "Test that the old and new diff algorithms do the same thing.
They sometimes won't, because the good enough algorithm doesn't
necessarily find the best diff."
  (let* ((x (make-random-diff-input n :top t))
	 (y (mutate-diff-input (mutate-diff-input x :top t) :top t)))
    (let ((result1 (multiple-value-list (ast-diff-on-lists x y)))
	  (result2 (multiple-value-list (ast-diff x y))))
      (if (equal (cadr result1) (cadr result2))
	  nil
	  (values :fail x y result1 result2)))))
|#

(defun random-test-2 (n &optional (fn #'ast-diff))
  "Confirm on random input that the diff algorithm produces
a valid patch.  Return :FAIL (and other values) if not."
  (let* ((x (make-random-diff-input n :top t))
	 (y (mutate-diff-input (mutate-diff-input x :top t) :top t))
	 (diff (funcall fn x y))
	 (patched-x (ast-patch x diff)))
    (unless (equalp y patched-x)
      (values :fail x y patched-x diff))))

(defun random-sequence (n &key (m 5))
  (iter (repeat n) (collect (random m))))

(defun test-gcs2 (n)
  (let ((s1 (random-sequence n))
	(s2 (random-sequence n)))
    ;; (format t "s1 = ~A, s2 = ~A~%" s1 s2)
    (let ((triples (good-common-subsequences2 s1 s2)))
      ;; Verify
      (if
       (and (iter (for (s1 s2 l) in triples)
		  (always (<= 0 s1))
		  (always (< 0 l))
		  (always (<= (+ s1 l) n))
		  (always (<= 0 s2))
		  (always (<= (+ s2 l) n)))
	    (iter (for (s1 s2 l) in triples)
		  (for (s1-2 s2-2 l-2) in (cdr triples))
		  (always (<= (+ s1 l) s1-2))
		  (always (<= (+ s2 l) s2-2)))
	    (iter (for (p1 p2 l) in triples)
		  (always (equal (subseq s1 p1 (+ p1 l))
				 (subseq s2 p2 (+ p2 l))))))
       nil
       (list s1 s2 triples)))))

;;;; Check for hash collisions

(defun check-for-hash-collisions (ast)
  (let ((table (make-hash-table)))
    (mapc-ast-and-strings
     ast
     (lambda (a)
       (pushnew a (gethash (ast-hash a) table)
                :test #'ast-equal-p)))
    (let ((collisions
           (sort
            (iter (for (k v) in-hashtable table)
                  (when (> (length v) 1)
                    (collecting (list k (length v)))))
            #'>
            :key #'cadr)))
      (values
       collisions
       (length collisions)
       (hash-table-count table)))))

(defun hashs-of-ast (ast)
  (let ((table (make-hash-table)))
    (mapc-ast-and-strings
     ast
     (lambda (a)
       (let* ((h (ast-hash a))
              (p (gethash h table)))
         (unless p (setf p (setf (gethash h table)
                                 (cons 0 (if (stringp a) (list a) nil)))))
         (incf (car p)))))
    (sort
     (iter (for (k v) in-hashtable table)
           (collecting (cons k v)))
     #'>
     :key #'cadr)))



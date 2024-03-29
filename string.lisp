(defpackage :resolve/string
  (:use :gt/full)
  (:export :string-diff))

(in-package :resolve/string)

;;; String diff algorithms

(defun simplify-string (s)
  (if (typep s 'simple-string)
      s
      (coerce s 'simple-string)))

(defun string-diff (s1 s2 &key ignore-whitespace)
  (setf s1 (simplify-string s1)
        s2 (simplify-string s2))
  (let ((l1 (length s1))
        (l2 (length s2))
        (dp-limit 1000000000))
    (if (and (<= l1 dp-limit)
             (<= l2 dp-limit))
        (string-diff-dp s1 s2 ignore-whitespace) ;; dynamic programming alg (exact)
        (string-diff-approx s1 s2 ignore-whitespace))))

(defun string-diff-approx (s1 s2 ignore-whitespace)
  (declare (ignore s1 s2 ignore-whitespace))
  (error "STRING-DIFF-APPROX not yet implemented"))

(defun string-diff-dp (s1 s2 ignore-whitespace)
  (declare (type simple-string s1 s2))
  (let ((l1 (length s1))
        (l2 (length s2)))
    (let ((d (make-array (list (1+ l1) (1+ l2)) :element-type 'fixnum
                         :initial-element 0)))
      ;; d is the distance matrix;  d[i,j] = min distance from s1[0..i-1]
      ;; to s2[0..j-1].
      (let ((count 0))
        (iter (for i from 1 to l1)
              (unless (and ignore-whitespace (whitespacep (schar s1 (1- i))))
                (incf count))
              (setf (aref d i 0) count)))
      (let ((count 0))
        (iter (for j from 1 to l2)
              (unless (and ignore-whitespace (whitespacep (schar s2 (1- j))))
                (incf count))
              (setf (aref d 0 j) count)))
      (iter (for i from 1 to l1)
            (let ((i1 (1- i)))
              (iter (for j from 1 to l2)
                    (let ((j1 (1- j)))
                      (if (eql (schar s1 i1) (schar s2 j1))
                          (setf (aref d i j) (aref d i1 j1))
                          (setf (aref d i j)
                                (if ignore-whitespace
                                    (min (+ (aref d i1 j)
                                            (if (whitespacep (schar s1 i1)) 0 1))
                                         (+ (aref d i j1)
                                            (if (whitespacep (schar s2 j1)) 0 1)))
                                    (1+ (min (aref d i1 j)
                                             (aref d i j1))))))))))
      ;; (format t "l1 = ~a, l1 = ~a~%" l1 l2)
      ;; (format t "D matrix: ~s~%" d)
      ;; d[l1,l2] is the minimum distance
      ;; trace back to find the string diff
      (let ((i l1) (j l2))
        (let ((diff (iter (while (and (> i 0) (> j 0)))
                          (let ((c1 (schar s1 (1- i)))
                                (c2 (schar s2 (1- j)))
                                (d10 (aref d (1- i) j))
                                (d01 (aref d i (1- j)))
                                (d11 (aref d (1- i) (1- j))))
                            (cond
                              ((eql c1 c2)
                               (cond
                                 ((<= d10 d11)
                                  (collect (cons :delete c1))
                                  (decf i))
                                 ((<= d01 d11)
                                  (collect (cons :insert c2))
                                  (decf j))
                                 (t
                                  (collect (cons :same c1))
                                  (decf i)
                                  (decf j))))
                              ((> d10 d01)
                               (collect (cons :insert c2))
                               (decf j))
                              (t
                               (collect (cons :delete c1))
                               (decf i)))))))
          (setf diff (nreverse diff))
          (merge-sequence-diff-operations diff)
          (values
           (append
            (cond
              ((= i 1)
               `((:delete . ,(schar s1 0))))
              ((> i 0)
               `((:delete-sequence . ,(subseq s1 0 i))))
              ((= j 1)
               `((:insert . ,(schar s2 0))))
              ((> j 0)
               `((:insert-sequence . ,(subseq s2 0 j)))))
            diff)
           (aref d l1 l2)))))))


;;; Longest common substring algorithm was added to
;;; hacked version of cl-string-match.  TODO: fast
;;; heuristic algorithm that uses this.

(defun merge-sequence-diff-operations (diff)
  (flet ((%et (v1 v2)
           (cond
             ((and (characterp v1) (characterp v2))
              'character)
             ((and (typep v1 'bit) (typep v2 'bit))
              'bit)
             (t t))))
    (when diff
      (let* ((p diff))
        (iter (while (cdr p))
              (let ((s2 (caadr p)))
                (case (caar p)
                  ((:insert :delete :same)
                   (cond
                     ((eql s2 (caar p))
                      (setf (caar p) (ecase s2
                                       (:insert :insert-sequence)
                                       (:delete :delete-sequence)
                                       (:same :same-sequence))
                            (cdar p) (make-array
                                      '(2)
                                      :element-type (%et (cdar p) (cdadr p))
                                      :initial-contents (list (cdar p)
                                                              (cdadr p))
                                      :fill-pointer 2 :adjustable t)
                            (cdr p) (cddr p)))
                     ;; :insert-sequence could go here, but ignore
                     (t (pop p))))
                  ((:insert-sequence :delete-sequence :same-sequence)
                   (cond
                     ((eql s2 (case (caar p)
                                (:insert-sequence :insert)
                                (:delete-sequence :delete)
                                (:same-sequence :same)))
                      (let ((v (cdar p))
                            (e (cdadr p)))
                        (cond
                          ((and (vectorp v)
                                (adjustable-array-p v)
                                (array-has-fill-pointer-p v)
                                (typep (cdadr p) (array-element-type v)))
                           (vector-push-extend e v)
                           (setf (cdr p) (cddr p)))
                          (t
                           (setf (cdar p) (copy-seq (cdar p)))
                           (pop p)))))
                     (t
                      (setf (cdar p) (copy-seq (cdar p)))
                      (pop p))))
                  (t (pop p))))))
      diff)))

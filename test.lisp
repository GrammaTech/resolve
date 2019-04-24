;;;; test.lisp --- Tests for software search and replace.
(defpackage :resolve/test
  (:use :common-lisp
        :alexandria
        :iterate
        :named-readtables
        :curry-compose-reader-macros
        :split-sequence
        :uiop
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/stefil-plus
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :resolve/core
        :resolve/ast-diff
        :resolve/alist
        :resolve/software/project
        :resolve/software/parseable
        :resolve/software/lisp)
  (:shadowing-import-from :uiop :getenv :quit :parameter-error)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :test :batch-test))
(in-package :resolve/test)
(in-readtable :curry-compose-reader-macros)

(defvar *this-file*
  #.(or *compile-file-truename*
        *load-truename*
        *default-pathname-defaults*))

#-gt
(progn
  (defvar *success* nil "Variable indicating test success or failure.")
  (defun batch-test (test project branch &optional args)
    "Run tests in 'batch' mode, printing results as a string."
    (declare (ignorable project branch args))

    (let* ((stefil::*test-progress-print-right-margin* (expt 2 20))
           (failures (coerce (stefil::failure-descriptions-of
                              (without-debugging (funcall test)))
                             'list)))
      (setf *success*
            (if failures
                (prog1 nil
                  (format *error-output* "FAILURES~%")
                  (mapc [{format *error-output* "  ~a~%"}
                         #'stefil::name-of
                         #'stefil::test-of
                         #'car #'stefil::test-context-backtrace-of]
                        failures))
                (prog1 t
                  (format *error-output* "SUCCESS~%")))))))

(defun run-batch (&rest a)
  (declare (ignorable a))
  (batch-test #'test "RESOLVE" +resolve-branch+))

(defvar *binary-search* nil "Holds the binary_search software object.")
(defvar *forms* nil "Forms used in tests.")

(defixture resolve-asd-file-forms
  (:setup (setf *forms* (read-file-forms
                         (make-pathname
                          :name "resolve"
                          :type "asd"
                          :directory
                          (pathname-directory *this-file*)))))
  (:teardown (setf *forms* nil)))

(define-constant +etc-dir+
    (append (pathname-directory
             #.(or *compile-file-truename*
                   *load-truename*
                   *default-pathname-defaults*))
            (list "test" "etc"))
  :test #'equalp
  :documentation "Path to directory holding testing artifacts.")

(deftest sexp-diff-on-ast-file ()
  (with-fixture resolve-asd-file-forms
    (is (zerop (nth-value 1 (ast-diff *forms* *forms*)))
        "Handles forms from a lisp source file.")))

(defixture binary-search-clang
  (:setup
   (setf *binary-search*
         (from-file
          (make-instance 'clang
            :flags (list
                    "-I"
                    (namestring (make-pathname :directory +etc-dir+))))
          (make-pathname
           :name "binary_search"
           :type "c"
           :directory +etc-dir+))))
  (:teardown
   (setf *binary-search* nil)))

(defroot test)


;;;; AST Diff tests
(defsuite ast-diff-tests "AST-level diffs of Sexprs.")

(deftest sexp-diff-empty ()
  (is (equalp (multiple-value-list (ast-diff nil nil))
	      '(nil 0))
      "Simplest case -- empty list to itself."))

(deftest sexp-diff-empty-to-nonempty ()
  (is (equalp (multiple-value-list (ast-diff nil '(1)))
	      '(((:insert . 1)) 1))
      "Empty list vs. list of one atom"))

(deftest sexp-diff-nonempty-to-empty ()
  (is (equalp (multiple-value-list (ast-diff '(1) nil))
	      '(((:delete . 1)) 1))
      "List of one atom vs. empty list"))

(deftest sexp-diff-numbers ()
    (is (equalp (multiple-value-list (ast-diff 1 2))
		'(((:delete . 1) (:insert . 2))
		  2))
	"Diff of two numbers"))

(deftest sexp-diff-numbers-same ()
    (is (equalp (multiple-value-list (ast-diff 1 1))
		'(((:same . 1))
		  0))
	"Diff of two equal numbers"))

(deftest sexp-diff-equal-zero-cost ()
  (is (zerop (nth-value 1 (ast-diff '(1 2 3 4) '(1 2 3 4))))
      "Equal should have 0 cost."))

(deftest sexp-diff-non-equal-first-element ()
  (is (not (zerop (nth-value 1 (ast-diff '(1 2 3 4) '(0 2 3 4)))))
      "Difference in first car is caught."))

(deftest sexp-shuffled-elements1 ()
  (is (equalp (ast-patch '(1 2 3 4)
			 (ast-diff '(1 2 3 4) '(1 4 3 2)))
	      '(1 4 3 2))
      "Shuffled elements create a correct diff"))

(deftest sexp-shuffled-elements2 ()
  (is (equalp (ast-patch '(1 2 3 4)
			 (ast-diff '(1 2 3 4) '(1 4 3 5 2)))
	      '(1 4 3 5 2))
      "Shuffled elements create a correct diff"))

(deftest sexp-shuffled-elements3 ()
  (is (equalp (ast-patch '(1 2 3 5 4)
			 (ast-diff '(1 2 3 5 4) '(1 4 3 2)))
	      '(1 4 3 2))
      "Shuffled elements create a correct diff"))

(deftest sexp-insert-then-delete ()
  (is (equalp (ast-patch '1 '((:insert . 2) (:delete . 1))) 2)
      "Insert followed by delete in patch"))

(deftest sexp-two-differences ()
  (is (equalp (ast-patch '(1 2 3 4 5) (ast-diff '(1 2 3 4 5) '(1 6 3 7 5)))
	      '(1 6 3 7 5))
      "Two lists that differ in two non-adjacent places"))

(deftest sexp-diff-simple-sublist-test ()
  (multiple-value-bind (script cost)
      (ast-diff '(1 '(1 2 3 4) 3 4) '(1 '(1 2 3 4) 3 5))
    (declare (ignorable cost))
    (multiple-value-bind (script-sublist cost-sublist)
        (ast-diff '(1 2 3 4) '(1 2 3 5))
      (declare (ignorable cost-sublist))
      (is (= (length script) (length script-sublist))
          "Sublists have no effect on script when same.")
      (is (= (length script) (length script-sublist))
          "Sublists have no effect on cost when same."))))

(deftest ast-diff-one-added-at-the-end ()
  (multiple-value-bind (diff cost) (ast-diff '(1 2) '(1 2 3))
    (is (= 1 cost) "Cost of a single addition at the end is one.")
    (is (= 3 (length diff)))))

(deftest ast-diff-recurses-into-subtree ()
  (multiple-value-bind (diff cost)
      (ast-diff '(1 (1 2 3 4 5) 2) '(1 (1 2 4 5) 3))
    (is (= 3 cost) "Cost of a sub-tree diff performed recursion.")
    (is (equalp '(:same :recurse :insert :delete) (mapcar #'car diff)))))

(deftest ast-diff-nested-recursion-into-subtree ()
  (multiple-value-bind (diff cost) (ast-diff '(((1 nil 3)) 3) '(((1 2 3)) 3))
    (is (= 2 cost) "Cost of a nested sub-tree diff performed recursion.")
    (is (equalp '(:recurse :same) (mapcar #'car diff)))))

(defun ast-diff-and-patch-equal-p (orig new)
  (ast-equal-p new (ast-patch orig (ast-diff orig new))))

(deftest ast-diff-and-patch-is-equal-simple ()
  (is (ast-diff-and-patch-equal-p '(1 2 3 4) '(1 2 z 4))))

(deftest ast-diff-and-patch-is-equal-recurse ()
  (is (ast-diff-and-patch-equal-p '(1 2 (1 2 3 4) 4) '(1 2 (1 2 z 4) 4))))

(deftest ast-diff-and-patch-is-equal-tail ()
  (is (ast-diff-and-patch-equal-p '(1 2 3 . 4) '(1 5 3 . 4))))

(deftest ast-diff-and-patch-unequal-tail ()
  (is (ast-diff-and-patch-equal-p '(1 2 3 . 4) '(1 2 3 . 5))))

(deftest ast-diff-with-string ()
    (is (ast-diff-and-patch-equal-p '((1 "foo" 2)) '((3 "foo" 2)))))

(deftest ast-diff-simple-dotted-list ()
  (is (= 2 (nth-value 1 (ast-diff '(1 . 1) '(1 . 2))))))

(deftest ast-diff-nested-dotted-list ()
  (is (= 2 (nth-value 1 (ast-diff '((1 . 2)) '((1 . 1)))))))

(deftest ast-diff-mixed-proper-improper-list ()
  (is (= 2 (nth-value 1 (ast-diff (cons 1 nil) (cons 1 1))))))

(deftest ast-diff-double-insert ()
  (is (= 2 (nth-value 1 (ast-diff '(1 2 3 4) '(1 2 3 4 5 6))))))

(defun ast-diff-alist-test (al1 al2)
  (flet ((%f (alist) (make-instance 'resolve/alist:alist-for-diff
				    :alist alist)))
    (let* ((al-obj1 (%f al1))
	   (al-obj2 (%f al2))
	   (diff (ast-diff al-obj1 al-obj2))
	   (al-obj3 (ast-patch al-obj1 diff)))
      (sort (copy-list (resolve/alist:alist-of-alist-for-diff al-obj3))
            #'string< :key #'car))))

(deftest ast-diff-alist-diff-1 ()
  (is (equal (ast-diff-alist-test nil nil) nil)))

(deftest ast-diff-alist-diff-2 ()
  (is (equal (ast-diff-alist-test nil '((a . 1))) '((a . 1)))))

(deftest ast-diff-alist-diff-3 ()
  (is (equal (ast-diff-alist-test '((a . 1)) nil) nil)))

(deftest ast-diff-alist-diff-4 ()
  (is (equal (ast-diff-alist-test '((a . 1)) '((a . 2))) '((a . 2)))))

(deftest ast-diff-alist-diff-5 ()
  (is (equal (ast-diff-alist-test '((a . 1) (b . 3)) '((a . 2) (b . 3)))
             '((a . 2) (b . 3)))))

(deftest ast-diff-alist-diff-6 ()
  (is (equal (ast-diff-alist-test '((a . 1) (b . 2)) '((b . 2) (a . 1)))
             '((a . 1) (b . 2)))))

(deftest ast-diff-alist-diff-7 ()
  (is (equal (ast-diff-alist-test '((a . 1)) '((b . 2) (a . 3)))
             '((a . 3) (b . 2)))))

(deftest print-lisp-diff.1 ()
  (is (equalp (with-output-to-string (s)
		(print-diff (ast-diff '() '()) :no-color t :stream s))
              "")
      "Print diff of empty lists"))

(deftest print-lisp-diff.2 ()
  (is (equalp (with-output-to-string (s)
		(print-diff (ast-diff '(()) '(())) :no-color t :stream s))
              "()")
      "Print diff of list of empty list"))

(deftest print-lisp-diff.3 ()
  (is (equalp (with-output-to-string (s)
		(print-diff (ast-diff '() '(())) :no-color t :stream s))
              "{+()+}")
      "Print diff of insertion of empty list"))

(deftest print-lisp-diff.4 ()
  (is (equalp (with-output-to-string (s)
		(print-diff (ast-diff '(()) '()) :no-color t :stream s))
              "[-()-]")
      "Print diff of deletion of empty list"))


;;;; Clang AST Diff tests
(defsuite clang-ast-diff-tests "AST-level diffs of clang objects."
            (clang-mutate-available-p))

(deftest (diff-gets-back-on-track :long-running) ()
  (is (= 2 (nth-value
            1
            (ast-diff (from-string (make-instance 'clang)
                                   "int a; int b; int c; int d;")
                      (from-string (make-instance 'clang)
                                   "int a; int z; int b; int c; int d;"))))))

(deftest (diff-insert :long-running) ()
  (let ((orig (from-string (make-instance 'clang)
                           "int x; int y; int z;"))
        (a (from-string (make-instance 'clang)
                        "int a; int x; int y; int z;"))
        (b (from-string (make-instance 'clang)
                        "int x; int b; int y; int z;"))
        (c (from-string (make-instance 'clang)
                        "int x; int y; int z; int c;")))
    (let ((diff-a (ast-diff orig a)))
      (is diff-a)
      (is (ast-equal-p (ast-root (ast-patch (copy orig) diff-a))
                       (ast-root a)))
      (is (equalp (mapcar #'car diff-a)
                  '(:same :insert :insert :same :same
                    :same :same :same :same))))
    (let ((diff-b (ast-diff orig b)))
      (is diff-b)
      (is (ast-equal-p
           (ast-root (ast-patch (copy orig) diff-b))
           (ast-root b)))
      (is (equalp (mapcar #'car diff-b)
                  '(:same :same :insert :insert :same
                    :same :same :same :same))))
    (let ((diff-c (ast-diff orig c)))
      (is diff-c)
      (is (ast-equal-p
           (ast-root (ast-patch (copy orig) diff-c))
           (ast-root c)))
      (is (equalp (mapcar #'car diff-c)
                  '(:same :same :same :same :same
                    :same :insert :insert :same))))))

(deftest (diff-delete :long-running) ()
  (let ((orig (from-string (make-instance 'clang)
                           "int x; int y; int z;"))
        (a (from-string (make-instance 'clang)
                        "int y; int z;"))
        (b (from-string (make-instance 'clang)
                        "int x; int z;"))
        (c (from-string (make-instance 'clang)
                        "int x; int y;")))
    (let ((diff-a (ast-diff orig a)))
      (is diff-a)
      (is (ast-equal-p
           (ast-root (ast-patch (copy orig) diff-a))
           (ast-root a)))
      (is (equalp (mapcar #'car diff-a)
                  '(:same :delete :delete :same :same :same :same))))
    (let ((diff-b (ast-diff orig b)))
      (is diff-b)
      (is (ast-equal-p
           (ast-root (ast-patch (copy orig) diff-b))
           (ast-root b)))
      (is (equalp (mapcar #'car diff-b)
                  '(:same :same :same :delete :delete :same :same))))
    (let ((diff-c (ast-diff orig c)))
      (is diff-c)
      (is (ast-equal-p
           (ast-root (ast-patch (copy orig) diff-c))
           (ast-root c)))
      (is (equalp (mapcar #'car diff-c)
                  '(:same :same :same :same :delete :delete :same))))))

(deftest (diff-recursive :long-running) ()
  (let* ((orig (from-string (make-instance 'clang)
                            "int x = 1; int y = 2; int z = 3;"))
         (new (from-string (make-instance 'clang)
                           "int x = 1; int y = 5; int z = 3;"))
         (diff (ast-diff orig new)))
    (is diff)
    (is (ast-equal-p (ast-root (ast-patch (copy orig) diff))
                     (ast-root new)))
    (is (equalp (mapcar #'car diff)
                '(:same :same :same :recurse :same :same :same)))))

(deftest (diff-text-changes :long-running) ()
  (let ((orig (from-string (make-instance 'clang)
                           "/* 1 */ int x; /* 2 */ int y; int z; /* 3 */"))
        (a (from-string (make-instance 'clang)
                        "/* X */ int x; /* 2 */ int y; int z; /* 3 */"))
        (b (from-string (make-instance 'clang)
                        "/* 1 */ int x; /* X */ int y; int z; /* 3 */"))
        (c (from-string (make-instance 'clang)
                        "/* 1 */ int x; /* 2 */ int y; int z; /* X */")))
    (let ((diff-a (ast-diff orig a)))
      (is diff-a)
      (is (ast-equal-p
           (ast-root (ast-patch (copy orig) diff-a))
           (ast-root a)))
      (is (equalp (mapcar #'car diff-a)
                  '(:recurse :same :same :same :same :same :same))))
    (let ((diff-b (ast-diff orig b)))
      (is diff-b)
      (is (ast-equal-p
           (ast-root (ast-patch (copy orig) diff-b))
           (ast-root b)))
      (is (equalp (mapcar #'car diff-b)
                  '(:same :same :recurse :same :same :same :same))))
    (let ((diff-c (ast-diff orig c)))
      (is diff-c)
      (is (ast-equal-p
           (ast-root (ast-patch (copy orig) diff-c))
           (ast-root c)))
      (is (equalp (mapcar #'car diff-c)
                  '(:same :same :same :same :same :same :recurse))))))

(deftest diff-real-text ()
  (with-fixture binary-search-clang
    (let ((var (copy *binary-search*)))
      (mutate var)
      (ast-diff *binary-search* var))))

(deftest diff-elide-same-test ()
  (with-fixture binary-search-clang
    (let ((var (copy *binary-search*)))
      (setf var (mutate var))
      (is (every
           [{member _ '(:delete :insert :delete-sequence :insert-sequence)}
            #'second]
           (ast-diff-elide-same (ast-diff *binary-search* var)))))))

(deftest print-diff.1 ()
  (is (equalp (with-output-to-string (s)
		(flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "int a; int c;")
					(%f "int a; int b; int c;"))
                              :no-color t
			      :stream s)))
	      "int a; {+int b; +}int c;")))

(deftest print-diff.2 ()
  (is (equalp (with-output-to-string (s)
		(flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "int a; int b; int c;")
					(%f "int a; int c;"))
                              :no-color t
			      :stream s)))
	      "int a; [-int b; -]int c;")
      "Print diff of a deletion"))

(deftest print-diff.3 ()
  (is (equalp (with-output-to-string (s)
		(flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "int a; int b; int c;")
					(%f "int a; int d; int c;"))
                              :no-color t
			      :stream s)))
	      "int a; int {+d+}[-b-]; int c;")
      "Print diff of a replacement"))

(deftest print-diff.4 ()
  (is (equalp (with-output-to-string (s)
		(flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"abcd\";")
					(%f "char *s = \"acd\";"))
                              :no-color t
			      :stream s)))
	      "char *s = \"a[-b-]cd\";")
      "Print diff of deletion of a character in a string"))

(deftest print-diff.5 ()
  (is (equalp (with-output-to-string (s)
		(flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"abcd\";")
					(%f "char *s = \"ad\";"))
                              :no-color t
			      :stream s)))
	      "char *s = \"a[-bc-]d\";")
      "Print diff of deletion of substring in a string"))



(deftest print-diff.6 ()
  (is (equalp (with-output-to-string (s)
		(flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"ad\";")
					(%f "char *s = \"abcd\";"))
                              :no-color t
			      :stream s)))
	      "char *s = \"a{+bc+}d\";")
      "Print diff of insertion of a substring in a string"))

(deftest print-diff.7 ()
  (is (equalp (with-output-to-string (s)
		(flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"ad\";")
					(%f "char *s = \"abd\";"))
                              :no-color t
			      :stream s)))
	      "char *s = \"a{+b+}d\";")
      "Print diff of insertion of a character in a string"))


;;;; AST merge3 tests
(defun clang-mutate-available-p ()
  (zerop (nth-value 2 (shell "which clang-mutate"))))

(defsuite ast-merge3 "Tests of MERGE3"
  (clang-mutate-available-p))

(deftest sexp-merge3-empty ()
  (is (equalp (multiple-value-list (merge3 nil nil nil))
	      '(nil nil))
      "Simplest case"))

(deftest sexp-merge3-insert-first ()
  (is (equalp (multiple-value-list (merge3 nil '(a) nil))
	      '(((:insert . a)) nil))
      "Adding one element in first change"))

(deftest sexp-merge3-insert-second ()
  (is (equalp (multiple-value-list (merge3 nil nil '(a)))
	      '(((:insert . a)) nil))
      "Adding one element in second change"))

(deftest sexp-merge3-insert-both ()
  (is (equalp (multiple-value-list (merge3 nil '(a) '(a)))
	      '(((:insert . a)) nil))
      "Adding one element in both changes"))

(deftest sexp-merge3-delete-first ()
  (is (equalp (multiple-value-list (merge3 '(a) nil '(a)))
	      '(((:delete . a)) nil))
      "Deleting one element in first change"))

(deftest sexp-merge3-delete-second ()
  (is (equalp (multiple-value-list (merge3 '(a) '(a) nil))
	      '(((:delete . a)) nil))
      "Deleting one element in second change"))

(deftest sexp-merge3-delete-second-2 ()
  (is (equalp (multiple-value-list (merge3 '(a) nil nil))
	      '(((:delete . a)) nil))
      "Deleting one element in both changes"))

(deftest sexp-merge3-recursive-first ()
  (is (equalp (multiple-value-list (merge3 '((a)) '((a b)) '((a))))
	      '(((:recurse (:same . a) (:insert . b))) nil))
      "Recurse in first change"))

(deftest sexp-merge3-recursive-second ()
  (is (equalp (multiple-value-list (merge3 '((a)) '((a)) '((a b))))
	      '(((:recurse (:same . a) (:insert . b))) nil))
      "Recurse in first change"))

(deftest sexp-merge3-recursive-both ()
  (is (equalp (multiple-value-list (merge3 '((a)) '((a b)) '((a b))))
	      '(((:recurse (:same . a) (:insert . b))) nil))
      "Recurse in both changes"))

(deftest sexp-merge3-insert-delete ()
  (is (equalp (multiple-value-list (merge3 '(a) '(a b) ()))
	      '(((:delete . a) (:insert . b)) nil))
      "Insert and delete in the same place."))

(deftest sexp-merge3-delete-insert ()
  (is (equalp (multiple-value-list (merge3 '(a) '() '(a b)))
	      '(((:delete . a) (:insert . b)) nil))
      "Delete and insert in the same place."))

(deftest sexp-merge3-delete-insert-tail ()
  (is (equalp (multiple-value-list (merge3 '(a b . c) '(a) '(a e . c)))
              '(((:same . a) (:insert . e) (:delete . b)
                 (:recurse-tail (:delete . c) (:insert)))
	        (((:delete . b) (:insert . e)))))
      "Delete and insert in the same place, with improper list."))

(deftest sexp-merge3-delete-insert-tail.2 ()
  (is (equalp (multiple-value-list (merge3 '((a b . c)) '((a)) '((a b . c))))
	      '(((:recurse (:same . a) (:delete . b)
                  (:recurse-tail (:delete . c) (:insert))))
		nil))
      "Delete including tail of improper list."))

(deftest sexp-merge3-delete-insert-tail.3 ()
  (is (equalp (multiple-value-list (merge3 '((a b . c)) '((a b . c)) '((a))))
	      '(((:recurse (:same . a) (:delete . b)
                  (:recurse-tail (:delete . c) (:insert))))
		nil))
      "Delete including tail of improper list."))

(deftest sexp-merge3-delete-insert-tail.4 ()
  (is (equalp (multiple-value-list
               (merge3 '((a b . c)) '((a . c)) '((a b . e))))
	      '(((:recurse (:same . a) (:delete . b)
                  (:recurse-tail (:delete . c) (:insert . e))))
		nil))
      "Delete, but change tail of improper list."))


(deftest sexp-merge3-insert2 ()
  (is (equalp (multiple-value-list (merge3 '(a d) '(a b c d) '(a d)))
	      '(((:same . a) (:insert . b) (:insert . c) (:same . d)) nil))
      "Two insertions"))

(deftest sexp-merge3-insert3 ()
  (is (equalp (multiple-value-list (merge3 '(d) '(d) '(a b c d)))
	      '(((:insert . a) (:insert . b) (:insert . c) (:same . d)) nil))
      "Three insertions"))

(deftest sexp-merge3-delete2 ()
   (is (equalp (multiple-value-list (merge3 '(a b c d) '(a d) '(a b c d)))
	      '(((:same . a) (:delete . b) (:delete . c) (:same . d)) nil))
       "Two deletions"))

(deftest sexp-merge3-delete3 ()
   (is (equalp (multiple-value-list (merge3 '(a b c d e) '(a b c d e) '(a e)))
	       '(((:same . a) (:delete . b) (:delete . c) (:delete . d)
                  (:same . e))
                 nil))
       "Three deletions"))

(deftest sexp-merge3-unstable-inserts ()
  (is (equalp (multiple-value-list (merge3 '(x 0) '(x 1) '(x 2)))
	      '(((:same . x) (:conflict ((:insert . 1)) ((:insert . 2)))
                 (:delete . 0))
		(((:insert . 1) (:insert . 2)))))
      "Two insertions at the same point"))

(deftest sexp-merge3-unstable-recurse-insert/delete ()
  (is (equalp (multiple-value-list (merge3 '(x (a) y) '(x (b) y) '(x c y)))
	      '(((:same . x)
		 (:insert . c)
		 (:conflict ((:recurse (:insert . b) (:delete . a)))
		  ((:delete a)))
		 (:same . y))
		(((:recurse (:insert . b) (:delete . a)) (:delete a)))))
      "recurse and deletion at same point (unstable)"))

(deftest sexp-merge3-unstable-insert/delete-recurse ()
  (is (equalp (multiple-value-list (merge3 '(x (a) y) '(x c y) '(x (b) y)))
	      '(((:same . x)
		 (:insert . c)
		 (:conflict ((:delete a))
		  ((:recurse (:insert . b) (:delete . a))))
		 (:same . y))
		(((:delete a) (:recurse (:insert . b) (:delete . a))))))
      "deletion and recurse at same point (unstable)"))

(deftest sexp-merge3-insert/insert-tail1 ()
  (is (equalp (multiple-value-list (merge3 '((a b)) '((a c)) '((a b . d))))
	      '(((:recurse (:same . a) (:insert . c) (:delete . b)
		  (:recurse-tail (:delete) (:insert . d))))
		nil))
      "insert and insertion of a tail element"))

(deftest sexp-merge3-insert/insert-tail2 ()
  (is (equalp (multiple-value-list (merge3 '((a b)) '((a b . d)) '((a c))))
	      '(((:recurse (:same . a) (:insert . c) (:delete . b)
		  (:recurse-tail (:delete) (:insert . d))))
		nil))
      "insert and insertion of a tail element"))

(deftest sexp-merge3-insert/insert-tail3 ()
  (is (equalp (multiple-value-list (merge3 '((a b)) '((a b . d)) '((a c . d))))
	      '(((:recurse (:same . a) (:insert . c) (:delete . b)
		  (:recurse-tail (:delete) (:insert . d))))
		nil))
      "insert and insertion of a tail element"))

(deftest sexp-merge3-insert/insert-tail4 ()
  (is (equalp (multiple-value-list
               (merge3 '((a . d)) '((a b . d)) '((a c . d))))
	      '(((:recurse (:same . a)
                  (:conflict ((:insert . b)) ((:insert . c)))
		  (:same-tail . d)))
		(((:insert . b) (:insert . c)))))
      "insert and insertion with a common tail element"))

(deftest sexp-merge3-insert-string ()
  (is (equalp (multiple-value-list (merge3 '("abc") '("adbc") '("abec")))
	      '(((:recurse (:same . #\a)
                  (:insert . #\d) (:same . #\b)
                  (:insert . #\e) (:same . #\c))) nil))
      "Merge of separate insertions into a string"))

(deftest sexp-merge3-insert-string.2 ()
  (is (equalp (multiple-value-list
               (merge3 '("axbycz") '("axdbycz") '("axbyecz")))
	      '(((:recurse (:same . #\a) (:same . #\x) (:insert . #\d)
		  (:same . #\b) (:same . #\y)
		  (:insert . #\e)
		  (:same . #\c) (:same . #\z))) nil))
      "Merge of separate insertions into a string"))

(deftest sexp-merge3-string-same.1 ()
  (is (equalp (multiple-value-list
               (merge3 '(("foo" 2)) '((3 "foo" 2)) '((4 "foo" 2))))
	      '(((:recurse (:conflict ((:insert . 3)) ((:insert . 4)))
                  (:same . "foo") (:same . 2)))
		(((:insert . 3) (:insert . 4)))))
      "Special handling of strings following insertions"))

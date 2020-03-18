;;;; test.lisp --- Tests for software difference display and resolution
(defpackage :resolve/test
  (:use :gt/full
        #+gt :testbot
        :software-evolution-library
        :software-evolution-library/utility/debug
        :software-evolution-library/stefil-plus
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/source
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-project
        :software-evolution-library/software/javascript
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/json
        :software-evolution-library/software/simple
        :software-evolution-library/components/formatting
        :software-evolution-library/components/test-suite
        :resolve/core
        :resolve/ast-diff
        :resolve/alist
        :resolve/auto-merge
        :resolve/software/project
        :resolve/software/auto-mergeable
        :resolve/software/parseable
        :resolve/software/lisp)
  (:import-from :uiop/stream :read-file-forms)
  (:import-from :resolve/ast-diff :ast-diff* :ast-patch*
                :put-inserts-before-deletes)
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
  (setf sel/stefil+:*long-tests* t)
  (batch-test #'test "RESOLVE" +resolve-branch+))

(define-constant +etc-dir+
    (append (pathname-directory
             #.(or *compile-file-truename*
                   *load-truename*
                   *default-pathname-defaults*))
            (list "test" "etc"))
  :test #'equalp
  :documentation "Path to directory holding testing artifacts.")

(define-constant +javascript-dir+ (append +etc-dir+ (list "javascript"))
  :test #'equalp
  :documentation "Path to directory holding Javascript test programs.")

(define-constant +gcd-dir+ (append +etc-dir+ (list "gcd"))
  :test #'equalp
  :documentation "Path to directory holding GCD test programs.")

(defvar *binary-search* nil "Holds the binary_search software object.")
(defvar *forms*         nil "Forms used in tests.")
(defvar *old*           nil "Software used in diff/merge tests.")
(defvar *my*            nil "Software used in diff/merge tests.")
(defvar *your*          nil "Software used in diff/merge tests.")
(defvar *variants*      nil "List of software variants.")
(defvar *cnf* nil)
(defvar *new* nil)
(defvar *tests* nil)


;;; Fixtures
(defixture resolve-asd-file-forms
  (:setup (setf *forms* (read-file-forms
                         (make-pathname
                          :name "resolve"
                          :type "asd"
                          :directory
                          (pathname-directory *this-file*)))))
  (:teardown (setf *forms* nil)))

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

(defixture json-conflict-yargs
  (:setup
   (destructuring-bind (base left right)
       (mapcar
        (lambda (name)
          (from-file
           (make-instance 'json)
           (make-pathname :directory (append +javascript-dir+ '("package-json"))
                          :type "json"
                          :name name)))
        '("d5da5eb" "ea14630" "6655688"))
     (setf *old* base *my* left *your* right)))
  (:teardown
   (setf *old* nil *my* nil *your* nil)))

(defixture gcd-conflict-clang
  (:setup
   (destructuring-bind (my old your)
       (mapcar
        (lambda (name)
          (from-file
           (make-instance 'clang)
           (make-pathname :directory +gcd-dir+
                          :type "c"
                          :name name)))
        '("gcd-wo-curlies-fix" "gcd-wo-curlies" "gcd-wo-curlies-prose"))
     (setf *my* my *old* old *your* your)))
  (:teardown
   (setf *old* nil *my* nil *your* nil)))

(defixture gcd-conflict-javascript
  (:setup
   (destructuring-bind (my old your)
       (mapcar
        (lambda (name)
          (from-file
           (make-instance 'javascript)
           (make-pathname :directory +gcd-dir+
                          :type "js"
                          :name name)))
        '("gcd-fix" "gcd" "gcd-prose"))
     (setf *my* my *old* old *your* your)))
  (:teardown
   (setf *old* nil *my* nil *your* nil)))

(defun populate-js-abacus-variants ()
  (mapcar
   (lambda (name)
     (cons (make-keyword (string-upcase name))
           (from-file
            (make-instance 'javascript)
            (make-pathname :directory (append +javascript-dir+ '("abacus"))
                           :type "js"
                           :name (concatenate 'string "abacus-" name)))))
   '("orig" "calc-line" "borders" "space" "bead" "animate" "min-lines")))

(defixture javascript-abacus-variants
  (:setup
   (setf *variants* (populate-js-abacus-variants)))
  (:teardown
   (setf *variants* nil)))

(defixture javascript-converge-conflict
  (:setup (setf *variants* (populate-js-abacus-variants)
                *cnf* (converge (aget :borders *variants*)
                                (aget :orig *variants*)
                                (aget :min-lines *variants*) :conflict t
                                :base-cost 5)))
  (:teardown (setf *variants* nil *cnf* nil)))

(defroot test)


;;;; AST Diff tests
(defsuite ast-diff-tests "AST-level diffs of Sexprs.")

(deftest sexp-diff-empty ()
  (is (equalp (multiple-value-list (ast-diff nil nil))
              '(((:same . :nil)) 0))
      "Simplest case -- empty list to itself."))

(deftest sexp-diff-empty-to-nonempty ()
  (is (equalp (unastify-lisp-diff (multiple-value-list (ast-diff nil '(1))))
              '(((:insert . 1) (:same . :nil)) 1))
      "Empty list vs. list of one atom"))

(deftest sexp-diff-nonempty-to-empty ()
  (is (equalp (unastify-lisp-diff (multiple-value-list (ast-diff '(1) nil)))
              '(((:delete . 1) (:same . :nil)) 1))
      "List of one atom vs. empty list"))

(deftest sexp-diff-numbers ()
  (is (equalp (multiple-value-list (ast-diff 1 2 :base-cost 0))
              '(((:insert . 2)
                 (:delete . 1))
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

(deftest sexp-diff-string.1 ()
  (is (eql (nth-value 1 (ast-diff "a" "ab")) 1)
      "Adding a character has cost 1"))

(deftest sexp-diff-string.2 ()
  (is (eql (nth-value 1 (ast-diff "ab" "a")) 1)
      "Deleting a character has cost 1"))

(deftest sexp-diff-string.3 ()
  (is (eql (nth-value 1 (ast-diff "a" " a ")) 2)
      "Add whitespace has cost 2"))

(deftest sexp-diff-string.4 ()
  (is (eql (nth-value 1 (ast-diff "a" " a " :ignore-whitespace t)) 0)
      "Adding whitespace costs nothing"))

(deftest sexp-diff-string.5 ()
  (is (eql (nth-value 1 (ast-diff " a " "a")) 2)
      "Adding whitespace has cost 2"))

(deftest sexp-diff-string.6 ()
  (is (eql (nth-value 1 (ast-diff " a " "a" :ignore-whitespace t)) 0)
      "Adding whitespace costs nothing"))

(deftest sexp-diff-string.7 ()
  (is (equal (multiple-value-list (ast-diff "a" "b" :strings nil))
             '(((:insert-sequence . "b") (:delete-sequence . "a"))
               6))))

(deftest sexp-diff-string.8 ()
  (is (equal (ast-diff '("a") '("b") :strings nil)
             '((:insert . "b") (:delete . "a") (:same . :nil)))))

(deftest sexp-diff-string.9 ()
  (is (equal (ast-diff "" "b" :strings nil)
             '((:insert-sequence . "b")))))

(deftest sexp-diff-string.10 ()
  (is (equal (ast-diff "a" "" :strings nil)
             '((:delete-sequence . "a")))))

(deftest sexp-diff-string.11 ()
  (is (equal (multiple-value-list
              (ast-diff " " "" :strings nil :ignore-whitespace t))
             '(((:delete-sequence . " ")) 2))))

(deftest sexp-diff-string.12 ()
  (is (equal (multiple-value-list
              (ast-diff "" " " :strings nil :ignore-whitespace t))
             '(((:insert-sequence . " ")) 2))))

(deftest sexp-diff-string.13 ()
  (is (equal (multiple-value-list
              (ast-diff " " "a" :strings nil :ignore-whitespace t))
             '(((:insert-sequence . "a") (:delete-sequence . " ")) 5))))

(deftest sexp-diff-string.14 ()
  (is (equal (multiple-value-list
              (ast-diff "a" " " :strings nil :ignore-whitespace t))
             '(((:insert-sequence . " ") (:delete-sequence . "a")) 5))))

(deftest sexp-diff-string.15 ()
  (is (equal (multiple-value-list
              (ast-diff "abc" "abc" :strings nil))
             '(((:same-sequence . "abc")) 0))))

(deftest sexp-diff-string.16 ()
  (is (equal (multiple-value-list
              (ast-diff " abc " "  abc  " :strings nil :ignore-whitespace t))
             '(((:insert-sequence . "  abc  ") (:delete-sequence . " abc ")) 4))))

(deftest sexp-diff-vector.1 ()
  (is (equalp (multiple-value-list
               (ast-diff '(#(1 2)) '(#(1 2))))
              '(((:same . #(1 2)) (:same . :nil)) 0))))

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

(deftest sexp-wrap.1 ()
  (is (equalp (ast-patch '(1 (2) 3)
                         `((:same . 1) (:recurse :wrap ((:same . 2) (:same . :nil)) (0) (nil) ((:nil)) (:list) ,(astify '((2)))) (:same . 3) (:same . :nil)))
              '(1 ((2)) 3))))

(deftest sexp-wrap.2 ()
  (is (equalp (ast-patch '(1 (2) 3)
                         `((:same . 1) (:recurse :wrap ((:same . 2) (:same . :nil)) (0 0) (nil nil) ((:nil) (:nil)) (:list :list) ,(astify '((2)))) (:same . 3) (:same . :nil)))
              '(1 (((2))) 3))))

(deftest sexp-wrap-sequence.1 ()
  (is (equalp (ast-patch '(1 2 3 4)
                         `((:same . 1) (:wrap-sequence 2
                                                       ((:same . 2) (:same . 3) (:insert . :nil))
                                                       (0)
                                                       (nil) (nil)
                                                       (:list)
                                                       ,(astify '(1 2 3 4)))
                           (:same . 4) (:same . :nil)))
              '(1 (2 3) 4))))

(deftest sexp-unwrap-sequence.1 ()
  (is (equalp
       (ast-patch '(1 (:a (2 3 4 5) :b) 5)
                  '((:same . 1)
                    (:unwrap-sequence
                     ((:same . 3) (:same . 4))
                     (1)
                     ((:a) (2))
                     ((:b :nil) (5 :nil)))
                    (:same . 5)
                    (:same . :nil)))
       '(1 3 4 5))))

(deftest sexp-unwrap-sequence.2 ()
  (is (equalp
       (ast-patch '(1 (2 3 4 5) 5)
                  '((:same . 1)
                    (:unwrap-sequence
                     ((:same . 3) (:same . 4))
                     ()
                     ((2))
                     ((5 :nil)))
                    (:same . 5)
                    (:same . :nil)))
       '(1 3 4 5))))

(deftest sexp-diff-wrap-sequence.1 ()
  (is (equalp
       (unastify-lisp-diff (ast-diff '(1 2 (3) 4 5) '(1 (2 (3) 4) 5)
                                     :wrap-sequences t))
       '((:same . 1)
         (:wrap-sequence 3 ((:same . 2) (:same 3) (:same . 4) (:insert . :nil))
          nil nil nil nil (2 (3) 4))
         (:same . 5) (:same . :nil)))))

(deftest sexp-diff-wrap-sequence.2 ()
  (is (equalp
       (unastify-lisp-diff (ast-diff '(1 2 (3) 4 5) '(1 (2 (3) . 4) 5)
                                     :wrap-sequences t))
       '((:same . 1)
         (:wrap-sequence 3 ((:same . 2) (:same 3) (:same . 4)) nil
          nil nil nil (2 (3) . 4))
         (:same . 5) (:same . :nil)))))

;; Nested sequence wrapping
(deftest sexp-diff-wrap-sequence.3 ()
  (is (equalp
       (let ((l1 '(1 2 (3 :a (4) :b 5) 6 7))
             (l2 '(1 (2 (3 (:a (4) :b) 5) 6) 7)))
         (unastify-lisp-diff (ast-diff l1 l2 :wrap-sequences t)))
       '((:same . 1)
         (:wrap-sequence 3
          ((:same . 2)
           (:recurse (:same . 3)
                     (:wrap-sequence 3 ((:same . :a) (:same 4)
                                        (:same . :b) (:insert . :nil))
                                     nil nil nil nil (:a (4) :b))
                     (:same . 5) (:same . :nil))
           (:same . 6) (:insert . :nil))
          nil nil nil nil (2 (3 (:a (4) :b) 5) 6))
         (:same . 7) (:same . :nil)))))

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
    (is (= 4 (length diff)))))

(deftest ast-diff-recurses-into-subtree.1 ()
  (multiple-value-bind (diff cost)
      (ast-diff '(1 (1 2 3 4 5) 2) '(1 (1 2 4 5) 3) :base-cost 0)
    (is (= 3 cost) "Cost of a sub-tree diff performed recursion.")
    (is (equalp '(:same :recurse :insert :delete :same) (mapcar #'car diff)))))

(deftest ast-diff-recurses-into-subtree.2 ()
  (multiple-value-bind (diff cost)
      (ast-diff '(1 (1 2 3 4 5) 2) '(1 (1 2 4 5) 3) :base-cost 2)
    (is (= 9 cost) "Cost of a sub-tree diff performed recursion (expected 9, is ~a)." cost)
    (is (equalp '(:same :recurse :insert :delete :same) (mapcar #'car diff)))))

(deftest ast-diff-nested-recursion-into-subtree ()
  (multiple-value-bind (diff cost)
      (ast-diff '(((1 nil 3)) 3) '(((1 2 3)) 3) :base-cost 0)
    (is (= 3 cost) "Cost of a nested sub-tree diff performed recursion.")
    (is (equalp '(:recurse :same :same) (mapcar #'car diff)))))

(defun ast-diff-and-patch-equal-p (orig new &rest args &key &allow-other-keys)
  (ast-equal-p new (ast-patch orig (apply #'ast-diff orig new args))))

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

(deftest ast-diff-patch-wrap.1 ()
  (is (ast-diff-and-patch-equal-p '((1)) '(2 ((1)) 3) :wrap t)))

(deftest ast-diff-patch-wrap.2 ()
  (is (ast-diff-and-patch-equal-p '(:a :b :c :d ((1)) :e :f :g)
                                  '(:a :b :c :d (2 ((1)) 3) :e :f :g) :wrap t)))

(deftest ast-diff-patch-wrap.3 ()
  (is (ast-diff-and-patch-equal-p '((:a :b :c :d :e :f))
                                  '((:a :b (:c :d) :e :f)) :wrap t)))

(deftest ast-diff-patch-wrap.4 ()
  (is (ast-diff-and-patch-equal-p '((:a :b :c :d :e :f))
                                  '((:a :b (:c :d) :e :f)) :wrap t :wrap-sequence t)))

(deftest ast-diff-patch-wrap.5 ()
  (is (ast-diff-and-patch-equal-p '(1 (2) 3) '(1 ((2)) 3) :wrap t)))

(deftest ast-diff-patch-unwrap.1 ()
  (is (ast-diff-and-patch-equal-p '(2 ((1)) 3) '((1)) :wrap t)))

(deftest ast-diff-patch-unwrap.2 ()
  (is (ast-diff-and-patch-equal-p '(:a (2 ((1)) 3) :b) '(:a ((1)) :b) :wrap t)))

(deftest ast-diff-simple-dotted-list ()
  (is (= 2 (nth-value 1 (ast-diff '(1 . 1) '(1 . 2) :base-cost 0))))
  (is (= 6 (nth-value 1 (ast-diff '(1 . 1) '(1 . 2) :base-cost 2)))))

(deftest ast-diff-nested-dotted-list ()
  (is (= 2 (nth-value 1 (ast-diff '((1 . 2)) '((1 . 1)) :base-cost 0))))
  (is (= 6 (nth-value 1 (ast-diff '((1 . 2)) '((1 . 1)) :base-cost 2)))))

(deftest ast-diff-mixed-proper-improper-list ()
  (is (= 2 (nth-value 1 (ast-diff (cons 1 nil) (cons 1 1) :base-cost 0))))
  (is (= 6 (nth-value 1 (ast-diff (cons 1 nil) (cons 1 1) :base-cost 2)))))

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

(deftest sexp-diff-on-ast-file ()
  (with-fixture resolve-asd-file-forms
    (is (zerop (nth-value 1 (ast-diff *forms* *forms*)))
        "Handles forms from a lisp source file.")))


;;;; Clang AST Diff tests
(defsuite clang-ast-diff-tests "AST-level diffs of clang objects."
            (clang-mutate-available-p))

(deftest (diff-gets-back-on-track :long-running) ()
  (let ((obj1 (from-string (make-instance 'clang)
                           "int a; int b; int c; int d;"))
        (obj2 (from-string (make-instance 'clang)
                           "int a; int z; int b; int c; int d;")))
    (is (= 8 (nth-value 1 (ast-diff obj1 obj2 :base-cost 0))))
    (is (= 12 (nth-value 1 (ast-diff obj1 obj2 :base-cost 2))))
    (is (= 6 (nth-value 1 (ast-diff obj1 obj2 :ignore-whitespace t
                                    :base-cost 0))))
    (is (= 10 (nth-value 1 (ast-diff obj1 obj2 :ignore-whitespace t
                                    :base-cost 2))))))

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

(defun keys-of-diff (d)
  (sort (remove-if-not #'symbolp
                       (copy-list (remove-duplicates (flatten d))))
        #'string< :key #'symbol-name))

(deftest diff-wrap/unwrap.1 ()
  (flet ((keys (d) (keys-of-diff d)))
    (let* ((s1 "int f() { return 1; }")
           (s2 "int f() { return 1+2; }")
           (obj1 (from-string (make-instance 'clang) s1))
           (obj2 (from-string (make-instance 'clang) s2)))
      (multiple-value-bind (diff cost)
          (ast-diff obj1 obj2 :wrap t :max-wrap-diff 1000 :base-cost 0)
        (is (equal (keys diff) '(:binaryoperator :recurse :same :wrap)))
        (is (= cost 3))
        (is (equal (with-output-to-string (*standard-output*)
                     (print-diff diff :no-color t))
                   "int f() { return 1{++2+}; }")))
      (multiple-value-bind (diff cost)
          (ast-diff obj2 obj1 :wrap t :max-wrap-diff 1000 :base-cost 0)
        (is (equal (keys diff) '(:recurse :same :unwrap)))
        (is (= cost 3))
        (is (equal (with-output-to-string (*standard-output*)
                     (print-diff diff :no-color t))
                   "int f() { return 1[-+2-]; }")))
      (multiple-value-bind (diff cost)
          (ast-diff obj1 obj2 :wrap t :max-wrap-diff -100 :base-cost 0)
        (is (equal (keys diff) '(:delete :insert :recurse :same)))
        (is (= cost 8)))
      (multiple-value-bind (diff cost)
          (ast-diff obj2 obj1 :wrap t :max-wrap-diff -100 :base-cost 0)
        (is (equal (keys diff) '(:delete :insert :recurse :same)))
        (is (= cost 8))))))

(deftest diff-sequence-wrap/unwrap.1 ()
  (let* ((s1 "int f(int x, int y) { int c = 1; int z = x+y; return z+c; }")
         (s2 "int f(int x, int y, int p) { int c = 1; if (p == 0) { int z = x+y; return z+c; } return 0; }")
         (obj1 (from-string (make-instance 'clang) s1))
         (obj2 (from-string (make-instance 'clang) s2)))
    (multiple-value-bind (diff cost)
        (ast-diff obj1 obj2 :wrap t :max-wrap-diff 1000
                  :wrap-sequences t)
      (let ((k (keys-of-diff diff)))
        (is (equal k '(:ifstmt :insert :recurse :same :wrap-sequence))))
      (is (= cost 58))
      (let ((s (with-output-to-string (*standard-output*)
                 (print-diff diff :no-color t))))
        (is (equal s "int f(int x, int y{+, int p+}) { int c = 1; {+if (p == 0) { +}int z = x+y; return z+c;{+ } return 0;+} }"))))
    (multiple-value-bind (diff cost)
        (ast-diff obj2 obj1 :wrap t :max-wrap-diff 1000
                  :wrap-sequences t)
      (let ((k (keys-of-diff diff)))
        (is (equal k '(:delete :recurse :same :unwrap-sequence))))
      (is (= cost 58))
      (let ((s (with-output-to-string (*standard-output*) (print-diff diff :no-color t))))
        (is (equal s "int f(int x, int y[-, int p-]) { int c = 1; [-if (p == 0) { -]int z = x+y; return z+c;[- } return 0;-] }"))))))

(deftest diff-wrap-patch.1 ()
  (let* ((s1 "int f() { return 1; }")
         (s2 "int f() { return 1+2; }")
         (obj1 (from-string (make-instance 'clang) s1))
         (obj2 (from-string (make-instance 'clang) s2))
         (diff (ast-diff obj1 obj2 :wrap t))
         (ast1 (ast-root obj1))
         (ast2 (ast-root obj2))
         (ast3 (ast-patch ast1 diff)))
    ;; (format t "AST2:~%~a~%----------------~%~s~%" (ast-text ast2) (ast-to-list-form ast2))
    ;; (format t "AST3:~%~a~%----------------~%~s~%" (ast-text ast3) (ast-to-list-form ast3))
    (is (ast-equal-p ast2 ast3))))

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
                                        (%f "int a; int d; int c;")
                                        :base-cost 0)
                              :no-color t
			      :stream s)))
	      "int a; int {+d+}[-b-]; int c;")
      "Print diff of a replacement"))

;; Increasing the base cost makes larger scale replacements
;; more prefered, vs. fine scaled replacement inside strings
(deftest print-diff.3a ()
  (is (equalp (with-output-to-string (s)
                (flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "int a; int b; int c;")
                                        (%f "int a; int d; int c;")
                                        :base-cost 1)
                              :no-color t
			      :stream s)))
              "int a; int {+d+}[-b-]; int c;")
      "Print diff of a replacement"))

(deftest print-diff.4 ()
  (is (equalp (with-output-to-string (s)
                (flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"abcd\";")
                                        (%f "char *s = \"acd\";")
                                        :base-cost 2)
                              :no-color t
			      :stream s)))
	      "char *s = \"a[-b-]cd\";")
      "Print diff of deletion of a character in a string"))

(deftest print-diff.4a ()
  (is (equalp (with-output-to-string (s)
                (flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"abcd\";")
                                        (%f "char *s = \"acd\";")
                                        :base-cost 3)
                              :no-color t
			      :stream s)))
              "char *s = \"a[-b-]cd\";")
      "Print diff of deletion of a character in a string"))

(deftest print-diff.5 ()
  (is (equalp (with-output-to-string (s)
                (flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"abcd\";")
                                        (%f "char *s = \"ad\";")
                                        :base-cost 1)
                              :no-color t
			      :stream s)))
	      "char *s = \"a[-bc-]d\";")
      "Print diff of deletion of substring in a string"))

(deftest print-diff.6 ()
  (is (equalp (with-output-to-string (s)
                (flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"ad\";")
                                        (%f "char *s = \"abcd\";")
                                        :base-cost 1)
                              :no-color t
			      :stream s)))
	      "char *s = \"a{+bc+}d\";")
      "Print diff of insertion of a substring in a string"))

(deftest print-diff.7 ()
  (is (equalp (with-output-to-string (s)
                (flet ((%f (s) (from-string (make-instance 'clang) s)))
		  (print-diff (ast-diff (%f "char *s = \"ad\";")
                                        (%f "char *s = \"abd\";")
                                        :base-cost 1)
                              :no-color t
			      :stream s)))
	      "char *s = \"a{+b+}d\";")
      "Print diff of insertion of a character in a string"))

;;;; Simple object ast-diff tests
(deftest simple.ast-diff.1 ()
    (let ((obj1 (make-instance 'simple :genome nil))
          (obj2 (make-instance 'simple :genome nil)))
      (is (equalp (multiple-value-list (ast-diff obj1 obj2)) '(nil 0))
          "AST-DIFF of two trivial simple objects")))

(deftest simple.ast-diff.2 ()
    (let ((obj1 (make-instance 'simple :genome '(((:code . "x")))))
          (obj2 (make-instance 'simple :genome '(((:code . "x"))))))
      (is (equalp (multiple-value-list (ast-diff obj1 obj2)) '(((:same . "x")) 0))
          "AST-DIFF of two equivalent one line simple objects")))

(deftest simple.ast-diff.3 ()
    (let ((obj1 (make-instance 'simple :genome '(((:code . "axb")))))
          (obj2 (make-instance 'simple :genome '(((:code . "ayb"))))))
      (is (equalp (ast-diff obj1 obj2 :base-cost 0)
                  '((:recurse (:same . #\a) (:insert . #\y) (:delete . #\x) (:same . #\b))))
          "AST-DIFF of two different one line simple objects, base cost 0")
      (is (equalp (ast-diff obj1 obj2 :base-cost 2)
                  '((:recurse (:same . #\a) (:insert . #\y) (:delete . #\x) (:same . #\b))))
          "AST-DIFF of two different one line simple objects, base cost 2")
      (let ((obj3 (ast-patch obj1 '((:recurse (:same . #\a)
                                     (:insert . #\y) (:delete . #\x)
                                     (:same . #\b))))))
        (is (equalp (genome obj3) '(((:code . "ayb"))))
            "Patch correctly applies to a simple object"))))

;;;; AST edit tree tests
(deftest edit-tree.1 ()
  (let* ((obj1 (from-string (make-instance 'clang) "int a; int b; int c; int d; int e;"))
         (obj2 (from-string (make-instance 'clang) "int a; int c; int e;"))
         (edit-tree (create-edit-tree obj1 obj2 (ast-diff obj1 obj2))))
    (let ((count 0))
      (map-edit-tree edit-tree (lambda (x) (declare (ignore x)) (incf count)))
      (is (= 2 count)
          "Edit tree with two differences expect 2 nodes. (~a) (~a)"
          count edit-tree))))

(deftest edit-tree.2 ()
  (let* ((obj1 (from-string (make-instance 'clang) "int a;"))
         (obj2 (from-string (make-instance 'clang) "int a;"))
         (edit-tree (create-edit-tree obj1 obj2 (ast-diff obj1 obj2))))
    (is (null edit-tree)
        "Empty diffs produce the null edit tree")))

(deftest edit-tree.3 ()
  (let* ((obj1 (from-string (make-instance 'clang) "char *a = \"abcde\";"))
         (obj2 (from-string (make-instance 'clang) "char *a = \"ace\";"))
         ;; :STRINGS nil means the diff does not descend into the
         ;; string constant.
         (edit-tree
          (create-edit-tree obj1 obj2 (ast-diff obj1 obj2 :strings nil))))
    (let ((count 0))
      (map-edit-tree edit-tree (lambda (x) (declare (ignore x)) (incf count)))
      (is (= 1 count)
          "Edit tree with two differences expect 1 node. (~a)"
          count))))


;;;; AST merge3 tests
(defun clang-mutate-available-p ()
  (zerop (nth-value 2 (shell "which clang-mutate"))))

(defsuite ast-merge3 "Tests of MERGE3"
  (clang-mutate-available-p))

(deftest sexp-merge3-empty ()
  (is (equalp (multiple-value-list (merge3 nil nil nil))
              '(((:same . :nil)) nil))
      "Simplest case"))

(deftest sexp-merge3-empty-conflict ()
  (is (equalp (multiple-value-list (merge3 nil nil nil :conflict t))
              '(((:same . :nil)) nil))
      "Simplest case, conflict enabled"))

(deftest sexp-merge3-insert-first ()
  (is (equalp (unastify-lisp-diff (multiple-value-list (merge3 nil '(a) nil)))
              '(((:insert . a) (:same . :nil)) nil))
      "Adding one element in first change"))

(deftest sexp-merge3-insert-first-conflict ()
  (is (equalp (multiple-value-list (merge3 nil '(a) nil :conflict t))
              '(((:conflict ((:insert . a)) nil) (:same . :nil))
                (((:insert . a) (:same . :nil)))))
      "Adding one element in first change, conflict enabled"))

(deftest sexp-merge3-insert-second ()
  (is (equalp (multiple-value-list (merge3 nil nil '(a)))
              '(((:insert . a) (:same . :nil)) nil))
      "Adding one element in second change"))

(deftest sexp-merge3-insert-both ()
  (is (equalp (multiple-value-list (merge3 nil '(a) '(a)))
              '(((:insert . a) (:same . :nil)) nil))
      "Adding one element in both changes"))

(deftest sexp-merge3-delete-first ()
  (is (equalp (multiple-value-list (merge3 '(a) nil '(a)))
              '(((:delete . a) (:same . :nil)) nil))
      "Deleting one element in first change"))

(deftest sexp-merge3-delete-second ()
  (is (equalp (multiple-value-list (merge3 '(a) '(a) nil))
              '(((:delete . a) (:same . :nil)) nil))
      "Deleting one element in second change"))

(deftest sexp-merge3-delete-second-2 ()
  (is (equalp (multiple-value-list (merge3 '(a) nil nil))
              '(((:delete . a) (:same . :nil)) nil))
      "Deleting one element in both changes"))

(deftest sexp-merge3-recursive-first ()
  (is (equalp (multiple-value-list (merge3 '((a)) '((a b)) '((a)) :base-cost 0))
              '(((:recurse (:same . a) (:insert . b) (:same . :nil)) (:same . :nil)) nil))
      "Recurse in first change"))

(deftest sexp-merge3-recursive-second ()
  (is (equalp (multiple-value-list (merge3 '((a)) '((a)) '((a b)) :base-cost 0))
              '(((:recurse (:same . a) (:insert . b) (:same . :nil)) (:same . :nil)) nil))
      "Recurse in first change"))

(deftest sexp-merge3-recursive-both ()
  (is (equalp (multiple-value-list (merge3 '((a)) '((a b)) '((a b)) :base-cost 0))
              '(((:recurse (:same . a) (:insert . b) (:same . :nil)) (:same . :nil)) nil))
      "Recurse in both changes"))

(deftest sexp-merge3-insert-delete ()
  (is (equalp (multiple-value-list (merge3 '(a) '(a b) ()))
              '(((:delete . a) (:insert . b) (:same . :nil)) nil))
      "Insert and delete in the same place."))

(deftest sexp-merge3-delete-insert ()
  (is (equalp (multiple-value-list (merge3 '(a) '() '(a b)))
              '(((:delete . a) (:insert . b) (:same . :nil)) nil))
      "Delete and insert in the same place."))

(deftest sexp-merge3-delete-insert-tail ()
  (is (equalp (multiple-value-list (merge3 '(a b . c) '(a) '(a e . c)))
              '(((:same . a)
                 (:conflict ((:insert . :nil)) ((:insert . e)))
                 (:delete . b)
                 (:delete . c))
                (((:insert . :nil) (:insert . e)))))
      "Delete and insert in the same place, with improper list."))

(deftest sexp-merge3-delete-insert-tail.2 ()
  (is (equalp (multiple-value-list (merge3 '((a b . c)) '((a)) '((a b . c))
                                           :base-cost 0))
              '(((:recurse (:same . a) (:insert . :nil) (:delete . b)
                  (:delete . c))
                 (:same . :nil))
		nil))
      "Delete including tail of improper list."))

(deftest sexp-merge3-delete-insert-tail.3 ()
  (is (equalp (multiple-value-list (merge3 '((a b . c)) '((a b . c)) '((a))
                                           :base-cost 0))
              '(((:recurse (:same . a) (:insert . :nil) (:delete . b)
                  (:delete . c))
                 (:same . :nil))
		nil))
      "Delete including tail of improper list."))

(deftest sexp-merge3-delete-insert-tail.4 ()
  (is (equalp (multiple-value-list
               (merge3 '((a b . c)) '((a . c)) '((a b . e))
                       :base-cost 1))
	      '(((:recurse (:same . a) (:delete . b)
                  (:insert . e) (:delete . c))
                 (:same . :nil))
		nil))
      "Delete, but change tail of improper list."))

(deftest sexp-merge3-insert2 ()
  (is (equalp (multiple-value-list (merge3 '(a d) '(a b c d) '(a d)))
              '(((:same . a) (:insert . b) (:insert . c) (:same . d) (:same . :nil))
                nil))
      "Two insertions"))

(deftest sexp-merge3-insert3 ()
  (is (equalp (multiple-value-list (merge3 '(d) '(d) '(a b c d)))
              '(((:insert . a) (:insert . b) (:insert . c) (:same . d) (:same . :nil)) nil))
      "Three insertions"))

(deftest sexp-merge3-delete2 ()
   (is (equalp (multiple-value-list (merge3 '(a b c d) '(a d) '(a b c d)))
               '(((:same . a) (:delete . b) (:delete . c) (:same . d) (:same . :nil)) nil))
       "Two deletions"))

(deftest sexp-merge3-delete3 ()
   (is (equalp (multiple-value-list (merge3 '(a b c d e) '(a b c d e) '(a e)))
	       '(((:same . a) (:delete . b) (:delete . c) (:delete . d)
                  (:same . e) (:same . :nil))
                 nil))
       "Three deletions"))

(deftest sexp-merge3-unstable-inserts ()
  (is (equalp (multiple-value-list (merge3 '(x 0) '(x 1) '(x 2)))
	      '(((:same . x) (:conflict ((:insert . 1)) ((:insert . 2)))
                 (:delete . 0) (:same . :nil))
		(((:insert . 1) (:insert . 2)))))
      "Two insertions at the same point"))

(deftest sexp-merge3-unstable-recurse-insert/delete ()
  (is (equalp (unastify-lisp-diff
               (multiple-value-list (merge3 '(x (a) y) '(x (b) y) '(x c y)
                                            :base-cost 0)))
	      '(((:same . x)
                 (:insert . c)
                 (:conflict ((:recurse (:insert . b) (:delete . a) (:same . :nil)))
		  ((:delete a)))
                 (:same . y) (:same . :nil))
                (((:recurse (:insert . b) (:delete . a) (:same . :nil)) (:delete a)))))
      "recurse and deletion at same point (unstable)"))

(deftest sexp-merge3-unstable-insert/delete-recurse ()
  (is (equalp (unastify-lisp-diff
               (multiple-value-list (merge3 '(x (a) y) '(x c y) '(x (b) y)
                                            :base-cost 0)))
	      '(((:same . x)
		 (:insert . c)
		 (:conflict ((:delete a))
                  ((:recurse (:insert . b) (:delete . a) (:same . :nil))))
                 (:same . y) (:same . :nil))
                (((:delete a) (:recurse (:insert . b) (:delete . a) (:same . :nil))))))
      "deletion and recurse at same point (unstable)"))

(deftest sexp-merge3-insert/insert-tail1 ()
  (is (equalp (unastify-lisp-diff
               (multiple-value-list (merge3 '((a b)) '((a c)) '((a b . d))
                                            :base-cost 0)))
	      '(((:recurse (:same . a) (:insert . c) (:delete . b)
                  (:insert . d) (:delete . :nil))
                 (:same . :nil))
		nil))
      "insert and insertion of a tail element"))

(deftest sexp-merge3-insert/insert-tail2 ()
  (is (equalp (unastify-lisp-diff
               (multiple-value-list (merge3 '((a b)) '((a b . d)) '((a c))
                                            :base-cost 0)))
	      '(((:recurse (:same . a) (:insert . c) (:delete . b)
                  (:insert . d) (:delete . :nil))
                 (:same . :nil))
                nil))
      "insert and insertion of a tail element"))

(deftest sexp-merge3-insert/insert-tail3 ()
  (is (equalp (unastify-lisp-diff
               (multiple-value-list (merge3 '((a b)) '((a b . d)) '((a c . d))
                                            :base-cost 0)))
              '(((:recurse (:same . a) (:insert . c) (:insert . d) (:delete . b)
                  (:insert . d) (:delete . :nil))
                 (:same . :nil))
                nil))
      "insert and insertion of a tail element"))

(deftest sexp-merge3-insert/insert-tail4 ()
  (is (equalp (unastify-lisp-diff
               (multiple-value-list
                (merge3 '((a . d)) '((a b . d)) '((a c . d)))))
	      '(((:recurse (:same . a)
                  (:conflict ((:insert . b)) ((:insert . c)))
                  (:same . d))
                 (:same . :nil))
		(((:insert . b) (:insert . c)))))
      "insert and insertion with a common tail element"))

(deftest sexp-merge3-insert-string ()
  (is (equalp (multiple-value-list (merge3 '("abc") '("adbc") '("abec")))
	      '(((:recurse (:same . #\a)
                  (:insert . #\d) (:same . #\b)
                  (:insert . #\e) (:same . #\c))
                 (:same . :nil))
                nil))
      "Merge of separate insertions into a string"))

(deftest sexp-merge3-insert-string.2 ()
  (is (equalp (multiple-value-list
               (merge3 '("axbycz") '("axdbycz") '("axbyecz")))
	      '(((:recurse (:same . #\a) (:same . #\x) (:insert . #\d)
		  (:same . #\b) (:same . #\y)
		  (:insert . #\e)
                  (:same . #\c) (:same . #\z))
                 (:same . :nil))
                nil))
      "Merge of separate insertions into a string"))

(deftest sexp-merge3-string-same.1 ()
  (is (equalp (multiple-value-list
               (merge3 '(("foo" 2)) '((3 "foo" 2)) '((4 "foo" 2))))
	      '(((:recurse (:conflict ((:insert . 3)) ((:insert . 4)))
                  (:same . "foo") (:same . 2) (:same . :nil))
                 (:same . :nil))
		(((:insert . 3) (:insert . 4)))))
      "Special handling of strings following insertions"))

(deftest sexpr-converge-same.1 ()
  (is (equalp (converge nil nil nil) nil)
      "Trivial converge on nil"))

(deftest sexpr-converge-same.2 ()
  (is (equalp (converge '(a) '(a) '(a)) '(a))
      "Trivial converge on lists"))

(deftest sexpr-converge-same.3 ()
  (is (equalp (converge '(a b) '(a) '(a)) '(a b))
      "Converge on lists with insertion"))

(deftest sexpr-converge-same.4 ()
  (is (equalp (converge '(a c) '(a c) '(a b c)) '(a b c))
      "Converge on lists with insertion in second list"))

(deftest sexpr-converge.1-conflict ()
  (is (equalp (converge nil nil nil :meld? nil :conflict t) nil)
      "Trivial converge on nil with conflict flag enabled"))

(deftest sexpr-converge.2-conflict ()
  (is (equalp (converge '(a) '(a) '(a) :meld? nil :conflict t) '(a))
      "Trivial converge on lists with conflict flag enabled"))

(deftest sexpr-converge.3-conflict ()
  (is (equalp (converge '(a b) '(a) '(a b) :meld? nil :conflict t) '(a b))
      "Converge with matching insertions on lists with conflict flag enabled"))

(deftest sexpr-converge.4-conflict ()
  (let ((merged (converge '(a b) '(a) '(a c) :meld? nil :conflict t)))
    (is (= (length merged) 2) "4-conflict 1")
    (is (eql (car merged) 'a) "4-conflict 2")
    (is (conflict-ast-p (cadr merged))
        "4-conflict 3")
    (is (equal (sel/sw/parseable:conflict-ast-child-alist (cadr merged))
               '((:my b) (:your c)))
        "4-conflict 4")))

(deftest sexpr-converge.5-conflict ()
  (let ((merged (converge '(a b) '(a) '(a) :meld? nil :conflict t)))
    (is (= (length merged) 2) "5-conflict 1")
    (is (eql (car merged) 'a) "5-conflict 2")
    (is (conflict-ast-p (cadr merged))
        "5-conflict 3")
    (is (equal (sel/sw/parseable:conflict-ast-child-alist (cadr merged))
               '((:my b)))
        "5-conflict 4")))

(deftest sexpr-converge.5a-conflict ()
  (let ((merged (converge '(a c) '(a c) '(a b c) :meld? nil :conflict t)))
    (is (typep merged '(cons (eql a) (cons conflict-ast (cons (eql c) null))))
        "5a-conflict 1")
    (is (equal (sel/sw/parseable:conflict-ast-child-alist (cadr merged))
               '((:your b)))
        "5a-conflict 2")))

(deftest sexpr-converge.6-conflict ()
  (let ((merged (converge '(a b c) '(a b) '(a) :meld? nil :conflict t)))
    (is (typep merged '(cons (eql a)
                        (cons conflict-ast null)))
        "6-conflict 1")
    (is (equalp (conflict-ast-child-alist (cadr merged))
                '((:old b) (:my b c)))
        "6-conflict 2")))

(deftest sexpr-converge.6a-conflict ()
  (let ((merged (converge '(a) '(a b) '(a b c) :meld? nil :conflict t)))
    (is (typep merged '(cons (eql a)
                        (cons conflict-ast null)))
        "6a-conflict 1")
    (is (equalp (conflict-ast-child-alist (cadr merged))
                '((:old b) (:your b c)))
        "6a-conflict 2")))

(deftest sexpr-converge.7-conflict ()
  (let ((merged (converge '(a (c) b) '(a (d) b) '(a (e) b) :meld? nil
                          :conflict t :base-cost 0)))
    (is (typep merged '(cons (eql a)
                        (cons (cons conflict-ast null)
                         (cons (eql b) null))))
        "7-conflict 1")
    (is (equal (conflict-ast-child-alist (caadr merged))
               '((:my c) (:your e) (:old d)))
        "7-conflict 2")))

(deftest sexpr-converge.8-conflict ()
  (let ((merged (converge '(a ("a") b) '(a (d) b) '(a ("b") b)
                          :meld? nil :conflict t
                          :strings nil :base-cost 0)))
    (is (typep merged '(cons (eql a)
                        (cons (cons conflict-ast null)
                         (cons (eql b) null))))
        "8-conflict 1")
    (is (equal (conflict-ast-child-alist (caadr merged))
               '((:my "a") (:your "b") (:old d)))
        "8-conflict 2")))

(deftest sexpr-converge.9-conflict ()
  (let ((merged (converge '(a b) '(a (d) b) '(a (e) b) :meld? nil
                          :conflict t :base-cost 0)))
    (is (typep merged '(cons (eql a)
                        (cons conflict-ast
                         (cons (eql b) null))))
        "9-conflict 1")
    (is (equal (unastify-lisp-diff
                (conflict-ast-child-alist (cadr merged)))
               '((:old (d)) (:your (e))))
        "9-conflict 2")))

(defun converge-test-helper (m)
  (sort (copy-list (unastify-lisp-diff
                    (conflict-ast-child-alist (cadr m))))
        #'string< :key #'car))

(deftest sexpr-converge.10-conflict ()
  (let ((merged (converge '(a (d) b) '(a (d) b) '(a (e) b) :meld? nil
                          :conflict t :base-cost 0)))
    (is (typep merged '(cons (eql a) (cons conflict-ast
                                      (cons (eql b) null))))
        "10-conflict 1")
    (is (equalp (converge-test-helper merged)
                '((:my (d)) (:old (d)) (:your (e))))
        "10-conflict 2")))

(deftest sexpr-converge.11-conflict ()
  (let ((merged (converge '(a (e) b) '(a (d) b) '(a (d) b) :meld? nil
                          :conflict t :base-cost 0)))
    (is (typep merged '(cons (eql a) (cons conflict-ast
                                      (cons (eql b) null))))
        "11-conflict 1")
    (is (equalp (converge-test-helper merged)
                '((:my (e)) (:old (d)) (:your (d))))
        "11-conflict 2")))

(deftest sexpr-converge.12-conflict ()
  (let ((merged (converge '(a b c) '(a (b 1) c) '(a (b 2) c) :conflict t :wrap t)))
    (is (typep merged '(cons (eql a) (cons conflict-ast (cons (eql c) null)))))
    (is (equalp (converge-test-helper merged)
                '((:my b) (:old (b 1)) (:your (b 2)))))))

(deftest sexpr-converge.13-conflict ()
  (let ((merged (converge '(a b d) '(a (b c) d) '(a c d) :conflict t :wrap t)))
    (is (typep merged '(cons (eql a) (cons conflict-ast (cons (eql d) null)))))
    (is (equalp (converge-test-helper merged)
                '((:my b) (:old (b c)) (:your c))))))

(deftest sexpr-converge.14-conflict ()
  (let ((merged (converge '(a (b c 1) d) '(a b c d) '(a (b c 2) d) :conflict t :wrap t
                          :wrap-sequences t)))
    (is (typep merged '(cons (eql a) (cons conflict-ast (cons (eql d) null)))))
    (is (equalp (converge-test-helper merged)
                '((:my (b c 1)) (:old b c)
                  (:your (b c 2)))))))

(deftest sexpr-converge.15-conflict ()
  (let ((merged (converge '(a (b c) d) '(a b c d) '(a (b) c d) :conflict t :wrap t
                          :wrap-sequences t)))
    (is (typep merged '(cons (eql a) (cons conflict-ast (cons (eql d) null)))))
    (is (equal (converge-test-helper merged)
               '((:my (b c)) (:old b c)
                 (:your (b) c))))))

(deftest sexpr-converge.16-conflict ()
  (let ((merged (converge '(a b c e) '(a (b c d) e) '(a c d e) :conflict t :wrap t
                          :wrap-sequences t)))
    (is (typep merged '(cons (eql a) (cons conflict-ast (cons (eql e) null)))))
    (is (equal (converge-test-helper merged)
               '((:my b c) (:old (b c d))
                 (:your c d))))))


(deftest (json-merge3 :long-running) ()
  (with-fixture json-conflict-yargs
    (multiple-value-bind (merged unstable)
        (converge *my* *old* *your*)
      (is (null unstable))                     ; No conflicts.
      (is (search "minimist" (genome merged))) ; Something from my.
      (is (search "3\"," (genome merged))))))  ; Something from your.

(deftest (gcd-conflict-merge3 :long-running) ()
  (with-fixture gcd-conflict-clang
    (multiple-value-bind (merged unstable)
        (converge *my* *old* *your* :conflict t)
      (declare (ignorable merged unstable))
      ;; TODO: This *should* be the case but it isn't.
      #+regression (is unstable))))

(deftest (gcd-conflict-merge3-js :long-running) ()
  (with-fixture gcd-conflict-javascript
    (multiple-value-bind (merged unstable)
        (converge *my* *old* *your* :conflict t)
      (declare (ignorable merged unstable))
      ;; TODO: Possibly another place where our merge is too eager.
      #+regression (is unstable))))


;;; Automatic merge tests
(deftest (merges-and-test-of-abacus-variants :long-running) ()
  (with-fixture javascript-abacus-variants
    (let ((orig (aget :orig *variants*))
          ;; Expected to work on a simple merge.
          (expected-functional-pairs
           '((:CALC-LINE :MIN-LINES)
             (:BORDERS :SPACE)
             (:BORDERS :BEAD)
             (:BORDERS :ANIMATE)
             (:SPACE :BEAD)
             (:SPACE :ANIMATE)
             (:SPACE :MIN-LINES)
             (:BEAD :ANIMATE)
             (:BEAD :MIN-LINES)
             (:ANIMATE :MIN-LINES))))
      (mapcar
       (lambda (pair)
         (nest
          (let ((test (make-pathname
                       :name "test" :type "sh"
                       :directory (append +javascript-dir+ '("abacus"))))))
          (destructuring-bind ((my-name . my-obj) . (your-name . your-obj))
              pair)
          (let ((path (make-pathname
                       :directory (append +javascript-dir+ '("abacus"))
                       :type "js"
                       :name (mapconcat #'identity
                                        (cons "merged"
                                              (mapcar #'symbol-name
                                                      (list my-name your-name)))
                                        "-")))))
          (multiple-value-bind (merged unstable)
              (converge my-obj orig your-obj :base-cost 5)
            #-debug (declare (ignorable unstable))
            #+debug
            (format t "~&~12a~12a~12a~%" my-name your-name (length unstable))
            (to-file merged path))
          (multiple-value-bind (stdout stderr errno)
              (shell "~a ~a ~a"
                     (namestring test) (namestring path)
                     (mapconcat #'identity
                                (mapcar [#'string-downcase #'symbol-name]
                                        (list my-name your-name))
                                ","))
            (declare (ignorable stdout stderr))
            #+debug (format t "~&~12a~12a~12a~%" my-name your-name
                            (if (zerop errno) "PASS" "FAIL")))
          (when (member (list my-name your-name) expected-functional-pairs
                        :test #'equalp))
          (is (zerop errno)
              "Combination of ~a and ~a should be functional"
              my-name your-name)))
       (pairs (remove-if [{eql :orig} #'car] *variants*))))))

(deftest (merges-of-abacus-variants-w-conflicts :long-running) ()
  (with-fixture javascript-abacus-variants
    (let ((orig (aget :orig *variants*)))
      (mapcar
       (lambda (pair)
         (nest
          (destructuring-bind ((my-name . my-obj) . (your-name . your-obj))
              pair)
          #+debug (ignore-errors)
          (multiple-value-bind (merged unstable)
              (converge my-obj orig your-obj :conflict t)
            #-debug (declare (ignorable unstable))
            #+debug
            (format t "~12a~12a~12a~%" my-name your-name (length unstable)))
          (to-file merged)
          (make-pathname :directory (append +javascript-dir+ '("abacus"))
                         :type "js" :name)
          (mapconcat #'identity
                     (cons "conflict"
                           (mapcar #'symbol-name (list my-name your-name)))
                     "-")))
       (pairs (remove-if [{eql :orig} #'car] *variants*))))))

(deftest (resolve-to-single-equals-original/old :long-running) ()
  (with-fixture javascript-converge-conflict
    (is (string= (genome (astyle (resolve-to (copy *cnf*) :old)))
                 (genome (astyle (aget :orig *variants*)))))))

(deftest (resolve-to-single-equals-original/my :long-running) ()
  (with-fixture javascript-converge-conflict
    (is (string= (genome (astyle (resolve-to (copy *cnf*) :my)))
                 (genome (astyle (aget :borders *variants*)))))))

(deftest (resolve-to-single-equals-original/your :long-running) ()
  (with-fixture javascript-converge-conflict
    (is (string= (genome (astyle (resolve-to (copy *cnf*) :your)))
                 (genome (astyle (aget :min-lines *variants*)))))))

(deftest (resolve-to-should-not-modify-conflict-nodes-in-the-original
          :long-running) ()
  ;; NOTE: This was fixed by replacing `nconc' with `append' in
  ;; `set-ast-siblings' in SEL/SW/PARSEABLE.
  (flet ((conflict-nodes (obj)
           (let ((result
                  (remove-if-not #'conflict-ast-p
                                 (ast-to-list (ast-root obj)))))
             #+debug (format t "Conflict nodes:~%")
             #+debug (dolist (cn result) (format t "~a~%" cn))
             result)))
    (with-fixture javascript-converge-conflict
      (is (= (length (aget :my (conflict-ast-child-alist
                                (car (conflict-nodes *cnf*)))))
             (progn (replace-ast (copy *cnf*)
                                 (ast-path (first (conflict-nodes *cnf*)))
                                 (aget :my (conflict-ast-child-alist
                                            (first (conflict-nodes *cnf*))))
                                 :literal t)
                    (length (aget :my (conflict-ast-child-alist
                                       (car (conflict-nodes *cnf*)))))))))))

(deftest (resolve-to-of-copy-leaves-original-genome-unmollested
          :long-running) ()
  (with-fixture javascript-converge-conflict
    (let* (;; (orig-asts (mapc-ast (ast-root *cnf*) #'copy))
           (orig-genome (genome *cnf*))
           (my (resolve-to (copy *cnf*) :my)))
      (is (not (string= orig-genome (genome my)))
          "Resolved should have a different genome from original.~% ~
           orig:~Snew:~%~S~%" orig-genome (genome my))
      (is (string= orig-genome (genome *cnf*))
          "Original should *NOT* have a different genome from original.~% ~
           orig:~Snew:~%~S~%" orig-genome (genome *cnf*))
      ;; (is (equal-it orig-asts (mapc-ast (ast-root *cnf*) #'copy))
      ;;     "Original should *NOT* have different ASTs from original.~% ~
      ;;      orig:~Snew:~%~S~%" orig-asts (mapc-ast (ast-root *cnf*) #'copy))
      )))

(deftest (resolve-to-of-copy-leaves-original-genome-unmollested-simple
          :long-running) ()
  (with-fixture javascript-converge-conflict
    (let ((it (lastcar (remove-if-not #'conflict-ast-p
                                      (asts *cnf*)))))
      (is it "There is a conflict ast")
      #+debug
      (labels ((to-list (x)
                 (if (ast-p x)
                     (cons (ast-class x)
                           (mapcar #'to-list (ast-children x)))
                     x)))
        (iter
         (for (k . a) in *variants*)
         (format t "~A:~%~s~%" k (to-list (ast-root a))))
        (format t "AST:~%~s~%" (to-list (ast-root *cnf*))))
      (is (conflict-ast-p (get-ast *cnf* '(5 3 3)))
          "Path (5 3 3) is a conflict ast in the original.")
      (let ((new (replace-ast (copy *cnf*)
                              (ast-path it)
                              (aget :my (conflict-ast-child-alist it))
                              :literal t)))
        (is (typep (get-ast new '(5 3 3)) 'javascript-ast)
            "Path (5 3 3) is a JavaScript ast in result of replace-ast.")
        (is (conflict-ast-p (get-ast *cnf* '(5 3 3)))
            "Path (5 3 3) is STILL a conflict-ast in the original ~
             after replace-ast.")))))

(deftest (resolve-to-selects-alternatives-of-conflicts
          :long-running) ()
  (with-fixture javascript-converge-conflict
    ;; Conflicted software object has ASTs.
    (is (asts *cnf*))
    ;; Conflicted software object has conflcit ASTs.
    (is (remove-if-not #'conflict-ast-p
                       (ast-to-list (ast-root *cnf*))))
    (let ((old (resolve-to (copy *cnf*) :old))
          (my (resolve-to (copy *cnf*) :my))
          (your (resolve-to (copy *cnf*) :your)))
      (is (null (remove-if-not #'conflict-ast-p
                               (ast-to-list (ast-root old)))))
      (is (string/= (genome my) (genome old)))
      (is (string/= (genome your) (genome old)))
      (is (string/= (genome my) (genome your)))
      (is (string= (genome (astyle old))
                   (genome (astyle (aget :orig *variants*)))))
      ;; TODO: These next two should probably be passing.  In both
      ;;       (my and your) cases the trailing "}" closing the
      ;;       "board" function (in which the conflict was resolved)
      ;;       is being dropped.  This must be due to us somehow
      ;;       losing string siblings of resolved conflict nodes.
      ;;
      ;; NOTE: One could call `astyle' before calling `genome' to
      ;;       ensure more uniformity, but it doesn't matter yet.
      ;;
      ;; NOTE: It may be that replace-ast is actually modifying the
      ;;       original program's AST.  This is something to check.
      (is (string= (genome my)
                   (genome (aget :borders *variants*))))
      (is (string= (genome your)
                   (genome (aget :min-lines *variants*)))))))

#+manual          ; This test is only useful for manual investigation.
(deftest targeted-populate-run ()
  )

(deftest (can-populate-from-conflicted-merges :long-running) ()
  (nest
   (with-fixture javascript-converge-conflict)
   (destructuring-bind (my old your)
       (mapcar {aget _ *variants*} '(:borders :orig :min-lines)))
   (let* ((conflicted (nest (create-auto-mergeable)
                            (converge my old your :conflict t)))
          (chunks (remove-if-not #'conflict-ast-p
                                 (ast-to-list (ast-root conflicted))))
          (*population* (populate conflicted))))
   (is (= (length *population*) (expt 5 (length chunks)))
       "Population has the expected size ~d = 5^|chunks| => ~d."
       (length *population*) (expt 5 (length chunks)))
   (is (not (some [{some #'conflict-ast-p} #'ast-to-list] *population*))
       "Population has no conflict ASTs remaining.")))


;;; Additional tests of internals
(deftest ast-size-test ()
  (is (equal (ast-size (astify nil)) 2))
  (is (equal (ast-size (astify '(x y))) 4))
  (is (equal (ast-size (astify '(x . y))) 3))
  (is (equal (ast-size nil) 1))
  (is (equal (ast-size (astify "x")) 1))
  (let ((a (astify '(a (b) c))))
    (is (equal (ast-size a) 7))
    ;; Recomputation (hitting the cache) gives same result
    (is (equal (ast-size a) 7))))

(deftest ast-text-test ()
  (let ((*package* (find-package :resolve/test)))
    (is (equal (ast-text (astify '(x y))) "(X Y)"))
    (is (equal (ast-text (astify '(x . y))) "(X . Y)"))
    (is (equal (ast-text '("foo" "bar")) "foobar"))
    (is (equal (ast-text '("foo" . "bar")) "foo.bar"))))

(deftest print-simple-lisp-ast ()
  (with-standard-io-syntax
    (let ((*package* (find-package :resolve/ast-diff)))
      (is (equal (write-to-string (astify '(1 2))
                                  :readably nil)
                 "#<SIMPLE-LISP-AST :VALUE (1 2)>"))
      (is (equal (write-to-string (astify '(1 2)) :readably t :pretty nil)
                 "#S(SIMPLE-LISP-AST :CHILDREN (1 2 :NIL) :ORIGINAL (1 2) :HASH NIL :COST NIL :SIZE NIL)")))))

(deftest copy-test ()
  (is (equal (unastify (copy (astify '(x y)))) '(x y)))
  (is (equal (unastify (copy (astify '(x . y)))) '(x . y))))

(deftest put-inserts-before-deletes-test ()
  (is (equal (put-inserts-before-deletes
              '((:insert x) (:delete y) (:delete w)))
             '((:insert x) (:delete y) (:delete w))))
  (is (equal (put-inserts-before-deletes '((:delete y) (:insert x)))
             '((:insert x) (:delete y))))
  (is (equal (put-inserts-before-deletes
              '((:delete y) (:delete w) (:same z) (:insert v) (:insert x)))
             '((:delete y) (:delete w) (:same z) (:insert v) (:insert x)))))

;;; Functions for interactive testing and experimentation.
(defun do-populate (my your)
  "Build a population of resolutions of conflicts from the merge of MY and YOUR."
  (with-fixture javascript-abacus-variants
    (setf *population* (populate (converge (aget my *variants*)
                                           (aget :orig *variants*)
                                           (aget your *variants*)
                                           :conflict t)))))

(defun test-variant (variant my your)
  "One-off test of a variant."
  (let ((script (namestring (make-pathname
                             :directory (append +javascript-dir+
                                                '("abacus"))
                             :name "test"
                             :type "sh"))))
    (with-temp-file (bin)
      (phenome variant :bin bin)
      (multiple-value-bind (stdout stderr errno)
          (shell "~a ~a ~a"
                 script bin (mapconcat #'identity
                                       (mapcar [#'string-downcase #'symbol-name]
                                               (list my your))
                                       ","))
        (declare (ignorable errno stderr))
        (count-if {string= "PASS"} (split-sequence #\Newline stdout))))))

(defun do-populate-and-resolve (my-name your-name target)
  (assert (and (keywordp my-name) (keywordp your-name)) (my-name your-name)
          "MY and YOUR must be keywords indicating abacus variants.")
  (assert (null *population*) (*population*)
          "Population should be nil to run `do-populate-and-resolve'.")
  (setf *fitness-evals* 0)
  (nest
   (let ((*note-level* 0)))
   (with-fixture javascript-converge-conflict)
   (destructuring-bind (my old your)
       (mapcar {aget _ *variants*} (list my-name :orig your-name)))
   ;; Target is 6 + 5 = 11.
   (flet ((test (variant) (test-variant variant my-name your-name)))
     (note 1 "OLD:~S" (test old))
     (note 1 "YOUR:~S" (test your))
     (note 1 "MY:~S" (test my))
     (let ((*target-fitness-p* {= target}))
       (resolve #'test my old your :target target)))))

#+run
(do-populate-and-resolve :borders :min-lines)

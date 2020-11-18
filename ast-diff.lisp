;;; ast-diff.lisp --- diffs between ASTs and other tree structures
;;;
;;; The @code{resolve/ast-diff} library provides
;;; for the differencing of software objects.  A general tree
;;; differencing algorithm (originally adopted from
;;; @url{http://thume.ca/2017/06/17/tree-diffing/#a-tree-diff-optimizer})
;;; is used to difference tree genomes.  Optimizations are made which
;;; assume the common cases of differences typically found in software
;;; development in which most top-level subtrees have no differences.
;;; This library is used to define the @code{ast-diff} and
;;; @code{ast-merge} command-line executables.  These executables
;;; provide for AST-level differencing and may be used as a
;;; replacement for common line- or word-based differencing tools
;;; during software development.
;;;
;;; MERGE-DIFF-2 should hold the language-specific logic.
;;;
;;; MELD take conflicts and moves them up to higher levels.
;;;
;;; MERGE-DIFFS-ON-SYMS generic function which dispatches off of
;;; combinations of edit operations.  This should fail more frequently
;;; (generate more :CONFLICT nodes).  Could write new methods to
;;; override existing methods.
;;;
;;; @texi{ast-diff}
(defpackage :resolve/ast-diff
  (:use
   :gt/full
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/non-homologous-parseable
   :software-evolution-library/software/clang
   :software-evolution-library/software/simple
   :software-evolution-library/software/source
   :resolve/string
   :metabang-bind
   :cl-heap)
  (:shadowing-import-from :software-evolution-library/view
                          :+color-RED+ :+color-GRN+ :+color-RST+)
  (:shadowing-import-from :functional-trees
   :child-slots :slot-spec-slot
   :children-alist :slot-specifier)
  (:export
   :ast-cost
   :ast-size
   :ast-can-recurse
   :ast-diff
   :ast-diff-elide-same
   :ast-patch
   :astify
   :unastify
   :unastify-lisp-diff
   :print-diff
   ;; Merge functions
   :chunk
   :diff3
   :merge3
   :converge
   :show-chunks
   :merge-diffs-on-syms
   ;; Functions needed by alist.lisp
   :record-unstable
   :merge-diffs2
   :merge3
   ;; Edit tree symbols
   :create-edit-tree
   :create-and-print-edit-tree
   :edit-tree-node-base
   :edit-tree-node
   :edit-tree-node-script
   :edit-tree-node-children
   :print-edit-tree-node
   :print-edit-tree
   :describe-edit-tree-node
   :describe-edit-tree
   :map-edit-tree
   :ast-size
   :ast-to-list-form
   :*base-cost*
   :*max-wrap-diff*
   :*wrap*
   :*wrap-sequences*
   :*strings*
   ;; :*ignore-whitespace*
   ))
(in-package :resolve/ast-diff)
(in-readtable :curry-compose-reader-macros)
;;; Comments on further algorithm improvements
;;;
;;; The "good enough" algorithm could be made slightly better
;;; by allowing limited lookahead.  With k lookahead it could
;;; run in O(k max(m,n)) time, m and n the lengths of the sequences.
;;;
;;; The hash function has not been fully tuned for speed.
;;;
;;; RECURSIVE-DIFF has an UPPER-BOUND argument.  This is not used
;;; now, but could be used to speed up the slow part of the algorithm
;;; if we want an exact solution rather than the "good enough" solution.
;;; Use the cost of the "good enough" solution to provide an upper bound
;;; for the exact solution.   Also, recursive calls could provide their
;;; own upper bounds based on the optimum path to the node from which
;;; the call is being made.
;;;
;;; The dynamic programming algorithm uses O(mn) space.  It can be
;;; changed to use linear space.  There are various schemes in the
;;; literature for doing LCS and edit distance in linear space.
;;; A simple one is as follows: note that we can compute the length
;;; of the LCS (or the edit distance) in linear space by scanning
;;; the array keeping just two rows.  We cannot reconstruct the
;;; edit from this, but we can record which entry on the m/2 row
;;; was in the final optimal solution.  Once we have done that,
;;; the parts before and after that point can be recomputed
;;; recursively.
;;;
;;; The speed on C programs is now dominated by the time needed
;;; for Clang to parse the C and send the AST to Lisp.
;;;
;;; It may be useful to have a hash function on ASTs that produces
;;; smaller integers, and use ast-hash-with-check to handle collisions.
;;; This could be tied in with a general mechanism for hash consing
;;; of ASTs.


(declaim (special *cost-table*))

(defparameter *base-cost* 2
  "Basic cost of a diff, before adding costs of components.")

(defvar *ignore-whitespace* nil
  "If true, inserting or removing whitespace in a string has zero cost")

(defvar *strings* t
  "If true, descend into strings when computing diffs.")

(defvar *wrap* nil
  "If true, perform wrap/unwrap actions in diffs.")

(defvar *wrap-sequences* nil
  "If true, perform wrap-sequence/unwrap-sequence actions in diffs.")

(defvar *max-wrap-diff* 500
  "When *wrap* is true, this is the maximum size difference for
wrapping and unwrapping to be considered.")

(defun clength (x) (iter (while (consp x)) (pop x) (summing 1)))

(defgeneric ast-class (ast)
  (:documentation "This was previously defined in sel/software/ast"))

(defmethod ast-class ((x ast))
  (type-of x))

(defmethod ccost ((x cons))
  (let ((conses nil))
    (let ((y x))
      (iter (while (consp y))
            (push y conses)
            (pop y)))
    (let ((cost 1))
      (iter (while conses)
            (incf cost (ccost (car (pop conses)))))
      cost)))

(defmethod ccost ((x t)) 1)

;; #+sbcl (declaim (optimize sb-cover:store-coverage-data))

(defmethod clast (obj) (iter (while (consp obj)) (pop obj)))


;;; Interface functions.
(defgeneric ast-cost (ast)
  (:documentation "Return cost of AST."))

(defmethod ast-cost ((ast ast))
  (cl:reduce #'+ (children ast) :key #'ast-cost :initial-value 1))

(defmethod ast-cost ((ast non-homologous-ast))
  (cl:reduce #'+ (standardized-children ast) :key #'ast-cost :initial-value 1))

;; Slot-specifiers are introduced as markers in the standardized-children
;; lists of non-homologous-ast nodes, and should not contribute the
;; the cost.
(defmethod ast-cost ((ss slot-specifier)) 0)

(defmethod ast-cost ((ast t))
  1)

(defmethod ast-cost ((s string))
  (if *ignore-whitespace*
      (count-if-not #'whitespacep s)
      (length s)))

(defmethod ast-cost ((ast vector))
  (length ast))

(defmethod ast-cost ((ast null))
  1)

(defmethod ast-cost ((ast cons))
  (+ (iter (sum (ast-cost (pop ast)))
           (while (consp ast)))
     ;; cost of terminal NIL is 0
     (if ast (ast-cost ast) 0)))

(defgeneric ast-can-recurse (ast-a ast-b)
  (:documentation "Check if recursion is possible on AST-A and AST-B.  Strings
can be recursed on if STRINGS is true (defaults to true)"))

(defmethod ast-can-recurse ((ast-a string) (ast-b string))
  *strings*)
(defmethod ast-can-recurse ((ast-a t) (ast-b t))
  nil)
(defmethod ast-can-recurse ((ast-a ast) (ast-b ast))
  t)

(defmethod source-text ((ast cons) &optional stream)
  (iter (while (consp ast)) (source-text (pop ast) stream))
  (when ast
    (write-string "." stream)
    (source-text ast stream)))

(defmethod source-text ((ast slot-specifier) &optional stream)
  (write-sequence "" stream))

(defgeneric ast-to-list-form (ast)
  (:documentation "Convert ast into a more readable list form")
  (:method ((ast ast))
    (cons (ast-class ast)
          (mapcar #'ast-to-list-form (children ast))))
  (:method (ast) ast))

(defgeneric ast-size (node)
  (:documentation "Number of nodes and leaves in an AST or ast-like thing")
  (:method ((node ast))
    (reduce #'+ (children node) :key #'ast-size :initial-value 1))
  (:method ((node t)) 1))


;;; Wrapper for Lisp lists in simple ASTs
;;; This is so we don't need to confuse the ast-diff machinery
;;; by making it handle raw lisp lists, and so information
;;; like size and cost can be cached in the ast nodes
;;;

;;; Each SIMPLE-LISP-AST is a list (either proper or improper)
;;; The elements of CHILDREN are the proper elements of the list,
;;; and either the tail value (if the list is improper) or :NIL
;;; (if the list is proper).   This will collide on improper lists
;;; that end in :NIL, but there is nothing special about :NIL, so fix
;;; this up later.
(defclass simple-lisp-ast (functional-tree-ast)
  ((children :initarg :children :initform nil)
   (child-slots :initform '(children) :allocation :class)
   ;; Slots for caching
   (cost-cache :initarg :cost-cache
               :initform nil
               :accessor cost-cache)
   (size-cache :initarg :size-cache
               :initform nil
               :accessor size-cache)
   (unastify-cache :initarg :unastify-cache
                   :initform nil
                   :accessor unastify-cache)))

(defmethod ast-class ((ast simple-lisp-ast)) :list)
(defmethod source-text ((ast simple-lisp-ast) &optional stream)
  (let ((v (unastify ast)))
    (if v
        (format stream "~a" (unastify v))
        (write-string "()" stream))))
(defmethod equal? ((a simple-lisp-ast) (b simple-lisp-ast))
  (equalp (unastify a) (unastify b)))

(defmethod ast-cost :around ((ast simple-lisp-ast))
  (or (cost-cache ast)
      (setf (cost-cache ast) (call-next-method))))
(defmethod ast-size :around ((ast simple-lisp-ast))
  (or (size-cache ast)
      (setf (size-cache ast) (call-next-method))))
(defmethod copy :around ((obj simple-lisp-ast) &rest args
                         &key &allow-other-keys)
  (apply #'call-next-method obj
         :cost-cache nil :size-cache nil :unastify-cache nil
         args))

(defgeneric astify (x)
  (:documentation "Convert a Lisp data structure to a SIMPLE-LISP-AST"))
(defgeneric unastify (x)
  (:documentation "Convert a SIMPLE-LISP-AST to a Lisp data structure"))

(let ((end-marker :nil))
  (defmethod source-text ((x (eql end-marker)) &optional stream)
    (source-text "" stream))
  (defmethod astify ((x list))
    (if (proper-list-p x)
        ;; Add an end marker to represent the NIL
        ;; (because AST-DIFF treats NIL as a list)
        (make-instance 'simple-lisp-ast
         :children (nconc (mapcar #'astify x) (list end-marker))
         :unastify-cache x)
        ;; Properize the list
        (let ((original-x x)
              (properized-x
               (iter (collecting
                      (if (consp x)
                          (car x)
                          (progn
                            (assert (not (eql x end-marker)) ()
                                    "End marker ~s found"
                                    x)
                            x)))
                     (while (consp x))
                     (pop x))))
          (make-instance 'simple-lisp-ast
           :children (mapcar #'astify properized-x)
           :unastify-cache original-x))))
  (defmethod astify (x) x)
  (defmethod unastify ((ast simple-lisp-ast))
    (or (unastify-cache ast)
        (unastify-list (children ast))))
  (defmethod unastify (val) val)

  (defun unastify-list (c)
    (and c
         (let ((last-c (lastcar c)))
           (if (eql last-c end-marker)
               (mapcar #'unastify (butlast c))
               (nconc (mapcar #'unastify (butlast c))
                      last-c))))))

(defun unastify-lisp-diff (d)
  (typecase d
    (resolve/ast-diff::simple-lisp-ast (resolve/ast-diff::unastify d))
    (cons
     (cons (unastify-lisp-diff (car d))
           (unastify-lisp-diff (cdr d))))
    (t d)))

(defmethod print-object ((obj simple-lisp-ast) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream ":VALUE ~s" (unastify obj)))))


;;; Classes for "edit trees"
;;;
;;; It's useful to extract an "edit tree" from an edit script.
;;; The edit tree is a tree representation of the parts of the
;;; diff that group the edit, hierarchically, into subedits.
;;;
;;; Each node is a mapping from one edit segment in the original
;;; tree to another edit segment in the target tree.  An edit segment
;;; represents a piece of the tree that is being changed by the
;;; a part of the edit script.

(defclass size-mixin ()
  ((size :reader ast-size
         :documentation "Cache slot for AST-SIZE function"))
  (:documentation "Mixin to give a class a slot for caching
the AST-SIZE value"))

(defclass edit-segment-common (size-mixin)
  ((node ;; :type (or ast-node list) ;; Need LIST because we don't just do ASTs
         :initarg :node
         :accessor edit-segment-node
         :documentation "The tree node for which a subset
of the children (possibly empty) form this edit segment")
   (start :type integer
          :initarg :start
          :accessor edit-segment-start
          :documentation "The index (starting at 0) of the first
child in the edit segment"))
  (:documentation "Common slots of edit-segment classes"))

(defclass edit-segment (edit-segment-common)
  ((length :type integer
           :initarg :length
           :accessor edit-segment-length
           :documentation "The number of children (possibly zero)
in the edit segment"))
  (:documentation "An edit-segment represents a subset of the children
of a tree node"))

(defclass string-edit-segment (edit-segment-common)
  ((string-start :type integer
                 :initarg :string-start
                 :accessor string-edit-segment-string-start
                 :documentation "Start of the substring that is
the AST segment")
   (string-length :type integer
                  :initarg :string-length
                  :accessor string-edit-segment-string-length
                  :documentation "Length of the substring that is
the edit segment"))
  (:documentation "This is a special case, because there may be edits
that descend into the strings at the leafs of a tree.  These need
special representation as edit-segments, recording both the position
of the leaf in the children of its parent in the tree, and the position
of the substring inside the string."))

(defmethod source-text ((segment edit-segment) &optional stream)
  (let ((start (edit-segment-start segment))
        (len (edit-segment-length segment))
        (ast-node (edit-segment-node segment)))
    (mapc {source-text _ stream}
          (subseq (children ast-node) start (+ start len)))))

(defmethod source-text ((segment string-edit-segment) &optional stream)
  (with-slots (node start string-start string-length)
      segment
    (assert node)
    (write-string (elt (children node) start) stream
                  :start string-start
                  :end (+ string-start string-length))))

(defmethod ast-to-list-form ((segment string-edit-segment))
  (source-text segment))

(defmethod ast-to-list-form ((segment edit-segment))
  (let ((start (edit-segment-start segment))
        (len (edit-segment-length segment))
        (ast-node (edit-segment-node segment)))
    `(,(ast-class ast-node)
       ,@(unless (eql start 0) '(|...|))
       ,@(mapcar #'ast-to-list-form
                 (subseq (children ast-node)
                         start (+ start len)))
       ,@(unless (eql (+ start len)
                      (length (children ast-node)))
           '(|...|)))))

(defclass edit-tree-node-base (size-mixin)
  ((script :type list
           :initarg :script
           :accessor edit-tree-node-script
           :documentation "The part of the edit script that
applies to this part of the diff")
   (children :type list  ;; of edit-tree-node objects
             :initarg :children
             :initform nil
             :accessor edit-tree-node-children
             :documentation "Children, in left-to-right order
in the source tree, of this edit-tree-node"))
  (:documentation "Base class for edit tree nodes"))

(defclass edit-tree-node (edit-tree-node-base)
  ((source :type edit-segment-common
           :initarg :source
           :accessor edit-tree-node-source
           :documentation "Segment in the source tree being
rewritten by part of the edit script")
   (target :type edit-segment-common
           :initarg :target
           :accessor edit-tree-node-target
           :documentation "Segment in the target tree being
rewritten TO by part of the edit script"))
  (:documentation " "))

(defmethod print-object ((node edit-tree-node) stream)
  (let ((s (source-text (edit-tree-node-source node)))
        (bound 30))
    (when (> (length s) bound)
      (setf s (concatenate 'string (subseq s 0 (- bound 3)) "...")))
    (format stream "#<EDIT-TREE-NODE ~s>" s)))

(defmethod slot-unbound (class (node edit-tree-node) (slot (eql 'size)))
  (declare (ignorable class))
  (let ((value (reduce #'+ (edit-tree-node-children node)
                       :key #'ast-size :initial-value 1)))
    (setf (slot-value node slot) value)
    value))

(defmethod ast-size ((segment string-edit-segment)) 1)

;; Cache for SIZE slot, accessed by ast-size
(defmethod slot-unbound (c (segment edit-segment) (slot (eql 'size)))
  (declare (ignorable c))
  (let* ((node (edit-segment-node segment))
         (length (edit-segment-length segment))
         (start (edit-segment-start segment))
         (children (subseq (children node) start (+ start length)))
         (value (reduce #'+ children :key #'ast-size :initial-value 1)))
    (setf (slot-value segment slot) value)))


;;; Main interface to calculating ast differences.

;;; Macro for memoizing ast-diff-like methods

#+(or)
(defmacro defmethod-cached (name args &body body)
  (let* ((args args)
         (primary-args
          (iter (while args)
                (nest
                 (if (not (consp args))
                     (error "Args not a proper list"))
                 (if (symbolp (car args))
                     (if (member (car args) lambda-list-keywords)
                         (finish)
                         (collecting (pop args))))
                 (if (consp (car args))
                     (progn
                       (assert (symbolp (caar args)))
                       (collecting (car (pop args))))
                     (error "Bad argument: ~s" (car args))))))
         (cache (gensym "CACHE"))
         (counter (gensym "COUNTER"))
         (hash-upper-limit 50000000)
         (key (gensym "KEY"))
         (h (gensym "H"))
         (val-alist (gensym "VAL-ALIST"))
         (vals (gensym "VALS"))
         (p (gensym "P"))
         )
    `(let ((,cache (make-hash-table))
           (,counter 0))
       (declare (type 0 1000000000) ,counter)
       (defmethod ,name :around ,args
          (let* ((,key (list ,@primary-args))
                 (,h (ast-hash ,key)))
            (let* ((,val-alist (gethash ,h ,cache))
                   (,p (assoc ,key ,val-alist :test #'equal)))
              (nest
               (if p (let ((,vals (cadr ,p)))
                       (values (car ,vals) (cdr ,vals))))
               (if (>= (length ,val-alist) 10)
                   (call-next-method))
               (let ((return-values (multiple-value-list (call-next-method))))
                 (when (> ,counter ,hash-upper-limit)
                   (setf ,counter (thin-ast-diff-table ,cache ,counter)))
                 (push (cons (list* ,key return-values ,counter)
                             (gethash ,key ,cache)))
                 (incf ,counter)
                 (apply #'values return-values)))))))))



(defgeneric ast-diff* (ast-a ast-b)
  (:documentation
   "Return a least-cost edit script which transforms AST-A into AST-B.
Also return a second value indicating the cost of the edit.

See `ast-patch' for more details on edit scripts.

The following generic functions may be specialized to configure
differencing of specialized AST structures.; `equal?',
`ast-cost' and `ast-can-recurse'."))

(let ((ast-diff-cache (make-hash-table :size 1021))
      (ast-diff-counter 0)
      (hash-upper-limit 100000000))
  (declare (type (integer 0 2000000000) ast-diff-counter))
  ;; The cache maps key pairs to values and counts
  ;; The counter is incremented to find the most recent
  ;; cached entries.
  (defmethod ast-diff* :around (ast-a ast-b)
    (let* ((key (cons ast-a ast-b))
           (h (ast-hash key)))
      ;; val-alist maps keys to (val . count) pairs
      (let ((val-alist (gethash h ast-diff-cache)))
        (let ((p (assoc key val-alist :test #'equal)))
          (cond
            (p
             (let ((vals (cadr p)))
               (values (car vals) (cdr vals))))
            ((>= (length val-alist) 10)
             ;; If a bucket gets too big, just stop caching
             ;; things that map there
             (call-next-method))
            (t
             (multiple-value-bind (diff cost)
                 (call-next-method)
               ;; Check for insert, delete pairs
               ;; When we find one, replace with a :REPLACE edit
               (let* ((changed nil)
                      (e diff)
                      (new-diff
                        (iter (while e)
                              (cond
                                ((and (consp (car e))
                                      (consp (cadr e))
                                      (eql (caar e) :insert)
                                      (eql (caadr e) :delete))
                                 (setf changed t)
                                 (collecting
                                   `(:replace ,(cdr (cadr e)) ,(cdr (car e))))
                                 (pop e) (pop e))
                                (t (collecting (pop e)))))))
                 (when changed
                   (setf diff new-diff
                         cost (diff-cost new-diff))))
               (when (>= ast-diff-counter hash-upper-limit)
                 (setf ast-diff-counter
                       (thin-ast-diff-table ast-diff-cache ast-diff-counter)))
               (assert (< ast-diff-counter hash-upper-limit))
               (setf (gethash h ast-diff-cache)
                     (cons (list* key (cons diff cost) ast-diff-counter)
                           (gethash key ast-diff-cache)))
               (incf ast-diff-counter)
               (values diff cost))))))))

  (defun thin-ast-diff-table (cache counter)
    (assert (>= counter hash-upper-limit))
    (let* ((h2 (ash hash-upper-limit -1))
           (d (- counter h2)))
      (maphash (lambda (k v)
                 (let* ((head (cons nil v))
                        (p head)
                        (n (cdr p)))
                   (loop
                      (unless n (return))
                      (if (< (cddar n) d)
                          (setf (cdr p) (cdr n))
                          (progn
                            (decf (cddar n) d)
                            (setf p n n (cdr n)))))
                   (unless (eql (cdr head) v)
                     (setf (gethash k cache) (cdr head)))))
               cache)
      (setf counter h2)))

  (defun clear-ast-diff-table ()
    (setf ast-diff-counter 0)
    (clrhash ast-diff-cache))

  (defun ast-diff (ast-a ast-b
                   &key
                     ((:ignore-whitespace *ignore-whitespace*)
                      *ignore-whitespace*)
                     ((:strings *strings*) *strings*)
                     ((:wrap-sequences *wrap-sequences*) *wrap-sequences*)
                     ((:wrap *wrap*) (or *wrap* *wrap-sequences*))
                     ((:max-wrap-diff *max-wrap-diff*) *max-wrap-diff*)
                     ((:base-cost *base-cost*) *base-cost*)
                     &allow-other-keys)
    ;; Convert raw lisp data to asts
    (let ((ast-a (astify ast-a))
          (ast-b (astify ast-b)))
      ;; Bag computation to accelerate wrapping/unwrapping will go here
      (unwind-protect
           (ast-diff* ast-a ast-b)
        (clear-ast-diff-table)))))

(defmethod ast-diff* ((ast-a ast) (ast-b ast))
  #+debug (format t "ast-diff[AST] AST-CAN-RECURSE: ~S~%"
                  (ast-can-recurse ast-a ast-b))
  (let (diff cost)
    (when (eql (ast-class ast-a) (ast-class ast-b))
      (setf (values diff cost)
            (ast-diff*-lists (standardized-children ast-a)
                             (standardized-children ast-b)
                             ast-a ast-b)))
    (when *wrap*
      (multiple-value-bind (wrap-diff wrap-cost)
          (ast-diff-wrap ast-a ast-b)
        (when (and wrap-cost (or (null cost) (< wrap-cost cost)))
          (setf diff wrap-diff
                cost wrap-cost)))
      (multiple-value-bind (unwrap-diff unwrap-cost)
          (ast-diff-unwrap ast-a ast-b)
        (when (and unwrap-cost (or (null cost) (< unwrap-cost cost)))
          (setf diff unwrap-diff
                cost unwrap-cost))))
    (if diff
        (values diff cost)
        (call-next-method))))

(defun map-ast-while-path (ast fn &optional path)
  "Apply FN to the nodes of AST A, stopping
the descent when FN returns NIL.  FN is also passed a PATH
argument, which is a list (in reverse order) of the indices
of children leading down to the node."
  (when (funcall fn ast path)
    (iter (for c in (children ast))
          (for i from 0)
          (when (typep c 'ast) (map-ast-while-path c fn (cons i path))))))

(defgeneric ast-diff-wrap (ast-a ast-b &key skip-root first-ast-child)
  (:documentation
   "Find a minimum cost 'wrap' edit, which wraps an AST in a larger ast"))

(defmethod ast-diff-wrap ((ast-a ast) (ast-b ast)
                          &key (skip-root t) first-ast-child)
  ;; search over the ASTs under ast-b that are the same class as ast-a,
  ;; and for which the size difference is not too large
  (let* ((ast-a-cost (ast-cost ast-a))
         (a-class (ast-class ast-a))
         (max-wrap-diff *max-wrap-diff*)
         (max-cost (+ ast-a-cost max-wrap-diff))
         (min-cost (- ast-a-cost max-wrap-diff))
         (best-candidate nil)
         (best-cost most-positive-fixnum)
         ;; Do not also search for wraps in the recursive calls
         ; (*wrap* nil)
         )
    (when (integerp first-ast-child)
      (setf first-ast-child (elt (children ast-a) first-ast-child)))
    #+ast-diff-wrap-debug (format t "(ast-class ast-a) = ~S~%" a-class)
    (nest
     (map-ast-while-path ast-b)
     (lambda (x path) #+ast-diff-wrap-debug (format t "Path = ~A~%" path))
     (if (and (null path) skip-root) t)
     (let ((x-cost (ast-cost x))))
     (cond
       ;; If X is too small, stop search down into it
       ((< x-cost min-cost)
        #+ast-diff-wrap-debug (format t "~a is too small~%" x)
        nil)
       ;; If X is too large, skip it but keep searching
       ((> x-cost max-cost)
        #+ast-diff-wrap-debug (format t "~a is too big~%" x)
        t)
       ;; If X is not the right class, also skip it
       ((not (eql (ast-class x) a-class))
        #+ast-diff-wrap-debug (format t "~a is wrong class~%" x)
        t)
       ;; If the first AST child is not found in the list of children
       ;; with a "good" match, skip
       ((and first-ast-child (not (ast-child-check first-ast-child x)))
        t))
     ;; Only if the size is in the right range, and the
     ;; ast-class matches, do we try to insert here
     (t) (multiple-value-bind (diff cost) (ast-diff* ast-a x))
     (when (< cost best-cost))
     (multiple-value-bind (left-wrap right-wrap classes)
         (wraps-of-path ast-b (reverse path)))
     (let ((total-cost (+ cost
                          (cost-of-wrap left-wrap)
                          (cost-of-wrap right-wrap))))
       (when (< total-cost best-cost)
         #+ast-diff-wrap-debug
         (progn
           (format t "Wrap candidate found~%")
           (format t "Cost = ~a~%" total-cost)
           (format t "ast-a = ~s~%" (ast-to-list-form ast-a))
           (format t "ast-b = ~s~%" (ast-to-list-form ast-b))
           (format t "diff = ~s~%" diff)
           (format t "path = ~s~%" path)
           (format t "left-wrap = ~s~%" left-wrap)
           (format t "right-wrap = ~s~%" right-wrap)
           (format t "classes = ~s~%" classes))
         (setf best-cost total-cost
               best-candidate (list :wrap diff path left-wrap right-wrap classes
                                    ast-b)))))
    (when best-candidate
      (values best-candidate best-cost))))

(defgeneric ast-diff-unwrap (ast-a ast-b &key skip-root)
  (:documentation "Find a minimum cost 'unwrap' edit, which pulls a subast
out of one tree and turns it into another."))

(defmethod ast-diff-unwrap ((ast-a ast) (ast-b ast)
                            &key (skip-root t) first-ast-child)
  ;; search over the ASTs under ast-a that are the same class as ast-b,
  ;; and for which the size difference is not too large
  (let* ((ast-b-cost (ast-cost ast-b))
         (b-class (ast-class ast-b))
         (max-wrap-diff *max-wrap-diff*)
         (max-cost (+ ast-b-cost max-wrap-diff))
         (min-cost (- ast-b-cost max-wrap-diff))
         (best-candidate nil)
         (best-cost most-positive-fixnum)
         ;; Do not also search for wraps in the recursive call
         (*wrap* nil))
    (when (integerp first-ast-child)
      (setf first-ast-child (elt (children ast-b) first-ast-child)))
    #+ast-diff-unwrap-debug (format t "(ast-class ast-b) = ~S~%" b-class)
    (nest
     (map-ast-while-path ast-a)
     (lambda (x path) #+ast-diff-unwrap-debug (format t "Path = ~A~%" path))
     (if (and (null path) skip-root) t)
     (let ((x-cost (ast-cost x))))
     (cond
       ;; If X is too small, stop search down into it
       ((< x-cost min-cost)
        #+ast-diff-unwrap-debug (format t "~a is too small~%" x)
        nil)
       ;; If X is too large, skip it but keep searching
       ((> x-cost max-cost)
        #+ast-diff-unwrap-debug (format t "~a is too big~%" x)
        t)
       ;; If X is not the right class, also skip it
       ((not (eql (ast-class x) b-class))
        #+ast-diff-unwrap-debug (format t "~a is wrong class~%" x)
        t)
       ;; If the first AST child is not found in the list of children
       ;; with a "good" match, skip
       ((and first-ast-child (not (ast-child-check first-ast-child x t)))
        t))
     ;; Only if the size is in the right range, and the
     ;; ast-class matches, do we try to insert here
     (t)
     (multiple-value-bind (diff cost) (ast-diff* x ast-b))
     (when (< cost best-cost)
       (multiple-value-bind (left-wrap right-wrap)
           (wraps-of-path ast-a (reverse path))
         (let ((total-cost (+ cost
                              *base-cost*
                              (cost-of-wrap left-wrap)
                              (cost-of-wrap right-wrap))))
           (when (< total-cost best-cost)
             (let ((new (list :unwrap diff (reverse path) left-wrap right-wrap)))
               #+ast-diff-unwrap-debug
               (format t "Replace~%~a (~a)~%with~%~a (~a)~%"
                       best-candidate best-cost
                       new total-cost)
               (setf best-cost total-cost
                     best-candidate new)))))))
    (when best-candidate
      (values best-candidate best-cost))))

(defgeneric ast-child-check (a b &optional reverse?)
  (:documentation
   "Check that A is 'close enough' to some child of b")
  (:method ((a ast) (b ast) &optional reverse?)
    (let* ((a-cost (ast-cost a))
           (cost-limit (floor (* *base-cost* a-cost 1/2))))
      (some (lambda (c)
              (and (typep c 'ast)
                   (<= 1/2 (/ a-cost (ast-cost c)) 3/2)
                   ;; REVERSE? is used so we don't compute ast-diffs backwards
                   ;; when checking for UNWRAP
                   (< (nth-value 1 (if reverse? (ast-diff* c a) (ast-diff* a c))) cost-limit)))
            (children b))))
  (:method ((a t) (b t) &optional reverse?) (declare (ignore reverse?))
    (equal? a b)))

(defun wraps-of-path (ast path)
  "Computes lists of children that lie on the left and right sides of a path
down from AST, as well as the classes of the nodes along the path."
  ;; (format t "Wraps of PATH = ~s in ~s~%" path (ast-to-list-form ast))
  (let (left right classes)
    (iter (while path)
          (assert (typep ast 'ast))
          (let ((c (children ast))
                (i (pop path)))
            (assert (<= 0 i))
            (assert (< i (length c)))
            (push (ast-class ast) classes)
            (push (subseq c 0 i) left)
            (push (subseq c (1+ i)) right)
            (setf ast (elt c i))
            (assert (typep ast 'ast))))
    (setf left (reverse left)
          right (reverse right)
          classes (reverse classes))
    #+ast-diff-debug (format t "Result: ~s ~s ~s~%" left right classes)
    (values left right classes)))

(defun cost-of-wrap (wrap)
  "Computes the sum of the costs of the objects in a wrap"
  (reduce #'+ wrap :initial-value 0
          :key (lambda (w) (reduce #'+ w :key #'ast-cost :initial-value *base-cost*))))

(defmethod ast-diff-wrap ((ast-a t) (ast-b t) &key skip-root first-ast-child)
  (declare (ignore skip-root first-ast-child))
  nil)

(defmethod ast-diff-unwrap ((ast-a t) (ast-b t) &key skip-root first-ast-child)
  (declare (ignore skip-root first-ast-child))
  nil)

(defun ast-diff-wrap-sequence (ast-a sub-a ast-b)
  (let ((len (length sub-a)))
    (assert (>= len 2))
    (let ((sub-ast (copy ast-a :children (coerce sub-a 'list)))
          (*wrap-sequences* nil)
          (*wrap* nil)
          (first-ast-child (position-if {typep _ 'ast} sub-a)))
      (let ((diff (ast-diff-wrap sub-ast ast-b :skip-root nil
                                 :first-ast-child first-ast-child)))
        (if (consp diff)
            (let ((new-diff `(:wrap-sequence ,len ,@(cdr diff))))
              (values new-diff (diff-cost new-diff)))
            (values :bad most-positive-fixnum))))))

(defun ast-diff-unwrap-sequence (a ast-b sub-b)
  (let ((len (length sub-b)))
    (assert (>= len 2))
    (let ((sub-ast (copy ast-b :children (coerce sub-b 'list)))
          (*wrap-sequences* nil)
          (*wrap* nil)
          (first-ast-child (position-if {typep _ 'ast} sub-b)))
      (let ((diff (ast-diff-unwrap a sub-ast :skip-root nil
                                   :first-ast-child first-ast-child)))
        (if (consp diff)
            (let ((new-diff (cons :unwrap-sequence (cdr diff))))
              (values new-diff (diff-cost diff)))
            (values :bad most-positive-fixnum))))))

#|
(defun ast-diff-wrap-sequence (ast-a sub-a ast-b)
  "Search for the best wrap of the children SUB-A of AST-A inside
AST-B.  If none are acceptable, return :BAD"
  ;; Search inside SUB-A for sequences of children of a node of Sort (AST-CLASS AST-A)
  ;; that meet some criterion
  (let* ((len (length sub-a))
         (best-candidate nil)
         (best-cost most-positive-fixnum))
    (assert (>= len 2))
    (nest
     (let ((a-class (ast-class ast-a)
           (limit (- (reduce #'+ sub-a :key #'ast-cost) *max-wrap-diff*))
           (*wrap-sequence* nil)
           (sub-ast-a (copy ast-a :children (coerce sub-a 'list)))))
     (map-ast-while-path ast-b)
     (lambda (x path)  #+ast-diff-debug (format t "Path = ~A~%" path))
     ;; Abort descent if the subtree is too small
     (if (and (> limit 0) (< (ast-cost x) limit)) nil)
     (if (not (eql (ast-class x) a-class)) t)
     (multiple-value-bind (diff raw-cost) (ast-diff* sub-ast-a b))

     (let ((cost (+ cost prefix-cost postfix-cost))))
     (progn
       (when (< cost best-cost)
         (setf best-cost cost)
         (multiple-value-bind (left-wrap rightwrap classes)
             (wraps-of-path ast-b (reverse path))
           (setf best-candidate
                 `(:wrap-sequence ,len ,diff ,path ,left-wrap
                                  ,right-wrap ,classes ,ast-a))))
       t))
    ;; At this point, we've found the best candidate.  Recompute the cost
    (values best-candidate (diff-cost best-candidate))))
|#


(defun remove-common-prefix-and-suffix (list-a list-b)
  "Return unique portions of LIST-A and LIST-B less shared prefix and postfix.
Prefix and postfix returned as additional values."
  ;; Just return the input lists immediately if not proper lists.
  (unless (and (consp list-a) (consp list-b)
               (proper-list-p list-a) (proper-list-p list-b))
    (return-from remove-common-prefix-and-suffix
      (values list-a list-b nil nil)))
  (labels ((prefix (list-a list-b)
             (iter (for a in list-a)
                   (for b in list-b)
                   (cond ((and (typep a 'ast) (typep b 'ast)
                               (equal? a b))
                          (collect a into common))
                         ((and (not (typep a 'ast)) (not (typep b 'ast))
                               (equalp a b))
                          (collect a into common))
                         (t (return common))))))
    (let* ((prefix (prefix list-a list-b))
           (pre-length (length prefix))
           (a (drop pre-length list-a))
           (b (drop pre-length list-b)))
      ;; If either list is completely consumed by the prefix, return here.
      (if (or (null a) (null b))
          (values a b prefix nil)
          ;; Calculate the postfix (less the prefix) if necessary.
          (let* ((postfix (prefix (reverse a) (reverse b)))
                 (post-length (length postfix)))
            (values (butlast a post-length)
                    (butlast b post-length)
                    prefix
                    (reverse postfix)))))))

(defun make-cache (total-a total-b)
  (make-array (list (1+ (clength total-a)) (1+ (clength total-b)))
              :initial-element nil))

;;; Simple queue.  This must be implemented in a library somewhere
;;; in Quicklisp.
(defun make-simple-queue ()
  (cons nil nil))

(defun simple-queue-dequeue (sq)
  (cond
    ((car sq) (pop (car sq)))
    ((cdr sq)
     (let ((r (nreverse (cdr sq))))
       (setf (car sq) (cdr r)
             (cdr sq) nil)
       (car r)))
    (t nil)))

(defun simple-queue-enqueue (sq val)
  (push val (cdr sq)))

(defstruct rd-node
  "Node in the recursive-diff computation graph"
  ;; Coordinates of the node in the r-d graph
  (a 0 :type (integer 0))
  (b 0 :type (integer 0))
  (in-arcs nil :type list) ;; list of arcs into this node
  (out-arcs nil :type list) ;; list of arcs out of this node
  (open-pred-count 0 :type (integer 0)) ;; number of predecessors that are still open
  (best-in-arc nil) ;; The in arc that gave the lowest cost to this point
  (cost 0) ;; total cost to reach this node along best path
  )

(defmethod print-object ((node rd-node) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (node stream :type t :identity t)
        (format stream ":A ~a :B ~a"
                (rd-node-a node) (rd-node-b node)))))

(defstruct rd-link
  "Link in the computation graph for edits on sequences"
  (src (required-argument :src)) ;; a b ;; indices of source node
  (dest (required-argument :dest)) ;; destination node
  (cost nil) ;; Cost of this operation
  ;; The kind of edit operation to corresponding to the link
  (kind (required-argument :kind))
  ;; The actual edit operation on this arc
  (op nil))

(defmethod print-object ((arc rd-link) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (arc stream)
        (format stream "~a ~a ~s ~a"
                (rd-link-src arc)
                (rd-link-dest arc)
                (rd-link-kind arc)
                (rd-link-cost arc)))))

(defun rd-link-a (e) (rd-node-a (rd-link-src e)))
(defun rd-link-b (e) (rd-node-b (rd-link-src e)))

(defun build-rd-graph (total-a total-b &key (wrap-sequences *wrap-sequences*)
                                         (unwrap-sequences *wrap-sequences*))
  ;; Construct the graph of a pair of lists.  The graph will have
  ;; (* (1+ (length total-a)) (1+ (length total-b))) nodes
  ;; Return the 2d array of the nodes
  (let* ((len-a (length total-a))
         (len-b (length total-b))
         (nodes (make-array (list (1+ len-a) (1+ len-b))))
         (vec-a (coerce total-a 'vector))
         (vec-b (coerce total-b 'vector)))
    ;; For now, we eagerly construct all possible wrap/unwrap edges
    ;; Prune these if they take too long
    (iter (for a from 0 to len-a)
          (iter (for b from 0 to len-b)
                (let ((node (make-rd-node :a a :b b)))
                  (when (> b 0)
                    (push (make-rd-link ;; :a a :b (1- b)
                           :src (aref nodes a (1- b))
                           :dest node
                           :kind :insert)
                          (rd-node-in-arcs node)))
                  (when (> a 0)
                    (push (make-rd-link
                           ;; :a (1- a) :b b
                             :src (aref nodes (1- a) b)
                             :dest node :kind :delete)
                          (rd-node-in-arcs node))
                    (when (> b 0 ) ;; (and (> b 0) (ast-can-recurse (aref vec-a (1- a))
                                   ;;                     (aref vec-b (1- b))))
                      (push (make-rd-link
                             ;; :a (1- a) :b (1- b)
                             :src (aref nodes (1- a) (1- b))
                             :dest node
                             :kind :same-or-recurse)
                            (rd-node-in-arcs node))))
                  (when wrap-sequences
                    (when (and (> b 0) (> a 1))
                      (iter (for x from 0 to (- a 2))
                            (push (make-rd-link
                                   ;; :a x :b b
                                   :src (aref nodes x (1- b))
                                   :dest node
                                   :kind :wrap-sequence)
                                  (rd-node-in-arcs node)))))
                  (when unwrap-sequences
                    (when (and (> a 0) (> b 1))
                      (iter (for x from 0 to (- b 2))
                            (push (make-rd-link
                                   ;; :a a :b x
                                   :src (aref nodes (1- a) x)
                                   :dest node
                                   :kind :unwrap-sequence)
                                  (rd-node-in-arcs node)))))
                  (setf (rd-node-open-pred-count node)
                        (length (rd-node-in-arcs node)))
                  (setf (aref nodes a b) node))))
    ;; Fill in out-arcs
    (iter (for a from 0 to len-a)
          (iter (for b from 0 to len-b)
                (let* ((node (aref nodes a b))
                       (in-arcs (reverse (rd-node-in-arcs node))))
                  (iter (for ia in in-arcs)
                        (let ((pa (rd-link-a ia))
                              (pb (rd-link-b ia)))
                          (push ia (rd-node-out-arcs (aref nodes pa pb))))))))

    (values nodes vec-a vec-b)))

(defun reconstruct-path-to-node (nodes node)
  (assert (rd-node-p node))
  (assert (typep nodes '(array * (* *))))
  (let ((ops nil))
    (iter (let ((pred-arc (rd-node-best-in-arc node)))
            (while pred-arc)
            (setf ops (append (rd-link-op pred-arc) ops))
            ;; (push (rd-link-op pred-arc) ops)
            (setf node (aref nodes
                             (rd-link-a pred-arc)
                             (rd-link-b pred-arc)))))
    (assert (eql (rd-node-a node) 0))
    (assert (eql (rd-node-b node) 0))
    ops))

(defun compute-best-paths (nodes vec-a vec-b parent-a parent-b)
  (let ((fringe (make-simple-queue))
        (total-open 1))
    ;; Start at the (0,0) node, which is the () -> ()
    ;; diff and has zero cost
    (let ((start (aref nodes 0 0)))
      (setf (rd-node-cost start) 0)
      (simple-queue-enqueue fringe start))
    (do ((node (simple-queue-dequeue fringe)
               (simple-queue-dequeue fringe)))
        ((zerop total-open) nil)
      (decf total-open)
      ;; Compute the costs of all arcs into this node
      (dolist (in-arc (rd-node-in-arcs node))
        (assert (eql (rd-link-dest in-arc) node))
        (compute-arc-cost in-arc nodes vec-a vec-b parent-a parent-b)
        (let ((best (rd-node-best-in-arc node)))
          #+ast-diff-debug (format t "best = ~a~%" best)
          (when (or (null best)
                    (> (rd-node-cost node)
                       (+ (rd-node-cost (rd-link-src in-arc))
                          (rd-link-cost in-arc))))
            #+ast-diff-debug
            (progn
              (format t "a = ~a, b = ~a~%"
                      (rd-link-a in-arc)
                      (rd-link-b in-arc))
              (format t "node cost = ~a~%" (rd-node-cost node))
              (if best
                  (format t "Replace~%~a (~a)(~a)~%with~%~a (~a)(~a)~%"
                          (rd-link-op best) (rd-link-cost best)
                          (+ (rd-node-cost (rd-link-src best))
                             (rd-link-cost best))
                          (rd-link-op in-arc) (rd-link-cost in-arc)
                          (+ (rd-node-cost (rd-link-src in-arc))
                             (rd-link-cost in-arc))
                          )
                  (format t "Best ~a (~a)~%" (rd-link-op in-arc) (rd-link-cost in-arc))))
            (setf (rd-node-best-in-arc node) in-arc
                  (rd-node-cost node) (+ (rd-link-cost in-arc)
                                         (rd-node-cost (rd-link-src in-arc)))))))
      ;; See if any successor are now ready to be handled
      (dolist (out-arc (rd-node-out-arcs node))
        (let ((dest (rd-link-dest out-arc)))
          (when (zerop (decf (rd-node-open-pred-count dest)))
            ;; All predecessors have been computed, queue this node
            (simple-queue-enqueue fringe dest)
            (incf total-open))))
      )))

(defun compute-arc-cost (arc nodes vec-a vec-b parent-a parent-b)
  "Compute the cost of an RD arc"
  (declare (ignorable nodes))
  (let* ((src (aref nodes (rd-link-a arc) (rd-link-b arc)))
         (dest (rd-link-dest arc))
         (dest-a (rd-node-a dest))
         (dest-b (rd-node-b dest))
         (a (when (> dest-a 0) (aref vec-a (1- dest-a))))
         (b (when (> dest-b 0) (aref vec-b (1- dest-b)))))
    (let ((op
           (ecase (rd-link-kind arc)
             (:insert (assert b) (list (cons :insert b)))
             (:delete (assert a) (list (cons :delete a)))
             (:same-or-recurse
              (assert a)
              (assert b)
              (cond ((equal? a b)
                     (list (cons :same a)))
                    ((not (ast-can-recurse a b))
                     (list (list :replace a b)))
                    (t
                     ;; Recursive -- we exploit the caching :around method
                     ;; of ast-diff*
                     ;; HACK -- if this is a full replacement, just splice it in
                     (let ((diff (ast-diff* a b)))
                       (cond
                         ((equal diff `((:replace ,a ,b)))
                          diff)
                         ((and (eql (length diff) 2)
                               (null (set-exclusive-or diff `((:delete . ,a) (:insert . ,b))
                                                       :test #'equal)))
                          `((:replace ,a ,b)))
                         (t
                          (list (cons :recurse (ast-diff* a b)))))))))
             (:wrap-sequence
              (assert (> dest-a 1))
              (assert (> dest-b 0))
              (let* ((len (- dest-a (rd-node-a src)))
                     (src-a (rd-node-a src))
                     (sub-a (subseq vec-a src-a (+ src-a len))))
                (list (ast-diff-wrap-sequence parent-a sub-a b))))
             (:unwrap-sequence
              (assert (> dest-a 0))
              (assert (> dest-b 1))
              (let* ((len (- dest-b (rd-node-b src)))
                     (src-b (rd-node-b src))
                     (sub-b (subseq vec-b src-b (+ src-b len))))
                (list (ast-diff-unwrap-sequence a parent-b sub-b)))))))
      (setf (rd-link-op arc) op)
      (values op (setf (rd-link-cost arc) (reduce #'+ op :key #'diff-cost :initial-value 0)))
      )))

(defun recursive-diff (total-a total-b parent-a parent-b)
  (flet ((%r (w uw)
           (multiple-value-bind (nodes vec-a vec-b)
               (build-rd-graph total-a total-b
                               :wrap-sequences w
                               :unwrap-sequences uw)
             (compute-best-paths nodes vec-a vec-b parent-a parent-b)
             (reconstruct-path-to-node nodes (aref nodes (length vec-a) (length vec-b))))))
    (let ((without-ws-diff (%r nil nil)))
      ;; Only try wrap-sequences if enabled and (un)wrapping actually helped
      (let ((w nil)
            (uw nil)
            (i 0) (j 0)
            #+ast-diff-wrap-sequence-debug
            (entered t))
        (when *wrap-sequences*
          (iter (for d in without-ws-diff)
                (while (or (not w) (not uw)))
                (when (consp d)
                  (case (car d)
                    ((:same :replace) (incf i) (incf j))
                    (:delete (incf i))
                    (:insert (incf j))
                    (:recurse
                     (case (cadr d)
                       (:wrap
                        (let* ((wrapped-cost (diff-cost (caddr d)))
                               (from (elt total-a i))
                               (from-cost (ast-cost from)))
                          (when (<= wrapped-cost from-cost)
                            (setf w t)
                            #+ast-diff-wrap-sequence-debug
                            (let* ((cost (diff-cost (list d)))
                                   (to (elt total-b j))
                                   (to-cost (ast-cost to)))
                              (when entered (format t "Enter~%"))
                              (setf entered nil)
                              (format t ":WRAP found, ~a/~a cost ~a~%"
                                      i j cost)
                              (format t "From (~a):~%~a~%"
                                      from-cost
                                      (source-text from))
                              (format t "To (~a):~%~a~%"
                                      to-cost
                                      (source-text to)))
                            )))
                       (:unwrap
                        (let* ((unwrapped-cost (diff-cost (caddr d)))
                               (to (elt total-b j))
                               (to-cost (ast-cost to)))
                          (when (<= unwrapped-cost to-cost)
                            (setf uw t)
                            #+ast-diff-unwrap-sequence-debug
                            (let* ((cost (diff-cost (list d)))
                                   (from (elt total-a i))
                                   (from-cost (ast-cost from)))
                              (when entered (format t "Enter~%"))
                              (setf entered nil)
                              (format t ":UNWRAP found, ~a/~a cost ~a~%"
                                      i j cost)
                              (format t "From (~a):~%~a~%"
                                      from-cost
                                      (source-text from))
                              (format t "To (~a):~%~a~%"
                                      to-cost
                                      (source-text to)))
                            ))))
                     (incf i) (incf j))))))
        (if (or w uw)
            (%r w uw)
            without-ws-diff)))))

(defun diff-cost (diff &aux (base-cost *base-cost*))
  "Computes the cost of a diff"
  (cond
    ((eql diff :bad) most-positive-fixnum)
    ((not (consp diff)) 0)
    ((symbolp (car diff))
     (ecase (car diff)
       (:bad most-positive-fixnum)
       (:insert (+ base-cost (ast-cost (cdr diff))))
       (:delete (+ base-cost (if (cdr diff) (ast-cost (cdr diff)) 1)))
       (:replace (+ base-cost (ast-cost (cadr diff)) (ast-cost (caddr diff))))
       ((:recurse-tail :recurse) (diff-cost (cdr diff)))
       ((:insert-sequence :delete-sequence)
        (+ base-cost
           (if (consp (cdr diff))
               (reduce #'+ (cdr diff) :key #'diff-cost :initial-value base-cost)
               1)))
       ((:same :same-tail :same-sequence) 0)
       ((:wrap :unwrap :unwrap-sequence)
        (if (not (consp (cdr diff)))
            most-positive-fixnum
            (+ base-cost
               (diff-cost (second diff))
               (cost-of-wrap (fourth diff))
               (cost-of-wrap (fifth diff)))))
       ((:wrap-sequence)
        (if (not (consp (cdr diff)))
            most-positive-fixnum
            (+ base-cost
               (diff-cost (third diff))
               (cost-of-wrap (fifth diff))
               (cost-of-wrap (sixth diff)))))
       ))
    (t
     (reduce #'+ diff :key #'diff-cost :initial-value 0))))

(defun ast-diff-on-lists (ast-a ast-b parent-a parent-b)
  (assert (proper-list-p ast-a))
  (assert (proper-list-p ast-b))
  ;; Drop common prefix and postfix, just run the diff on different middle.
  (multiple-value-bind (unique-a unique-b prefix postfix)
      (remove-common-prefix-and-suffix ast-a ast-b)
    ;; NOTE: We assume that the top level is a list (not a cons tree).
    ;; This is true for any ASTs parsed from a source file as a
    ;; sequence of READs.
    (labels ((add-common (diff cost)
               ;; Some special handling is required to interface
               ;; between the list model of the common pre-/post-fixes
               ;; and the cons-tree model of the calculated diff.
               ;;
               ;; This leads to unnecessary hair.  Fix!
               #+ast-diff-debug (format t "add-common: ~a ~a~%" diff cost)
               (values
                (let ((diff (if (equal '(:same-tail) (lastcar diff))
                                (butlast diff)
                                diff)))
                  (let ((diff
                         (if prefix
                             (append (mapcar (lambda (it) (cons :same it))
                                             prefix)
                                     diff)
                             diff)))
                    (if postfix
                        (append diff
                                (mapcar (lambda (it) (cons :same it))
                                        postfix))
                        diff)))
                cost)))
      (unless (or unique-a unique-b)
        (return-from ast-diff-on-lists
          (multiple-value-call #'add-common
            (values nil 0))))
      (when (null unique-a)
        (return-from ast-diff-on-lists
          (multiple-value-call #'add-common
            (values (mapcar (lambda (el) (cons :insert el)) unique-b)
                    (1- (ccost unique-b)))))) ; 1- for trailing nil.
      (when (null unique-b)
        (return-from ast-diff-on-lists
          (multiple-value-call #'add-common
            (values (mapcar (lambda (el) (cons :delete el)) unique-a)
                    (1- (ccost unique-a)))))) ; 1- for trailing nil.

      (let ((rdiff (recursive-diff unique-a unique-b parent-a parent-b)))
        (add-common rdiff (diff-cost rdiff))))))

(defun ast-hash-with-check (ast table)
  "Calls AST-HASH, but checks that if two ASTs have the same hash value,
they are actually equal.  If not, the second one gets a new, fresh hash
value that is used instead."
  (let* ((hash (ast-hash ast))
         (old-ast (gethash hash table)))
    (when (and old-ast (not (equal? ast old-ast)))
      (iter (incf hash) ; may be >= sel/sw/parseable::+ast-hash-base+, but that's ok
            (while (gethash hash table)))
      (setf (gethash hash table) ast))
    hash))

(defmethod ast-diff* (ast-a ast-b)
  #+debug (format t "ast-diff[T] ~S~%" (mapcar #'class-of (list ast-a ast-b)))
  (if (equal ast-a ast-b)
      (values `((:same . ,ast-a)) 0)
      (values `((:replace ,ast-a ,ast-b)) ; `((:insert . ,ast-b) (:delete . ,ast-a))
              (+ *base-cost* (ast-cost ast-a) (ast-cost ast-b)))))

(defmethod ast-diff* ((ast-a list) (ast-b list))
  (ast-diff*-lists ast-a ast-b nil nil))

(defgeneric ast-diff*-lists (list-a list-b parent-a parent-b))

(defmethod ast-diff*-lists ((ast-a list) (ast-b list) parent-a parent-b)
  #+ast-diff-debug (format t "ast-diff LIST LIST~%")
  (iter (for a in (list ast-a ast-b))
        (assert (proper-list-p a) () "Not a proper list: ~a" a))
  #+ast-diff-debug (format t "ast-a = ~a~%ast-b = ~a~%" ast-a ast-b)
  (let* ((table (make-hash-table))
         (hashes-a (mapcar (lambda (ast) (ast-hash-with-check ast table))
                           ast-a))
         (hashes-b (mapcar (lambda (ast) (ast-hash-with-check ast table))
                           ast-b))
         (subseq-triples (good-common-subsequences2 hashes-a hashes-b))
         diff-a common-a diff-b common-b)
    ;; split ast-a and ast-b into subsequences
    ;; Get lists of subsequences on which they differ, and subsequences on
    ;; which they are equal.  Some of the former may be empty.
    (setf (values diff-a common-a)
          (split-into-subsequences
           ast-a
           (mapcar (lambda (x) (list (car x) (caddr x)))
                   subseq-triples)))
    (setf (values diff-b common-b)
          (split-into-subsequences
           ast-b
           (mapcar (lambda (x) (list (cadr x) (caddr x)))
                   subseq-triples)))
    (assert (= (length diff-a) (length diff-b)))
    (assert (= (length common-a) (length common-b)))
    ;; (assert (= (length diff-a) (1+ (length common-a))))
    (let ((overall-diff nil)
          (overall-cost 0))
      (iter (for da in (reverse diff-a))
            (for db in (reverse diff-b))
            (for ca in (reverse (cons nil common-a)))
            (multiple-value-bind (diff cost)
                (ast-diff-on-lists da db parent-a parent-b)
              (setf overall-diff (append diff overall-diff))
              (incf overall-cost cost))
            (setf overall-diff
                  (append (mapcar (lambda (it) (cons :same it)) ca)
                          overall-diff)))
      (values overall-diff overall-cost))))

(defmethod ast-diff* ((s1 string) (s2 string)
                      &aux (ignore-whitespace *ignore-whitespace*))
  "special diff method for strings"
  #+debug (format t "ast-diff[STRING]~%")
  (if *strings*
    ;; if STRINGS is true, descend into the strings for a fine-grained diff
      (string-diff s1 s2 :ignore-whitespace ignore-whitespace)
      ;; Otherwise, treat the strings as single objects and replace
      ;; entirely if different.  Empty strings are considered to not
      ;; be present.  If whitespace is ignored, treat strings the same
      ;; if whitespace were there, except cost is computed as if whitespace
      ;; is not there.
      (string-diff-crude s1 s2 ignore-whitespace)))

(defun string-diff-crude (s1 s2 ignore-whitespace)
  (let*
      ((base-cost *base-cost*)
       (ns1 (if ignore-whitespace (remove-if #'whitespacep s1) s1))
       (ns2 (if ignore-whitespace (remove-if #'whitespacep s2) s2))
       (p (and ignore-whitespace (string= ns1 ns2))))
    (cond
      ((string= s1 s2)
       (values `((:same-sequence . ,s1)) 0))
      ((string= s1 "")
       (values `((:insert-sequence . ,s2))
               (+ base-cost (if p 0 1))))
      ((string= s2 "")
       (values `((:delete-sequence . ,s1))
               (+ base-cost (if p 0 1))))
      (t
       (values `((:insert-sequence . ,s2)
                 (:delete-sequence . ,s1))
               (+ (* 2 base-cost)
                  (cond (p 0)
                        ((string= ns1 "") 1)
                        ((string= ns2 "") 1)
                        (t 2))))))))

(defun simple-genome-pack (unpacked-g)
  "Converts list of pairs into a SIMPLE genome"
  (mapcar (lambda (p)
            `((:code . ,p)))
          unpacked-g))

(defun simple-genome-unpack (g)
  "Converts pairs in a SIMPLE genome to lists"
  (mapcar (lambda (p)
            (assert (and (eql (caar p) :code)
                         (stringp (cdar p))
                         (null (cdr p))))
            (cdar p))
          g))

(defmethod ast-diff* ((soft1 simple) (soft2 simple))
  #+debug (format t "ast-diff[SIMPLE]~%")
  (ast-diff*
   (simple-genome-unpack (genome soft1))
   (simple-genome-unpack (genome soft2))))

(defmethod ast-diff* ((simple simple) (soft parseable))
  #+debug (format t "ast-diff[SIMPLE,PARSEABLE]~%")
  (ast-diff* (simple-genome-unpack (genome simple))
             (split-sequence #\Newline (genome-string soft))))

(defmethod ast-diff* ((soft parseable) (simple simple))
  #+debug (format t "ast-diff[PARSEABLE,SIMPLE]~%")
  (ast-diff* (split-sequence #\Newline (genome-string soft))
             (simple-genome-unpack (genome simple))))

(defun split-into-subsequences (seq subseq-indices &aux (n (length seq)))
  "Return subsequences of SEQ delimited by SUBSEQ-INDICES.
Given list SEQ and a list of pairs SUBSEQ-INDICES, which are
start/length indices for disjoint nonempty subsequences of SEQ, return
a list of the N+1 subsequences between these subsequences (some
possibly empty), as well as the N subsequences themselves."
  (assert (every (lambda (x) (and (<= 0 (car x))
                                  (< (car x) n) (<= 1 (cadr x))))
                 subseq-indices))
  (assert (every (lambda (x y) (<= (+ (car x) (cadr x)) (car y)))
                 subseq-indices (cdr subseq-indices)))
  (let ((pos 0)
        (common nil)
        (diff nil))
    (iter (for (start len) in subseq-indices)
          (push (subseq seq pos start) diff)
          (push (subseq seq start (+ start len)) common)
          (setf pos (+ start len)))
    (values (nreconc diff (list (subseq seq pos))) (nreverse common))))

(defun ast-diff-elide-same (edit-script)
  "Return the non-same subset of EDIT-SCRIPT with path information.
Path's are represented as a sequence of car (:A) and cdr (:D) from the
root of the edit script (and implicitly also the program AST)."
  ;; TODO: Run length compression of these paths.
  (labels
      ((follow (edit-script path)
         (when edit-script
           (append
            (case (caar edit-script)
              ((:same :same-sequence :same-tail) nil)
              (:recurse (follow (cdar edit-script) (cons :a path)))
              (t (list (cons (reverse path) (car edit-script)))))
            (follow (cdr edit-script) (cons :d path))))))
    (follow edit-script nil)))

;;; Construct the edit tree from an ast and an edit script

(defgeneric create-edit-tree (source target script &key &allow-other-keys)
  (:documentation "Given a source that is the source of the
edit script SCRIPT, a target objec that is the result of the script on the
source, build the edit tree corresponding to the script."))

(defmethod create-edit-tree (source target (script cons)
                             &key &allow-other-keys)
  ;; The script is a list of actions on the AST's children
  (if (and (typep source 'ast)
           (typep target 'ast))
      (change-segments-on-seqs
       (children source)
       (children target)
       script
       (lambda (source-segment-start source-segment-length
                target-segment-start target-segment-length
                children actions)
         (let ((source-segment (make-instance 'edit-segment
                                              :node source
                                              :start source-segment-start
                                              :length source-segment-length))
               (target-segment (make-instance 'edit-segment
                                              :node target
                                              :start target-segment-start
                                              :length target-segment-length)))
           (make-instance 'edit-tree-node
                          :source source-segment
                          :target target-segment
                          :children children
                          :script actions)))
       (lambda (source-child target-child action-cdr &rest args &key &allow-other-keys)
         ;; Need to include source, target in case children are strings
         (apply #'create-edit-tree source-child target-child action-cdr args))
       source target)
      (call-next-method)))

(defun change-segments-on-seqs (source target script segment-fn recurse-fn
                                source-node target-node)
  "Traverses two sequences and finds the change segments.
source -- AST being editted
target -- AST that it is being turned into
script -- the edit script that changes source to target
segment-fn -- A function that is called on six arguments to construct
  an edit tree node for a pair of child segments
recurse-fn -- A function called on two children of source and target
  in order to create an edit tree for that recursive edit.

Returns a list of edit tree nodes for these nodes."
  (let ((source-position 0)
        (source-segment-start 0)
        (target-position 0)
        (target-segment-start 0)
        (collected-nodes nil)
        (collected-recurse nil)
        (collected-actions nil)
        (changes 0))
    (flet ((finish (&optional (n 1))
             ;; (format t "FINISH: CHANGES = ~A, N = ~A~%" changes n)
             (cond
               ;; Multiple changes -- group them into a single
               ;; edit tree node (with possible children)
              ((or (> changes 1)
                   (and (= changes 1)
                        (null collected-recurse)))
               (push (funcall segment-fn
                              source-segment-start
                              (- source-position source-segment-start)
                              target-segment-start
                              (- target-position target-segment-start)
                              (nreverse collected-recurse)
                              (nreverse collected-actions))
                     collected-nodes))
              ;; Single recurse; propagate that node upwards
              (collected-recurse
               (assert (= changes 1))
               (push collected-recurse collected-nodes)))
             (setf collected-recurse nil)
             (setf collected-actions nil)
             (setf changes 0)
             (setf source-segment-start (incf source-position n))
             (setf target-segment-start (incf target-position n))))
      (iter (for action in script)
            (ecase (car action)
              (:same (finish))
              (:same-sequence
               (finish (length (cdr action))))
              (:insert
               ;; Record new target tree child
               (push action collected-actions)
               (incf changes)
               (incf target-position))
              (:delete
               ;; Record new source tree child
               (push action collected-actions)
               (incf changes)
               (incf source-position))
              (:replace
               (push action collected-actions)
               (incf changes)
               (incf target-position)
               (incf source-position))
              (:insert-sequence
               (push action collected-actions)
               (let ((n (length (cdr action))))
                 (incf changes n)
                 (incf target-position n)))
              (:delete-sequence
               (push action collected-actions)
               (let ((n (length (cdr action))))
                 (incf changes n)
                 (incf source-position n)))
              (:recurse
               (push action collected-actions)
               (let ((node (funcall recurse-fn
                                    (elt source source-position)
                                    (elt target target-position)
                                    (cdr action)
                                    :source-node source-node
                                    :source-position source-position
                                    :target-node target-node
                                    :target-position target-position)))
                 (assert node)
                 (if (listp node)
                     (dolist (n node) (push n collected-recurse))
                     (push node collected-recurse))
                 (incf changes)
                 (incf source-position)
                 (incf target-position)))
              ;; :wrap and :unwrap should be changed into single edit tree nodes
              ))
      (finish 0))
    (if (and (null (cdr collected-nodes))
             (listp (car collected-nodes)))
        (car collected-nodes)
        (reverse collected-nodes))))

(defmethod create-edit-tree
    ((source-string string) (target-string string) script
     &key source-node target-node source-position target-position
       &allow-other-keys)
  ;; May want to just punt here
  (change-segments-on-seqs
   source-string target-string script
   (lambda (source-segment-start source-segment-length
            target-segment-start target-segment-length
            children actions)
     ;; children should be empty, but that may change if we hierarchically
     ;; group substring edits
     (let ((source-segment
            (make-instance 'string-edit-segment
                           :node source-node
                           :start source-position
                           :string-start source-segment-start
                           :string-length source-segment-length))
           (target-segment
            (make-instance 'string-edit-segment
                           :node target-node
                           :start target-position
                           :string-start target-segment-start
                           :string-length target-segment-length)))
       (make-instance 'edit-tree-node :source source-segment
                      :target target-segment
                      :children children
                      :script actions)))
   (lambda (&rest args)
     (error ":recurse action found on a string value: ~a" args))
   nil nil))

(defgeneric map-edit-tree (edit-tree fn)
  (:documentation "Apply FN to the nodes in EDIT-TREE, in preorder"))

(defparameter  *map-edit-tree-ancestors* nil
  "Stores the stack of ancestors above the current node in
during calls to MAP-EDIT-TREE.")

(defmethod map-edit-tree ((edit-tree edit-tree-node-base) fn)
  (funcall fn edit-tree)
  (let ((*map-edit-tree-ancestors*
         (cons edit-tree *map-edit-tree-ancestors*)))
    (map-edit-tree (edit-tree-node-children edit-tree) fn))
  edit-tree)

(defmethod map-edit-tree ((edit-trees list) fn)
  (dolist (c edit-trees)
    (map-edit-tree c fn))
  edit-trees)

(defun create-and-print-edit-tree (softwares script &key print-asts coherence)
  (assert (typep softwares '(cons t (cons t null))))
  (destructuring-bind (source target)
      softwares
    (print-edit-tree (create-edit-tree source target script)
                     :print-asts print-asts
                     :coherence coherence)))

(defun print-edit-tree (edit-tree &key print-asts coherence)
  (let ((*map-edit-tree-ancestors* nil))
    (map-edit-tree edit-tree
                   (lambda (node) (print-edit-tree-node
                                   node :print-asts print-asts
                                   :coherence coherence)))))

(defgeneric print-edit-tree-node (node &key &allow-other-keys)
  (:documentation "Print fragment of an edit tree, properly indented"))

(defun coherence (node)
  (let ((c1 (ast-size (edit-tree-node-source node)))
        (c2 (ast-size (ast-size node))))
    (/ (float c2) (float c1))))

(defmethod print-edit-tree-node
    ((node edit-tree-node) &key print-asts coherence)
  (assert (typep node 'edit-tree-node))
  ;; If COHERENCE is specified, print only the highest edit tree
  ;; nodes whose coherence is >= this limit
  (let ((node-coherence (coherence node))
        (parent (car *map-edit-tree-ancestors*)))
    (when (or (not coherence)
              (and (>= node-coherence coherence)
                   (or (null parent)
                       (< (coherence parent) coherence))))
      (let ((source-text (source-text (edit-tree-node-source node)))
            (target-text (source-text (edit-tree-node-target node)))
            (per-line-prefix
             (make-string (* (length *map-edit-tree-ancestors*) 2)
                          :initial-element #\>)))
        (terpri)
        (loop repeat 6 do (princ "----------"))
        (terpri)
        (format t "Coherence: ~a~%" node-coherence)
        (if (and (not (position #\Newline source-text))
                 (not (position #\Newline target-text)))
            ;; No newlines, so print in a compact form on a single line
            (format t "~a ~s => ~s~%" per-line-prefix source-text target-text)
            ;; Otherwise, print as a text block, with indentation
            (let ((*print-pretty* t))
              (pprint-logical-block (*standard-output*
                                     nil ; node
                                     :per-line-prefix per-line-prefix)
                (format t "~a~%---------------~%~a~&"
                        source-text target-text))))
        (when print-asts
          (format t "---------------~%")
          (format t "~s~%==>~%~s~%"
                  (ast-to-list-form (edit-tree-node-source node))
                  (ast-to-list-form (edit-tree-node-target node))))))))

(defun describe-edit-tree (edit-tree)
  (let ((*map-edit-tree-ancestors* nil))
    (map-edit-tree edit-tree #'describe-edit-tree-node)))

(defgeneric describe-edit-tree-node (node)
  (:documentation "Print more complete description of an edit
tree node.  Includes the output of print-edit-tree-node, plus
additional information for debugging."))

(defmethod describe-edit-tree-node ((node edit-tree-node))
  (print-edit-tree-node node)
  (format t "--------~%")
  (describe-segment (edit-tree-node-source node))
  (describe-segment (edit-tree-node-target node))
  (format t "~s~%" (edit-tree-node-script node)))

(defgeneric describe-segment (edit-tree-segment)
  (:method-combination progn :most-specific-last)
  (:documentation "Print debugging information for an
object of type edit-tree-segment"))

(defmethod describe-segment progn ((edit-tree-segment edit-segment-common))
  (format t "Start = ~a~%" (edit-segment-start edit-tree-segment)))

(defmethod describe-segment progn ((edit-tree-segment edit-segment))
  (format t "Length = ~a~%" (edit-segment-length edit-tree-segment)))

(defmethod describe-segment progn ((edit-tree-segment string-edit-segment))
  (let* ((start (edit-segment-start edit-tree-segment))
         (str (elt (children (edit-segment-node edit-tree-segment))
                   start)))
    (format t "String = ~s~%" str))
  (format t "String-start = ~a~%"
          (string-edit-segment-string-start edit-tree-segment))
  (format t "String-length = ~a~%"
          (string-edit-segment-string-length edit-tree-segment)))


;;; ast-patch and associated functions

(defmacro apply-values (fn &rest arg-exprs)
  "Apply FN to the values returned by each arg-expr."
  (unless arg-exprs
    (error "Requires at least one argument: (APPLY-VALUES ~A)" fn))
  `(apply-values-fn ,fn (list ,@(iter (for e in arg-exprs)
                                      (collect `(multiple-value-list ,e))))
                    nil))

(defun extend-list (list desired-length extension-value)
  "If list is less than the designed length, return a fresh list
that is padded to that length with the extension value.  Otherwise,
return list itself, uncopied."
  (let ((len (length list)))
    (if (< len desired-length)
        (append list (make-list (- desired-length len)
                                :initial-element extension-value))
        list)))

(defun apply-values-fn (fn arg-lists replicate?)
  "Map FN over the lists in arg-lists, where each list has been NIL extended
to the same length.  If REPLICATE? is true then pad with the last element of
the list, not with NIL.   Return the values produced by the mapping
as multiple return values."
  (let* ((len (reduce #'max arg-lists :key #'length))
         (extended-arg-lists
          (iter (for arg-list in arg-lists)
                (collect
                    (extend-list
                     arg-list
                     len (when replicate?
                           (lastcar arg-list)))))))
    (values-list (apply #'mapcar fn extended-arg-lists))))

(defmacro apply-values-extend (fn &rest arg-exprs)
  "Apply FN to the values returned by each arg-expr.  If an arg list
is shorter, replicate the last value (or NIL if none)."
  (unless arg-exprs
    (error "Requires at least one argument: (APPLY-VALUES ~A)" fn))
  `(apply-values-fn ,fn (list ,@(iter (for e in arg-exprs)
                                      (collect `(multiple-value-list ,e))))
                    t))

(defmacro apply-values-meld (fn form1 list-form)
  `(apply-values-meld-fn ,fn
                         (multiple-value-list ,form1)
                         (multiple-value-list ,list-form)))

(defun apply-values-meld-fn (fn vals list-vals)
  (let ((len1 (length vals))
        (len2 (length list-vals)))
    (assert (<= 1 len2 3))
    (ecase len1
      ;; If len1 is 1, we have a single value, so append the list-vals
      (1 (let ((tail (reduce #'append (cdr list-vals)
                             :initial-value (car list-vals)
                             :from-end t)))
           (funcall fn (car vals) tail)))
      ;; Otherwise, extend the two versions
      (2
       (values
        (first list-vals)
        (funcall fn (car vals) (cadr list-vals))
        (funcall fn (car vals) (caddr list-vals)))))))

(defmacro cons-values (meld? &rest args)
  `(if ,meld?
       (apply-values-meld #'cons ,@args)
       (apply-values-extend #'cons ,@args)))

(defmacro append-values (meld? &rest args)
  `(if ,meld?
       (apply-values-meld #'append ,@args)
       (apply-values-extend #'append ,@args)))

(defun ast-patch (original diff &rest keys)
  (if (consp original)
      (unastify (apply #'ast-patch* (astify original) diff keys))
      (apply #'ast-patch* original diff keys)))

;;; Macros, structures for accessing edit actions

;; I wanted to do all these with defstruct, but some are
;; represented with improper lists

(deftype same-eop () '(cons (eql :same) t))
(defun same-eop-p (e) (typep e 'same-eop))
(defmacro same-eop-old (e) `(cdr ,e))
(defun copy-same-eop (e) (cons :same (same-eop-old e)))

(deftype insert-eop () '(cons (eql :insert) t))
(defun insert-eop-p (e) (typep e 'insert-eop))
(defmacro insert-eop-new (e) `(cdr ,e))
(defun copy-insert-eop (e) (cons :insert (insert-eop-new e)))

(deftype delete-eop () '(cons (eql :delete) t))
(defun delete-eop-p (e) (typep e 'delete-eop))
(defmacro delete-eop-old (e) `(cdr ,e))
(defun copy-delete-eop (e) (cons :delete (delete-eop-old e)))

(deftype replace-eop () '(cons (eql :replace) (cons t (cons t null))))
(defun replace-eop-p (e) (typep e 'replace-eop))
(defmacro replace-eop-old (e) `(cadr ,e))
(defmacro replace-eop-new (e) `(caddr ,e))

(deftype recurse-eop () '(cons (eql :recurse) t))
(defun recurse-eop-p (e) (typep e 'recurse-eop))
(defmacro recurse-eop-ops (e) `(cdr ,e))
(defun copy-recurse-eop (e) (cons :recurse (recurse-eop-ops e)))

(defstruct (:wrap (:type list) (:conc-name wrap-)
                  (:predicate wrap-p)
                  (:copier copy-wrap)
                  :named)
  sub path left right classes new)

(defstruct (:unwrap (:type list) (:conc-name unwrap-)
                    (:predicate unwrap-p)
                    :named)
  sub path left right)

(defstruct (:wrap-sequence (:type list) (:conc-name wrap-sequence-)
                           (:predicate wrap-sequence-p)
                           (:copier copy-wrap-sequence)
                           :named)
  length sub path left right classes)

(defstruct (:unwrap-sequence (:type list) (:conc-name unwrap-sequence-)
                           (:predicate unwrap-sequence-p)
                           (:copier copy-unwrap-sequence)
                           :named)
  sub path left right)


(defgeneric ast-patch* (original diff &rest keys &key conflict &allow-other-keys)
  (:documentation "Create an edited AST by applying DIFF to ORIGINAL.

A diff is a sequence of actions as returned by `ast-diff' including:
:same A  : keep the current AST
:insert B  : insert B at the current position
:replace A B : remove the current AST A, replacing it with B
:delete A  : remove the current AST
:recurse S : recursively apply script S to the current AST
:wrap S <path> <left-wrap> <right-wrap> <classes> <new> : Apply S to current AST,
   then wrap it in nodes of kinds <classes> with left and right children along
   the path given by <left-wrap> and <right-wrap>.  <new> is the result
   of the wrap action.
:unwrap S <path> <left-wrap> <right-wrap> : Apply S to the subtree of given
  by following <path> down from this tree.
:wrap-sequence <length> S <path> <left-wrap> <right-wrap> <classes> : Apply S to the
  AST obtained by taking the next <length> elements of the current child
  list converted to an AST of the same kind as (last <classes>), then
  wrap it in nodes of kinds (butlast <classes>) with left and right children as for :wrap
:unwrap-sequence S <path> <left-wrap> <right-wrap> : Apply S to node obtained
  by going down <path>, then insert its children into the child sequence here.

If diff is a list of these operations, it is interpreted as applying
to the list of children of ORIGINAL.   The sequence operations can only
be applied in that context."))

(defmethod ast-patch* ((original null) (script null) &key &allow-other-keys)
  nil)

(defun actual-ast-patch (ast script &rest keys
                        &key delete? (meld? (ast-meld-p ast)) &allow-other-keys)
  (declare (ignorable delete? meld?))
  (assert (typep ast 'ast))
  (assert (not (typep ast 'non-homologous-ast)))
  (let* ((children (children ast))
         ;; For now, always meld
         ;; This may not give valid ASTs, but fix later
         (new-child-lists
          (multiple-value-list
           (apply #'ast-patch* children script :meld? meld? keys))))
    (apply #'values
           (iter (for new-children in new-child-lists)
                 (collect (copy ast :children new-children))))))

(defun actual-non-homologous-ast-patch (ast script &rest keys
                                        &key delete? (meld? (ast-meld-p ast))
                                        &allow-other-keys)
  ;; Outline of approach:
  ;;  Build a "fake children" list which contains slot specifiers for the named children
  ;;  slots, and also the non-empty interleaved text
  ;;  Compute diff on these list
  ;;  Convert these lists back to interleaved-text and child lists
  (declare (ignorable delete? meld?))
  (check-child-lists ast)
  (let* ((schildren (standardized-children ast))
         (new-schild-lists
           (multiple-value-list
            (apply #'ast-patch* schildren script :meld? meld? keys))))
    (apply #'values
           (iter (for new-schildren in new-schild-lists)
                 (collect (copy-with-standardized-children ast new-schildren))))))

(defmethod ast-patch* ((ast ast) (script list) &rest keys
                       &key &allow-other-keys)
  (if (listp (car script))
      (apply #'actual-ast-patch ast script keys)
      (call-next-method)))

(defmethod ast-patch* ((ast non-homologous-ast) (script list) &rest keys
                        &key &allow-other-keys)
  (if (listp (car script))
      (apply #'actual-non-homologous-ast-patch ast script keys)
      (call-next-method)))

(defmethod ast-patch* ((original t) (script cons)
                       &rest keys &key (delete? t) &allow-other-keys)
  (declare (ignorable delete? keys))
  (if (typep original 'ast)
      (case (car script)
        (:wrap
         (apply #'ast-patch-wrap original (cdr script) keys))
        (:unwrap
         (apply #'ast-patch-unwrap original (cdr script) keys))
        (t
         (apply #'actual-ast-patch original script keys)))
      (case (car script)
        ;; The script is a single command
        (:wrap
         (apply #'ast-patch-wrap original (cdr script) keys))
        (:unwrap
         (apply #'ast-patch-unwrap original (cdr script) keys))
        (:recurse-tail
         (ast-patch* original (cdr script)))
        (:same
         (assert (equal? original (cdr script))
                 ()
                 "AST-PATCH*: :SAME not same as in script: ~a, ~a"
                 original
                 (cdr script))
         (cdr script))
        (t
         ;; The script is a list of commands
         ;; The case of applying a list of commands to
         ;; a list of things is handled in another method
         ;; This is just for one or two operations on an atom
         (assert (proper-list-p script))
         (let ((keys (mapcar #'car script)))
           (cond
             ((equal keys '(:same))
              (assert (equal? original (cdar script))
                      ()
                      "AST-PATCH*: :SAME not same as in script(2): ~a, ~a"
                      original
                      (cdar script))
              (cdar script))
             ((equal keys '(:insert :delete))
              (assert (equal? original (cdadr script))
                      ()
                      "AST-PATCH*: ~a not same as in script(2): ~a, ~a"
                      keys
                      original (cdadr script))
              (cdar script))
             ((equal keys '(:delete :insert))
              (assert (equal? original (cdar script))
                      ()
                      "AST-PATCH*: ~a not same as in script(2): ~a, ~a"
                      keys
                      original (cdar script))
              (cdadr script))
             ((equal keys '(:replace))
              (assert (equal? original (cadar script))
                      ()
                      "AST-PATCH*: ~a not the same as in script: ~a, ~a"
                      keys original (cadar script))
              (caddar script))
             ((member :conflict keys)
              (values-list (iter (for s in (cdr script))
                                 (collect (ast-patch* original s)))))
             (t (error "Invalid diff on atom: ~a" script))))))))

(defun ast-patch-conflict-action (asts args)
  "Perform a patch of the action (:conflict . args) on ASTS.
Returns the conflict node and the list of remaining asts to
process with the rest of the script."
  ;; Special case: one conflict action is :SAME, the other is :RECURSE
  ;; In that case, we need to propagate the changes down the tree IFF there
  ;; is a nested conflict AST.
  (let ((sc (cond ((and (eql (caaar args) :same)
                        (eql (caaadr args) :recurse))
                   (ast-patch-same-recurse (car asts) (cdaadr args)
                                           :your))
                  ((and (eql (caaar args) :recurse)
                        (eql (caaadr args) :same))
                   (ast-patch-same-recurse (car asts) (cdaar args)
                                           :my)))))
    (if (or (typep sc 'conflict-ast)
            (and (listp sc) (some {typep _ 'conflict-ast} sc)))
        ;; Special case
        (values sc (cdr asts))
        ;; Base case
        (let ((consume nil)) ;; If set to true, consume an element of ASTS
          (flet ((%process (action)
                   "Process an inner action.  Returns a list of asts"
                   (ecase (car action)
                     ((nil) nil)
                     (:insert (list (cdr action)))
                     (:replace
                      (setf consume t)
                      (list (caddr action)))
                     (:delete
                      (setf consume t)
                      nil)
                     (:same (setf consume t)
                            (list (car asts)))
                     (:recurse (setf consume t)
                               ;; Don't process recursive conflicts
                               ;; In particular, this means :delete actions in
                               ;; the conflict branch do not cause recording
                               ;; of the original version in conflict nodes.
                               ;; This is arguably wrong, but for now we do it
                               ;; this way.
                               (list (ast-patch* (car asts) (cdr action)
                                                 :conflict nil))))))
            (let ((child-alist
                   (iter (for script in args)
                         (for i in '(:my :your))
                         (let ((actions (%process (car script))))
                           (when actions
                             (collecting (cons i actions)))))))
              (when consume
                ;; :old is the key for the base version
                (setf child-alist (cons (list :old (pop asts))
                                        child-alist)))
              (values
               (make-instance 'conflict-ast :child-alist child-alist)
               asts)))))))

(defun ast-patch-same-recurse (asts script tag)
  "Perform actions in SCRIPT in ASTS in parallel with implicit :SAME operations"
  ;; Should only happen when CONFLICT is true
  (ast-patch asts script :tag tag :conflict t))

(defun ast-patch-same-wrap (ast args tag)
  (declare (ignorable ast args tag))
  (error "Not yet implemented"))

(defgeneric ast-patch-wrap (ast args &key &allow-other-keys))

(defmethod ast-patch-wrap ((ast ast) (args list) &rest keys
                           &key &allow-other-keys)
  (destructuring-bind (sub-action path left-wrap right-wrap classes base-ast)
      args
    (assert (= (length path) (length left-wrap) (length right-wrap)))
    (values-list
     (mapcar
      (lambda (a) (ast-wrap a left-wrap right-wrap classes base-ast))
      (multiple-value-list (apply #'ast-patch* ast sub-action keys))))))

(defmethod ast-patch-wrap ((ast list) (args list) &rest keys &key &allow-other-keys)
  (destructuring-bind (sub-action path left-wrap right-wrap classes base-ast)
      args
    (assert (= (length path) (length left-wrap) (length right-wrap)))
    (values-list
     (mapcar
      (lambda (a) (ast-wrap (list a) left-wrap right-wrap classes base-ast))
      (multiple-value-list (apply #'ast-patch* ast sub-action keys))))))

(defgeneric ast-patch-unwrap (ast args &key &allow-other-keys))

(defmethod ast-patch-unwrap ((ast ast) (args list) &rest keys
                             &key &allow-other-keys)
  (destructuring-bind (sub-action path left-wrap right-wrap)
      args
    (assert (= (length path) (length left-wrap) (length right-wrap)))
    (iter (while path)
          (setf ast (nth (pop path) (children ast))))
    (apply #'ast-patch* ast sub-action keys)))

(defun ast-wrap (ast left-wrap right-wrap classes base-ast)
  (assert (= (length left-wrap) (length right-wrap) (length classes)))
  (assert left-wrap)
  (setf left-wrap (reverse left-wrap))
  (setf right-wrap (reverse right-wrap))
  (iter (while left-wrap)
        (let ((class (pop classes)))
          (assert class)
          (setf ast (copy base-ast
                          :class class
                          :children (append (pop left-wrap)
                                            (list ast)
                                            (pop right-wrap))))))
  #+ast-diff-debug (format t "AST-WRAP returned:~%~s~%" (source-text ast))
  ast)

(defun ast-patch-same-wrap-sequence (ast args tag)
  (declare (ignore ast args tag))
  (error "Not yet implemented"))

(defgeneric ast-patch-wrap-sequence (ast args &key &allow-other-keys)
  (:method ((asts list) (args list) &rest keys &key &allow-other-keys)
    (destructuring-bind (len sub-action path left-wrap right-wrap classes base-ast)
        args
      (declare (ignore len))
      (assert (= (length path) (length left-wrap) (length right-wrap)))
      (let ((ast (copy base-ast :children asts)))
        (values-list
         (mapcar
          (lambda (a) (ast-wrap-sequence a left-wrap right-wrap classes base-ast))
          (multiple-value-list (apply #'ast-patch* ast sub-action keys))))))))

(defun ast-wrap-sequence (ast left-wrap right-wrap classes base-ast)
  (assert (= (length left-wrap) (length right-wrap) (length classes)))
  (setf left-wrap (reverse left-wrap))
  (setf right-wrap (reverse right-wrap))
  (let ((asts (children ast)))
    (iter (while left-wrap)
          (let ((class (pop classes)))
            (assert class)
            (setf asts (list (copy base-ast
                                   :class class
                                   :children (append (pop left-wrap)
                                                     asts
                                                     (pop right-wrap)))))))
    #+ast-diff-debug (format t "AST-WRAP returned:~%~s~%" (source-text asts))
    (car asts)))


;; This is not handling meld?
(defgeneric ast-patch-unwrap-sequence (ast args &key &allow-other-keys)
  (:method ((ast ast) (args list) &rest keys &key &allow-other-keys)
    (destructuring-bind (sub-action path left-wrap right-wrap)
        args
      (assert (= (1+ (length path)) (length left-wrap) (length right-wrap)))
      (iter (while path)
            (setf ast (nth (pop path) (children ast)))
            (pop left-wrap)
            (pop right-wrap))
      (assert (= (length left-wrap) 1))
      (assert (= (length right-wrap) 1))
      (let* ((c (children ast))
             (total-len (length c))
             (left-len (length (car left-wrap)))
             (right-len (length (car right-wrap))))
        (assert (>= total-len (+ left-len right-len)))
        (let ((new-ast (copy ast :children (subseq c left-len (- total-len right-len)))))
          (children
           (apply #'ast-patch* new-ast sub-action keys)))))))

(defmethod ast-patch* ((original cons) (script list)
                       &rest keys
                       &key (delete? t) (meld? t) conflict tag &allow-other-keys)
  ;; MELD? causes conflicts to be all placed into the list, if possible
  ;; CONFLICT causes conflict objects to be produced
  ;; Otherwise, multiple values are returned, one for each conflict
  ;; This feature allows conflicts to be migrated up ASTs until they can
  ;; be more safely combined.
  ;; When MELD? is true, place conflicts in contiguous pieces.
  ;; When TAG is true, the patch is being done in the context
  ;; of another patch that is implicitly :SAME.  This can generate
  ;; conflict nodes.
  ;;
  ;; This desperately needs to be cleaned up.
  (declare (ignorable delete?))
  (when (member (car script) '(:wrap :unwrap))
    (return-from ast-patch* (call-next-method)))
  ;; Avoid the problem that, since each `:same' results in non-tail
  ;; recursion, comparing two large, identical files can overflow the
  ;; stack.
  (when (every {starts-with :same} script)
    (return-from ast-patch* original))
  (labels
      ((merge-conflict-ast (conflict-node rest)
         (if (and (typep (car rest) 'conflict-ast)
                  (typep conflict-node 'conflict-ast))
             (cons (combine-conflict-asts conflict-node (car rest))
                   (cdr rest))
             (cons conflict-node rest)))
       (record-conflict (asts script)
         (multiple-value-bind (conflict-node asts-rest)
             (ast-patch-conflict-action
              asts (list (list (car script))))
           (let ((rest (edit asts-rest (cdr script))))
             (merge-conflict-ast conflict-node rest))))
       (edit (asts script)
         ;; Returns multiple values, depending on the value of MELD
         ;; When MELD is false, return a single value if there are no
         ;; conflicts, otherwise if conflict is false, return the conflict
         ;; versions.  If conflict is true, return a single version
         ;; with conflict nodes.
         ;; When MELD is true, returns three values: the common list
         ;; formed by patching some tail of this list, and the partial
         ;; lists of the conflict versions (this will be three values).
         (when script
           (destructuring-bind (action . args) (car script)
             (ecase action
               (:conflict
                (cond
                  (meld? ;; was handled by around method in false case
                   (edit asts (append (meld-scripts (first args) (second args))
                                      (cdr script))))
                  (conflict
                   ;; (assert (null meld?))
                   (multiple-value-bind (conflict-node asts-rest)
                       (ast-patch-conflict-action asts args)
                     ;; Possible merge conflict nodes here
                     (let ((rest (edit asts-rest (cdr script))))
                       (merge-conflict-ast conflict-node rest))))
                  (t (error "This case should never happen"))))
               (:recurse
                (if tag
                    (cons (ast-patch-same-recurse (car asts) args tag)
                          (edit (cdr asts) (cdr script)))
                    (cons-values meld?
                                 (apply #'ast-patch* (car asts) args keys)
                                 (edit (cdr asts) (cdr script)))))
               (:wrap
                ;; The desired patch is:  apply the sub action, then wrap
                ;; the result term in the left and right wrap lists.
                (if tag
                    (cons (ast-patch-same-wrap (car asts) args tag)
                          (edit (cdr asts) (cdr script)))
                    (cons-values meld?
                                 (apply #'ast-patch-wrap (car asts) args keys)
                                 (edit (cdr asts) (cdr script)))))

               (:wrap-sequence
                (let ((len (car args)))
                  (assert (typep len '(integer 0)))
                  (assert (>= (length asts) len))
                  (if tag
                      (cons (ast-patch-same-wrap-sequence (subseq asts 0 len) args tag)
                            (edit (subseq asts len) (cdr script)))
                      (cons-values meld?
                                   (apply #'ast-patch-wrap-sequence (subseq asts 0 len) args keys)
                                   (edit (subseq asts len) (cdr script))))))

               (:unwrap-sequence
                (if tag
                    (error "Not implemented: :unwrap-sequences with tag")
                    (append-values meld?
                                   (apply #'ast-patch-unwrap-sequence (car asts) args keys)
                                   (edit (cdr asts) (cdr script)))))
               (:same
                (let* ((script script)
                       (asts asts)
                       (prefix (iter (while (eql (caar script) :same))
                                     (collecting (pop asts))
                                     (pop script))))
                  (append-values meld? prefix (edit asts script))))
               (:same-tail
                (assert (null (cdr script))) ;; :same-tail always occurs last
                (assert (equal? asts args) () "AST-PATCH* (CONS): ~
                        :SAME-TAIL not as as in script: ~a, ~a" asts args)
                asts)
               (:recurse-tail
                (assert (null (cdr script)))
                (ast-patch* asts args))
               (:delete
                (assert (equal? (car asts) args) () "AST-PATCH* (CONS): ~
                        :DELETE not same as in script: ~a,~a" (car asts) args)
                (cond
                  (tag
                   ;; Conducting an implicit :SAME
                   (let ((alist (iter (for i in '(:old :my :your))
                                      (unless (eql i tag)
                                        (collecting (list i (car asts)))))))
                     (merge-conflict-ast
                      (make-instance 'conflict-ast :child-alist alist)
                      (edit (cdr asts) (cdr script)))))
                  (conflict
                   ;; Record this, since it conflicts with :old
                   (record-conflict asts script))
                  ;; The key DELETE?, if NIL (default T) will
                  ;; cause :DELETE edits to be ignored.  The
                  ;; use case for this is to do a kind of binary
                  ;; merge of two objects, sharing as much structure
                  ;; as possible
                  (delete?
                   (edit (cdr asts) (cdr script)))
                  (t
                   (cons-values meld?
                                (car asts) (edit (cdr asts) (cdr script))))))

               (:replace
                (assert (equal? (car asts) (car args)) ()
                        "AST-PATCH* (CONS): ~
                         :REPLACE not same as in script: ~a, ~a"
                        (car asts) (car args))
                (cond
                  (tag
                   ;; implicit :SAME on other tags
                   (let ((alist (iter (for i in '(:old :my :your))
                                      (if (eql i tag)
                                          (collecting (list i (cadr args)))
                                          (collecting (list i (car asts)))))))
                     (merge-conflict-ast
                      (make-instance 'conflict-ast :child-alist alist)
                      (edit (cdr asts) (cdr script)))))
                  (conflict
                   ;; Record this, since it conflicts with :old
                   (record-conflict asts script))
                  (delete?
                   (cons-values meld? (cadr args) (edit (cdr asts) (cdr script))))
                  (t
                   (cons-values meld? (cadr args) (edit asts (cdr script))))))
                   
               (:insert
                (if tag
                    (let ((alist `((,tag ,args))))
                      (merge-conflict-ast
                       (make-instance 'conflict-ast :child-alist alist)
                       (edit asts (cdr script))))
                    (cons-values meld? args (edit asts (cdr script)))))

               (:insert-sequence
                (append-values meld? args (edit asts (cdr script))))
               (:delete-sequence
                (append-values
                 meld?
                 (iter (while (consp args))
                       (assert asts)
                       (assert (equal? (car asts) (car args)) ()
                               "AST-PATCH* (CONS): ~
                               :DELETE-SEQUENCE not same as in script: ~a, ~a"
                               (car asts) (car args))
                       (let ((a (pop asts)))
                         (unless delete? (collect a)))
                       (pop args))
                 (edit asts (cdr script)))))))))
    ;; cause various unmerged subsequences to be combined before
    ;; returning, if meld? is true
    (if (listp (car script))
        (append-values meld? nil (edit original script))
        (call-next-method))))

(defun meld-scripts (script1 script2)
  "Combine two edit scripts that process sequences of the same length.
Do this by pairing off the edit operations that consume list elements,
and replicating the others."
  (prog1
      (iter (let ((inserts1 (iter (while (member (caar script1)
                                                 '(:insert :insert-sequence)))
                                  (collect (pop script1))))
                  (inserts2 (iter (while (member (caar script2)
                                                 '(:insert :insert-sequence)))
                                  (collect (pop script2)))))
              (appending inserts1)
              (appending inserts2))
            (while (and script1 script2))
            ;; At this point, both start with a non-insert action
            (let ((action1 (caar script1))
                  (action2 (caar script2)))
              ;; actions are one of: :same, :delete, :recurse
              ;; Don't do :same-tail, :recurse-tail here
              (let ((val (list action1 action2)))
                (flet ((%check (s1 s2)
                         (assert (equal? s1 s2) () "MELD-SCRIPTS ~a: ~
                                 should have been the same: ~a, ~a" val s1 s2)))
                  
                  (switch (val :test #'equal)
                    ('(:same :same)
                      (%check (cdar script1) (cdar script2))
                      (collect (pop script1))
                      (pop script2))
                    ('(:delete :delete)
                      (%check (cdar script1) (cdar script2))
                      (collect (pop script1))
                      (pop script2))
                    ('(:delete :same)
                      (%check (cdar script1) (cdar script2))
                      (collect (pop script1))
                      (pop script2))
                    ('(:recurse :same)
                      (collect (pop script1))
                      (pop script2))
                    ('(:recurse :delete)
                      (collect (pop script1))
                      (pop script2))
                    ('(:same :delete)
                      (%check (cdar script1) (cdar script2))
                      (pop script1)
                      (collect (pop script2)))
                    ('(:same :recurse)
                      (pop script1)
                      (collect (pop script2)))
                    ('(:delete :recurse)
                      (pop script1)
                      (collect (pop script2)))
                    ('(:recurse :recurse)
                      ;; should not happen?
                      (pop script2)
                      (collect (pop script1)))
                    ('(:replace :same)
                      (%check (cadar script1) (cdar script2))
                      (collect (pop script1))
                      (pop script2))
                    ('(:replace :delete)
                      (%check (cadar script1) (cdar script2))
                      (collect (pop script1)))
                    ('(:replace :replace)
                      (%check (cadar script1) (cadar script2))
                      (pop script2)
                      (collect (pop script1)))
                    ('(:replace :recurse)
                      (pop script2)
                      (collect (pop script1)))
                    ('(:same :replace)
                      (%check (cdar script1) (cadar script2))
                      (pop script1)
                      (collect (pop script2)))
                    ('(:delete :replace)
                      (%check (cdar script1) (cadar script2))
                      (pop script1)
                      (collect (pop script2)))
                    ('(:recurse :replace)
                      (pop script2)
                      (collect (pop script1)))
                    (t (error "Do not recognize actions in meld-scripts: ~A, ~A"
                              action1 action2)))))))
    (when (or script1 script2)
      (error
       "Could not meld scripts: different number of fixed location actions"))))

(defmethod ast-patch* :around ((original sequence) (script list)
                               &key delete? meld? conflict &allow-other-keys)
  (declare (ignorable delete?))
  (if (and (listp (car script))
           (find :conflict script :key #'car))
      (if (or meld? conflict)
          (let ((result (call-next-method)))
            #+ast-diff-debug
            (format t "AST-PATCH* returned:~%~a~%" (mapcar #'source-text result))
            result)
          (let ((script1 (iter (for action in script)
                               (appending
                                (if (eql (car action) :conflict)
                                    (second action)
                                    (list action)))))
                (script2 (iter (for action in script)
                               (appending
                                (if (eql (car action) :conflict)
                                    (third action)
                                    (list action))))))
            (values (ast-patch* original script1)
                    (ast-patch* original script2))))
      (let ((result (call-next-method)))
        #+ast-diff-debug
        (format t "AST-PATCH* returned:~%~a~%" (mapcar #'source-text result))
        result)))

(defmethod ast-patch* ((original vector) (script list)
                       &rest keys &key (delete? t) meld? &allow-other-keys)
  ;; Specialized method for patching vectors
  ;; we require that the elements inserted must be compatible
  ;; with the element type of the original vector
  (declare (ignorable delete? meld?))
  ;; Create a single result, with conflicts combined
  (let* ((len (length original))
         (etype (array-element-type original))
         (result (make-array (list len)
                             :element-type etype :adjustable t :fill-pointer 0))
         (i 0))
    (loop
       (unless script (return))
       (destructuring-bind (action . args) (pop script)
         (ecase action
           (:conflict
            (setf script (append (meld-scripts (first args) (second args))
                                 script)))
           (:same
            (assert (< i len))
            (assert (equalp args (elt original i)))
            (incf i)
            (vector-push-extend args result))
           (:insert
            (assert (typep args etype))
            (vector-push-extend args result))
           (:delete
            (assert (< i len))
            (assert (equalp args (elt original i)))
            (incf i))
           (:replace
            (assert (typep (cadr args) etype))
            (assert (< i len))
            (assert (equalp (car args) (elt original i)))
            (incf i)
            (vector-push-extend (cadr args) result))
           (:recurse
            (assert (< i len))
            (let ((vals (multiple-value-list
                         (apply #'ast-patch* (elt original i) args keys))))
              (dolist (v vals) (vector-push-extend v result)))
            (incf i))
           (:insert-sequence
            (assert (typep args 'sequence))
            (map nil (lambda (e)
                       (assert (typep e etype))
                       (vector-push-extend e result))
                 args))
           (:delete-sequence
            (assert (typep args 'sequence))
            (let ((arg-len (length args)))
              (assert (<= (+ i arg-len) len))
              (assert (equalp args (subseq original i (+ i arg-len))))
              (incf i arg-len)))
           (:same-sequence
            (assert (typep args 'sequence))
            (let ((arg-len (length args)))
              (assert (<= (+ i arg-len) len))
              (map nil (lambda (e)
                         (assert (typep e etype))
                         (assert (equalp e (elt original i)))
                         (incf i)
                         (vector-push-extend e result))
                   args))))))
    (loop while (< i len)
       do (vector-push-extend (elt original i) result)
       do (incf i))
    ;; Make the result simple again
    (copy-seq result)))

(defmethod ast-patch* ((original simple) script
                       &rest keys &key &allow-other-keys)
  (let ((new-unpacked-genome
         (apply #'ast-patch* (simple-genome-unpack (genome original))
                script keys)))
    (let ((patched (copy original)))
      (setf (genome patched) (simple-genome-pack new-unpacked-genome))
      patched)))

(defgeneric ast-meld-p (ast)
  (:documentation
   "Returns true if the children of AST are to be combined on merge conflict.")
  (:method ((ast ast))
    (ast-class-meld? (ast-class ast) ast)))

(defgeneric ast-class-meld? (ast-class ast)
  (:documentation
   "Dispatches on the ast-class of an ast to compute `ast-meld-p'")
  (:method ((ast-class t) (ast t)) nil)
  (:method ((ast-class (eql :TopLevel)) ast)
    (declare (ignorable ast))
    t))

(defgeneric patch-files (soft file-diffs &rest args &key &allow-other-keys)
  (:documentation "Apply the DIFFs in file-diffs to the files of SOFT.
FILE-DIFFS is an alist mapping strings (?) to diffs, which are as
in AST-PATCH.  Returns a new SOFT with the patched files."))

(defun print-diff
    (diff &key
            (stream *standard-output*)
            (no-color nil)
            (delete-start (if no-color "[-" (format nil "~a[-" +color-RED+)))
            (delete-end (if no-color "-]" (format nil "-]~a" +color-RST+)))
            (insert-start (if no-color "{+" (format nil "~a{+" +color-GRN+)))
            (insert-end (if no-color "+}" (format nil "+}~a" +color-RST+)))
            (sort-insert-delete t))
  "Return a string form of DIFF suitable for printing at the command line.
Numerous options are provided to control presentation."
  (let ((*print-escape* nil)
        (*deletep* nil)
        (*insertp* nil)
        (insert-buffer nil)
        (delete-buffer nil))
    (declare (special *insertp* *deletep*))
    (labels ((%p (c) (if (null c)
                         (princ "()" stream)
                         (write (continue-color (source-text c)) :stream stream)))
             (continue-color (text)
               (cond
                 (no-color text)
                 (*deletep*
                  (regex-replace-all (string #\Newline) text
                                     (format nil "~%~a" +color-RED+)))
                 (*insertp*
                  (regex-replace-all (string #\Newline) text
                                     (format nil "~%~a" +color-GRN+)))
                 (t text)))
             (purge-insert ()
               (setf *insertp* t)
               (when insert-buffer
                 (mapc #'%p (reverse insert-buffer))
                 (write insert-end :stream stream)
                 (setf insert-buffer nil))
               (setf *insertp* nil))
             (purge-delete ()
               (setf *deletep* t)
               (when delete-buffer
                 (mapc #'%p (reverse delete-buffer))
                 (write delete-end :stream stream)
                 (setf delete-buffer nil))
               (setf *deletep* nil))
             (push-insert (c)
               (unless (equal c "")
                 (purge-delete)
                 (unless insert-buffer (write insert-start :stream stream))
                 (push c insert-buffer)))
             (push-inserts (l) (mapc #'push-insert l))
             (push-delete (c)
               (unless (equal c "")
                 (purge-insert)
                 (unless delete-buffer (write delete-start :stream stream))
                 (push c delete-buffer)))
             (push-deletes (l) (mapc #'push-delete l))
             (purge ()
               (purge-insert)
               (purge-delete))
             (pr (c) (purge) (%p c))
             (%print-wrap (content)
               (destructuring-bind (sub-diff path left-wrap
                                             right-wrap . rest)
                   content
                 (declare (ignore path rest))
                 (mapc #'push-inserts left-wrap)
                 (%print-diff sub-diff)
                 (mapc #'push-inserts (reverse right-wrap))))
             (%print-unwrap (content)
               (destructuring-bind (sub-diff path left-wrap right-wrap)
                   content
                 (declare (ignore path))
                 (mapc #'push-deletes left-wrap)
                 (%print-diff sub-diff)
                 (mapc #'push-deletes (reverse right-wrap))))
             (%print-diff (diff)
               (case (car diff)
                 ((:wrap) (%print-wrap (cdr diff)))
                 ((:unwrap) (%print-unwrap (cdr diff)))
                 (t
                  (assert (every #'consp diff))
                  (when sort-insert-delete
                    (setf diff (simplify-diff-for-printing diff)))
                  (mapc (lambda-bind ((type . content))
                                     (ecase type
                                       (:same (pr content))
                                       (:delete (push-delete content))
                                       (:insert (push-insert content))
                                       ;; The :replace case is used only when
                                       ;; sort-insert-delete is NIL.  Otherwise,
                                       ;; these have been broken up into :insert
                                       ;; and :delete already                                     
                                       (:replace
                                        (push-insert (cadr content))
                                        (push-delete (car content)))
                                       (:recurse (%print-diff content))
                                       (:same-sequence (map nil #'pr content))
                                       (:insert-sequence
                                        (map nil #'push-insert content))
                                       (:delete-sequence
                                        (map nil #'push-delete content))
                                       (:same-tail (map nil #'pr content))
                                       (:wrap-sequence (%print-wrap (cdr content)))
                                       (:unwrap-sequence (%print-unwrap content))
                                       (:recurse-tail
                                        (%print-diff
                                         (remove-if
                                          (lambda (e)
                                            (or (equal e '(:delete))
                                                (equal e '(:insert))))
                                          content)))))
                        diff)))))
      (%print-diff diff)
      (purge)
      (values))))

(defun simplify-diff-for-printing (diff)
  "Rearrange DIFF so that in each chunk of inserts and delete, the
   inserts precede the deletes.  Also, flatten :RECURSE and
   remove :SAME of empty strings."
  (let ((saved-deletes nil))
    (nconc
     (iter (while diff)
           (let ((d (pop diff)))
             (flet ((%pop ()
                      (appending (nreverse saved-deletes))
                      (setf saved-deletes nil)
                      (collecting d)))
               (case (car d)
                 (:insert (collecting d))
                 (:delete (push d saved-deletes))
                 (:recurse (if (listp (cadr d))
                               (setf diff (append (cdr d) diff))
                               (%pop)))
                 (:replace (collecting `(:insert . ,(caddr d)))
                  (push `(:delete ,(cadr d)) saved-deletes))
                 (t
                  (unless (equal d '(:same . ""))
                    (%pop)))))))
     (nreverse saved-deletes))))


;;; Merge algorithms


(defgeneric converge (my old your &key conflict &allow-other-keys)
  (:documentation
   "Merge changes in MY and YOUR when both are descended from OLD.
Compute a version of OLD that is the result of trying to apply to OLD
both the changes from OLD -> MY and the changes from OLD -> YOUR.
Returns this object, and a second argument that describes problems
that were encountered, or NIL if no problems were found.  If CONFLICT
is true then insert conflict objects into the result AST."))

(defmethod converge ((branch-a t) (original t) (branch-b t)
                     &key conflict ((:base-cost *base-cost*) *base-cost*)
                       &allow-other-keys)
  "Assumes the arguments are things that can be treated as ASTs or SEXPRs."
  (let ((original (astify original))
        (branch-a (astify branch-a))
        (branch-b (astify branch-b)))
    (multiple-value-bind (diff problems)
        (merge3 original branch-a branch-b :conflict conflict)
      (values (unastify (ast-patch original diff :meld? (not conflict) :conflict conflict))
              problems))))

(declaim (special *unstable*))

(defun merge2 (branch-a branch-b &rest args &key &allow-other-keys)
  "Find an object that contains branch-a and branch-b as substructures.
Do this by computing the diff from branch-a to branch-b, then not performing
the deletions in that diff."
  (let ((diff (apply #'ast-diff branch-a branch-b args)))
    (ast-patch branch-a diff :delete? nil)))

(defvar *conflict* nil "Holds the CONFLICT parameter for MERGE3")

(defun merge3 (original branch-a branch-b &rest args
               &key conflict ((:base-cost *base-cost*) *base-cost*)
                 &allow-other-keys)
  "Find a version of that is a plausible combination of the changes from
ORIGINAL -> BRANCH-A and ORIGINAL -> BRANCH-B.  Return the edit sequence
from ORIGINAL to this merged version.   Also return a second value that
is true if a clean merge could be found; otherwise, it is a list of
unstable differences.  CONFLICT controls how merge conflicts are handled."
  (let ((*unstable* nil))
    (values
     (let ((*conflict* conflict))
       (merge-diffs2 (apply #'ast-diff original branch-a args)
                     (apply #'ast-diff original branch-b args)))
     *unstable*)))

(defun record-unstable (o-a o-b)
  (push (list (car o-a) (car o-b)) *unstable*))

(defun handle-conflict (o-a o-b &key (unstable t) leave-a leave-b use-b)
  (assert (consp o-a))
  (assert (consp o-b))
  (values
   (if (or (equalp (car o-a) (car o-b))
           (and (not *conflict*)
                (not unstable)))
       (list (car (if (or leave-a use-b) o-b o-a)))
       (progn
         (record-unstable o-a o-b)
         (flet ((%f (x) (when x `(,(car x)))))
           `((:conflict ,(unless leave-a (%f o-a))
                        ,(unless leave-b (%f o-b)))))))
   (if leave-a o-a (cdr o-a))
   (if leave-b o-b (cdr o-b))))

;; New implementation of merge-diffs that uses methods for dispatching
;; on different combinations of diff symbols
(defgeneric merge-diffs-on-syms (sym-a sym-b diff-a diff-b)
  (:documentation "Merge two diffs DIFF-A and DIFF-b, where
sym-a is (caar diff-a) and sym-b is (caar diff-b).  Return
three things: a list to be appended to the final merged diff,
a tail of diff-a, and a tail of diff-b.")
  ;; sym-a is :same
  (:method ((sym-a (eql :same)) (sym-b (eql :same)) o-a o-b)
    (handle-conflict o-a o-b))
  (:method ((sym-a (eql :same)) (sym-b null) o-a o-b)
    (handle-conflict o-a o-b))
  (:method ((sym-a (eql :same)) (sym-b (eql :insert)) o-a o-b)
    (handle-conflict o-a o-b :unstable nil :leave-a t))
  ;; (values (list (car o-b)) o-a (cdr o-b))
  (:method ((sym-a (eql :same)) (sym-b (eql :delete)) o-a o-b)
    (handle-conflict o-a o-b :unstable nil :use-b t))
  (:method ((sym-a (eql :same)) (sym-b (eql :replace)) o-a o-b)
    (handle-conflict o-a o-b :unstable nil :use-b t))
  (:method ((sym-a (eql :same)) (sym-b (eql :recurse)) o-a o-b)
    (handle-conflict o-a o-b :use-b t :unstable nil))

  ;;; Leaving these as-is for now, no conflict handler
  (:method ((sym-a (eql :same)) (sym-b (eql :same-tail)) o-a o-b)
    ;; The tail should never be a cons
    ;; cdr o-b should be nil
    (record-unstable o-a o-b)
    (values (list (car o-b)) nil (cdr o-b)))
  (:method ((sym-a (eql :same)) (sym-b (eql :recurse-tail)) o-a o-b)
    (record-unstable o-a o-b)
    (values (list (car o-a)) nil (cdr o-b)))

  ;; sym-a is :insert
  (:method ((sym-a (eql :insert)) (sym-b (eql :insert)) o-a o-b)
    (handle-conflict o-a o-b))
  (:method ((sym-a (eql :insert)) (sym-b (eql :replace)) o-a o-b)
    (if (equal? (cdar o-a) (caddar o-b))
        (handle-conflict o-a o-b :unstable nil :use-b t)
        (call-next-method)))
  ;; default cases for :insert
  (:method ((sym-a (eql :insert)) (sym-b t) o-a o-b)
    (handle-conflict o-a o-b :unstable nil :leave-b t))
  (:method ((sym-a t) (sym-b (eql :insert)) o-a o-b)
    (handle-conflict o-a o-b :unstable nil :leave-a t))
  ;; sym-a is :delete
  (:method ((sym-a (eql :delete)) (sym-b (eql :delete)) o-a o-b)
    (handle-conflict o-a o-b))
  (:method ((sym-a (eql :delete)) (sym-b (eql :recurse)) o-a o-b)
    (handle-conflict o-a o-b))
  (:method ((sym-a (eql :delete)) (sym-b (eql :insert)) o-a o-b)
    (record-unstable o-a o-b)
    ;; Do insert first, keep the delete around
    (handle-conflict o-a o-b :unstable nil :leave-a t))
  (:method ((sym-a (eql :delete)) (sym-b (eql :replace)) o-a o-b)
    (if (equal? (cdar o-a) (cadar o-b))
        (handle-conflict o-a o-b :unstable nil :use-b t)
        (handle-conflict o-a o-b :unstable t)))
  (:method ((sym-a (eql :delete)) (sym-b null) o-a o-b)
    (handle-conflict o-a o-b :leave-b t))
  (:method ((sym-a (eql :delete)) (sym-b (eql :same)) o-a o-b)
    (handle-conflict o-a o-b :unstable nil))

  ;;; Do not handle these for now
  (:method ((sym-a (eql :delete)) (sym-b (eql :same-tail)) o-a o-b)
    ;; should not happen?
    (record-unstable o-a o-b)
    (values () (cdr o-a) nil))
  (:method ((sym-a (eql :delete)) (sym-b (eql :recurse-tail)) o-a o-b)
    ;; should not happen?
    (record-unstable o-a o-b)
    (values () (cdr o-a) nil))

  ;; sym-a is :recurse
  (:method ((sym-a (eql :recurse)) (sym-b (eql :delete)) o-a o-b)
    (handle-conflict o-a o-b :unstable t))
  (:method ((sym-a (eql :recurse)) (sym-b (eql :replace)) o-a o-b)
    (handle-conflict o-a o-b :unstable t))
  (:method ((sym-a (eql :recurse)) (sym-b null) o-a o-b)
    (handle-conflict o-a o-b :unstable t :leave-b t))
  (:method ((sym-a (eql :recurse)) (sym-b (eql :recurse)) o-a o-b)
    (values (list (cons :recurse (merge-diffs2 (cdar o-a) (cdar o-b))))
            (cdr o-a) (cdr o-b)))
  (:method ((sym-a (eql :recurse)) (sym-b (eql :same)) o-a o-b)
    (handle-conflict o-a o-b :unstable nil))

  ;; Do not handle these for now
  (:method ((sym-a (eql :recurse)) (sym-b (eql :same-tail)) o-a o-b)
    ;; should not happen
    (record-unstable o-a o-b)
    (values () (cdr o-a) o-b))
  (:method ((sym-a (eql :recurse)) (sym-b (eql :recurse-tail)) o-a o-b)
    ;; should not happen
    (record-unstable o-a o-b)
    (values () (cdr o-a) o-b))

  (:method ((sym-a null) (sym-b null) o-a o-b)
    (error "Bad diff merge: ~A, ~A" o-a o-b))

  (:method ((sym-a null) (sym-b t) o-a o-b)
    (handle-conflict o-a o-b :leave-a t))

  (:method ((sym-a (eql :replace)) (sym-b (eql :same)) o-a o-b)
    (handle-conflict o-a o-b :unstable nil))
  (:method ((sym-a (eql :replace)) (sym-b (eql :insert)) o-a o-b)
    (if (equal? (caddar o-a) (cdar o-b))
        (handle-conflict o-a o-b :unstable nil)
        (handle-conflict o-a o-b :unstable t)))
  (:method ((sym-a (eql :replace)) (sym-b (eql :delete)) o-a o-b)
    (if (equal? (cadar o-a) (cdar o-b))
        (handle-conflict o-a o-b :unstable nil)
        (handle-conflict o-a o-b :unstable t)))
  (:method ((sym-a (eql :replace)) (sym-b (eql :replace)) o-a o-b)
    (handle-conflict o-a o-b :unstable t))
  (:method ((sym-a (eql :replace)) (sym-b (eql :recurse)) o-a o-b)
    (handle-conflict o-a o-b :unstable t))

  ;; do not handle these for now
  (:method ((sym-a (eql :same-tail)) (sym-b (eql :same)) o-a o-b)
    ;; should not happen
    (record-unstable o-a o-b)
    (values (list (car o-a)) (cdr o-a) nil))
  (:method ((sym-a (eql :same-tail)) (sym-b (eql :delete)) o-a o-b)
    ;; should not happen
    (record-unstable o-a o-b)
    (values (list (car o-a)) (cdr o-a) nil))
  (:method ((sym-a (eql :same-tail)) (sym-b (eql :recurse)) o-a o-b)
    ;; should not happen
    (record-unstable o-a o-b)
    (values (list (car o-a)) (cdr o-a) nil))
  (:method ((sym-a (eql :same-tail)) (sym-b (eql :same-tail)) o-a o-b)
    (cond
      ((equalp (car o-a) (car o-b))
       (values `(,(car o-a)) nil nil))
      (t
       (record-unstable o-a o-b)
       (values `((:conflict (,(car o-a)) (,(car o-b)))) nil nil))))
  (:method ((sym-a (eql :same-tail)) (sym-b (eql :recurse-tail)) o-a o-b)
    (values `(,(car o-b)) (cdr o-a) (cdr o-b)))

  (:method ((sym-a (eql :recurse-tail)) (sym-b (eql :same)) o-a o-b)
    ;; should not happen
    (record-unstable o-a o-b)
    (values (list (car o-b)) nil nil))
  (:method ((sym-a (eql :recurse-tail)) (sym-b (eql :delete)) o-a o-b)
    ;; should not happen
    (record-unstable o-a o-b)
    (values (list (car o-b)) nil (cdr o-b)))
  (:method ((sym-a (eql :recurse-tail)) (sym-b (eql :recurse)) o-a o-b)
    ;; should not happen
    (record-unstable o-a o-b)
    (values (list (car o-b)) nil (cdr o-b)))
  (:method ((sym-a (eql :recurse-tail)) (sym-b (eql :recurse-tail)) o-a o-b)
    (values (list (cons :recurse-tail (merge-diffs2 (cdar o-a) (cdar o-b))))
            (cdr o-a) (cdr o-b)))
  (:method ((sym-a (eql :recurse-tail)) (sym-b (eql :same-tail)) o-a o-b)
    (values (list (car o-a)) (cdr o-a) (cdr o-b)))

  ;; Unwind :*-sequence operations

  (:method ((sym-a (eql :insert-sequence)) (sym-b t) o-a o-b)
    (let ((new-o-a (nconc (map 'list (lambda (x) (cons :insert x)) (cdar o-a))
                          (cdr o-a))))
      (merge-diffs2 new-o-a o-b)))
  (:method ((sym-a t) (sym-b (eql :insert-sequence)) o-a o-b)
    (let ((new-o-b (nconc (map 'list (lambda (x) (cons :insert x)) (cdar o-b))
                          (cdr o-b))))
      (merge-diffs2 o-a new-o-b)))

  (:method ((sym-a (eql :delete-sequence)) (sym-b t) o-a o-b)
    (let ((new-o-a (nconc (map 'list (lambda (x) (cons :delete x)) (cdar o-a))
                          (cdr o-a))))
      (merge-diffs2 new-o-a o-b)))
  (:method ((sym-a t) (sym-b (eql :delete-sequence)) o-a o-b)
    (let ((new-o-b (nconc (map 'list (lambda (x) (cons :delete x)) (cdar o-b))
                          (cdr o-b))))
      (merge-diffs2 o-a new-o-b)))

  (:method ((sym-a (eql :same-sequence)) (sym-b t) o-a o-b)
    (setf o-a (same-seq-to-list o-a))
    (merge-diffs2 (same-seq-to-sames o-a) o-b))
  (:method ((sym-a t) (sym-b (eql :same-sequence)) o-a o-b)
    (setf o-b (same-seq-to-list o-b))
    (merge-diffs2 o-a (same-seq-to-sames o-b)))
  (:method ((sym-a (eql :same-sequence)) (sym-b (eql :same-sequence)) o-a o-b)
    (setf o-a (same-seq-to-list o-a))
    (setf o-b (same-seq-to-list o-b))
    (merge-diffs2 (same-seq-to-sames o-a) (same-seq-to-sames o-b))))

(defun same-seq-to-sames (o)
  (nconc (map 'list (lambda (x) (cons :same x)) (cdar o)) (cdr o)))

(defun same-seq-to-list (o)
  (assert (consp o))
  (assert (consp (car o)))
  (assert (eql (caar o) :same-sequence))
  (if (listp (cdar o))
      o
      (cons (cons :same-sequence (map 'list #'identity (cdar o))) (cdr o))))

(defun merge-diffs2  (orig-a orig-b &aux (o-a orig-a) (o-b orig-b))
  ;; Derived from CHUNK, but a bit smarter, and
  ;; produce an actual diff not a list of chunks
  ;; The last call to merge-diffs2-syms may return
  ;; an improper list.  Handle it specially (appending
  ;; cannot be used even if it is the last thing
  (cond
    ((and (listp (car orig-a)) (listp (car orig-b)))
     ;; to be appended.)
     (let* ((result (list nil))
            (last result)
            (collected))
       (iter (while o-a)
             (while o-b)
             (setf (values collected o-a o-b)
                   (merge-diffs2-syms o-a o-b))
             (setf (cdr last) collected)
             (while (proper-list-p collected))
             (setf last (last last)))
       (if (or o-a o-b)
           ;; collected must be a proper list
           (append
            (cdr result)
            (if *conflict*
                (if o-a
                    (iter (for action in o-a)
                          (collect `(:conflict (,action) nil)))
                    (iter (for action in o-b)
                          (collect `(:conflict nil (,action)))))
               (append o-a o-b)))
           (cdr result))))
    (t
     (assert (eql (car orig-a) (car orig-b)))
     (assert (symbolp (car orig-a)))
     (ecase (car orig-a)
       (:alist
        (let ((diff (merge-diffs2 (list orig-a) (list orig-b))))
          ;; (format t "~A~%" diff)
          diff)))))) ; (car diff)

(defun merge-diffs2-syms (o-a o-b)
  (merge-diffs-on-syms (caar o-a) (caar o-b) o-a o-b))

;;; TODO: printing clang-ast-node should use a safer printer ~s.
(defun show-chunks (chunks &optional (stream t))
  (mapc (lambda (chunk)
          (if (keywordp (car chunk))
              (ecase (car chunk)
                (:stable (format stream "~a" (cdr chunk)))
                (:unstable (format stream "+{UNSTABLE}+")))
              (show-chunks chunk)))
        chunks))

;;; Another algorithm for good common subsequences, more robust in the
;;; face of elements that occur with high frequency.  Instead, focus
;;; on elements that occur just once in each list, and grow
;;; subsequences from those.

(defstruct good-common-subsequences
  (count 0 :type fixnum)
  (positions-1 nil :type list)
  (positions-2 nil :type list))

(defun good-common-subsequences2 (s1 s2 &key (test #'eql))
  (let* ((table (make-hash-table :test test))
         (v1 (map 'vector #'identity s1))
         (v2 (map 'vector #'identity s2))
         (l1 (length v1))
         (l2 (length v2)))
    (macrolet ((init-table (v fn)
                 `(iter (for x in-vector ,v)
                        (for i from 0)
                        (let ((g (gethash x table)))
                          (unless g
                            (setf (gethash x table)
                            (setf g (make-good-common-subsequences))))
                          (incf (good-common-subsequences-count g))
                          (push i (,fn g))))))
      (init-table v1 good-common-subsequences-positions-1)
      (init-table v2 good-common-subsequences-positions-2))
    #+gcs2-debug
    (progn
      (format t "v1 = ~A~%" v1)
      (format t "v2 = ~A~%" v2)
      (format t "l1 = ~a~%" l1)
      (format t "l1 = ~a~%" l2))
    ;; Walk v1, find those elements that occur just once in each
    ;; sequence.  When found, grow the largest common contiguous
    ;; subsequence around each of the two points.  These may
    ;; end up being out of order, and perhaps overlapping, so
    ;; we'll select the ones to actually use by a greedy algorithm.
    (let ((candidates nil)
          (i 0))
      (iter (while (< i l1))
            #+gcs2-debug (format t "i = ~A~%" i)
            (let* ((x (svref v1 i))
                   (g (gethash x table)))
              #+gcs2-debug (format t "x = ~A, g = ~A~%" x g)
              (if (and (= (good-common-subsequences-count g) 2)
                       (good-common-subsequences-positions-1 g)
                       (good-common-subsequences-positions-2 g))
                ;; x occurs precisely once in each sequence
                (let ((j (car (good-common-subsequences-positions-2 g))))
                  (assert (= (car (good-common-subsequences-positions-1 g)) i))
                  (let ((start1 i)
                        (start2 j)
                        (end1 (1+ i))
                        (end2 (1+ j)))
                    (iter (while (> start1 0))
                          (while (> start2 0))
                          (while (funcall test
                                          (svref v1 (1- start1))
                                          (svref v2 (1- start2))))
                          (decf start1)
                          (decf start2))
                    (iter (while (< end1 l1))
                          (while (< end2 l2))
                          (while (funcall test (svref v1 end1)
                                          (svref v2 end2)))
                          (incf end1)
                          (incf end2))
                    ;; At this point, the subsequences of v1 and v2
                    ;; from start1 to end1-1 and start2 to end2-1 are
                    ;; maximal contiguous subsequences containing
                    ;; v1[i] and v2[j]. Record them.
                    (push (list start1 start2 (- end1 start1))
                          candidates)
                    (setf i end1)))
                (incf i))))
      (setf candidates (nreverse candidates))
      #+gcs2-debug (format t "candidates = ~A~%" candidates)
      ;; sort subsequences into decreasing order by length
      (setf candidates (stable-sort candidates #'> :key #'caddr))
      #+gcs2-debug (format t "candidates = ~A~%" candidates)
      ;; All candidates should be disjoint
      (let ((selected-triples nil))
        (iter (for triple in candidates)
              (for (s21 s22 l2) = triple)
              (when
                  ;; Reject triples when they break ordering with
                  ;; previous triples The triples should never
                  ;; overlap.
                  (iter (for (s11 s12 l1) in selected-triples)
                        (assert (/= s11 s21))
                        (always (if (< s21 s11)
                                    (and (<= (+ s21 l2) s11)
                                         (<= (+ s22 l2) s12))
                                    (and (>= s21 (+ s11 l1))
                                         (>= s22 (+ s12 l1))))))
                (push triple selected-triples)))
        (sort selected-triples #'< :key #'car)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conversion functions for handling variants of ast nodes

;;; The problem solved here is to put different kinds of
;;; ast nodes into a form ameanable to the diff and patch
;;; algorithms, so they may be handled together.

(defgeneric standardized-children (ast)
  (:documentation "Obtain a standardized CHILDREN list for ast.

Return the children as a flat list, in textual order, of
text nodes, slot markers, and children. The same slot marker may occur
more than once.

A standardized child list is a flat list, in textual order, of (1)
text nodes, (3) child nodes, and (2) special slot-specifier nodes such
that the child nodes between slot-specifier n and slot-specifier n+1
belong to the slot n.

The same slot specifier may occur more than once."))

(defgeneric copy-with-standardized-children (ast standardized-children &key &allow-other-keys)
  (:documentation "Make a copy of AST, but use STANDARDIZED-CHILDREN
and convert it back to whatever internal form this kind of AST uses."))

(defmethod standardized-children ((ast ast))
  "The default method uses the ordinary CHILDREN function"
  (children ast))

(defmethod standardized-children ((ast non-homologous-ast))
  (check-child-lists ast)
  (nest
   (let ((itext (interleaved-text ast))
         (calist (children-slot-specifier-alist ast)))
     (assert (= ;; (1+ (length calist))
              (reduce #'+ calist :key (lambda (p) (length (cdr p)))
                                 :initial-value 1)
              (length itext))
             ()
             "Length mismatch in child alist, interleaved text lists:~%~a~%~s~%"
             calist itext))
   (flet ((child-slot-spec (child)
            "Return the spec for the slot that includes CHILD."
            (car (find child calist
                       :key #'cdr
                       :test #'member)))
          (interleave-text-with-alist (alist)
            "Given an alist of (child-spec . children), return a list
interleaving text nodes, slot specs, and children."
            (cons (pop itext)
                  (iter (for (child-spec . vals) in alist)
                        (appending
                         (cons child-spec
                               (iter (for v in vals)
                                     (collecting v)
                                     (collecting (pop itext))))))))))
   (if (not (ast-annotation ast :child-order))
       (interleave-text-with-alist calist))
   (let* ((sorted-children (sorted-children ast))
          ;; Gather runs of children having the same slot specifier.
          (specifier-runs (runs sorted-children :key #'child-slot-spec))
          ;; Return an "alist" of (slot-spec . children) where the same
          ;; slot-spec may occur more than once.
          (ordered-calist
           (mapcar (lambda (run)
                     (cons (child-slot-spec (car run))
                           run))
                   specifier-runs)))
     (interleave-text-with-alist ordered-calist))))

(defmethod copy-with-standardized-children ((ast ast) (children list) &rest args)
  "The default method uses ordinary copy, treating the children list
as the ordinary children list."
  (let ((new (apply #'copy ast :children children :stored-hash nil args)))
    (check-child-lists new)
    new))

(defun is-slot-specifier (obj)
  (typep obj 'slot-specifier))

(defun is-conflict-node-with-slot-specifier (node)
  (and (typep node 'conflict-ast)
       (some [{some #'is-slot-specifier} #'cdr]
             (conflict-ast-child-alist node))))

(defun has-conflict-node-before-slot-specifiers (children)
  (iter (for c in children)
        (typecase c
          (slot-specifier (return nil))
          (conflict-ast (return t)))))

(defmethod copy-with-standardized-children ((ast non-homologous-ast) (children list) &rest args)
  (declare (special *a* *c*))
  ;; Remember state; used for debugging on failure
  (setf *a* ast)
  (setf *c* children)
  (if (or (some #'is-conflict-node-with-slot-specifier children)
          (has-conflict-node-before-slot-specifiers children))
      ;; Conflict that prevents creation of a node here
      ;; Instead, combine any conflict nodes and move up to
      ;; the parent
      (combine-all-conflict-asts ast children)
      ;; Must extract the interleaved-text strings
      (multiple-value-bind (child-alist itext new-child-order)
          (unstandardize-children children)
        (if (and (equal? itext (interleaved-text ast))
                 (equal? (mapcar (lambda (p)
                                   (cons (ft::slot-specifier-slot (car p))
                                         (cdr p)))
                                 child-alist)
                         (children-alist ast))
                 (null args))
            ast
            (let ((calen (length child-alist)))
              (declare (ignorable calen))
              (setf child-alist
                    (iter (for ss in (child-slot-specifiers ast))
                          (let ((p (assoc ss child-alist)))
                            (collecting (or p (list ss))))))
              ;; These checks are problematic
              ;; Conditionalize them out for now
              #+nil
              (assert (eql calen (length (child-slots ast)))
                      ()
                      "CHILD-ALIST and (CHILD-SLOTS AST) have different lengths.~%~a~%~a~%~a~%~a" child-alist (child-slots ast) ast children)
              #+nil
              (assert (eql (length itext) (cl:reduce #'+ child-alist :key [#'length #'cdr] :initial-value 1))
                ()
                "Lengths mismatch:  itext = ~s, child-alist = ~a" itext child-alist)
              (let ((new (multiple-value-call #'copy-with-children-alist
                           ast child-alist
                           :stored-hash nil
                           :interleaved-text itext
                           (if (ast-annotation ast :child-order)
                               (values :annotations
                                       (acons :child-order
                                              new-child-order
                                              (ast-annotations ast)))
                               (values))
                           (values-list args))))
                (check-child-lists new)
                (check-interleaved-text new)
                new))))))

(defmethod combine-all-conflict-asts ((parent non-homologous-ast) (child-list list))
  (multiple-value-bind (alist def)
      (combine-conflict-asts-in-list child-list)
    (make-instance
     'conflict-ast
     :child-alist (iter (for (k . children) in alist)
                        (collecting (list k (copy-with-standardized-children parent children))))
     :default-children (list (copy-with-standardized-children parent def)))))

(defun unstandardize-children (children)
  "Extract interleaved-text from a standardized list of children,
introducing empty strings or merging strings as needed.

Return three values.

The first value is the new values of the child slots, grouped as an
alist of (slot . children), ordered by the first textual appearance of
each slot.

The second value is the interleaved text.

The third value is a list suitable for use as the :child-order
annotation."
  ;; Note that this fails if there's a non-string child that occurs
  ;; before any slot-specifier.  This may happen, for example,
  ;; if a conflict node is generated that "swallows" slot-specifiers.
  ;; For that reason, conflict nodes need to be moved up to the parent
  ;; level when that happens.
  (let ((s (make-string-output-stream))
        (itext nil)
        (child-alist nil)
        (current-slot nil)
        ;; Capture the current order of the first occurrence of each
        ;; of the slot specifiers.
        (ordering (ordering (filter (of-type 'slot-specifier) children)))
        (order nil))
    (loop
      (unless children (return))
      (let ((c (pop children)))
        (typecase c
          (string (write-string c s))
          (slot-specifier
           (setf current-slot c)
           (unless (assoc c child-alist)
             (push (list c) child-alist)))
          (otherwise
           (unless current-slot
             (error "Child ~a occurs before any slot-specifier" c))
           (push (get-output-stream-string s) itext)
           (let ((slot-pair (assoc current-slot child-alist)))
             (push c (cdr slot-pair))
             ;; The extra layer here (a cons in a list) is to mimic
             ;; the format of `ft:position'.
             (let ((slot (ft::slot-specifier-slot current-slot)))
               (push (list (cons slot (1- (length (cdr slot-pair)))))
                     order)))))))
    (push (get-output-stream-string s) itext)
    (dolist (p child-alist)
      (setf (cdr p) (nreverse (cdr p))))
    (values (sort child-alist ordering :key #'car)
            (nreverse itext)
            (nreverse order))))

(defgeneric check-child-lists (ast)
  (:documentation "Checks if the child slots of AST have appropriate
contents.  Uses ASSERT to cause failure if they do not."))

(defmethod check-child-lists (ast) ast) ;; default
(defmethod check-child-lists ((ast node))
  (let ((child-slots (child-slots ast)))
    (iter (for (name . arity) in child-slots)
          (let ((v (slot-value ast name)))
            (if (eql arity 1)
                (assert (not (consp v))
                        ()
                        "Arity 1 slot has a nonempty list: ~a, ~a"
                        name v)
                (assert (and (listp v)
                             (not (some #'listp v)))
                        ()
                        "General child slot has improper contents: ~a, ~a"
                        name v)))))
  ast)

;;; Informational printing of AST structure, used for debugging

(defun dump-ast (ast)
  (dump-ast* ast 0))

(defgeneric dump-ast* (ast indent)
  (:method ((ast t) (indent integer)) (format t "~v@{ ~}~s~%" indent ast))
  (:method ((ast functional-tree-ast) (indent integer))
    (format t "~v@{ ~}~a~%" indent (ast-class ast))
    (let ((n2 (+ indent 2)))
      (mapc {dump-ast* _ n2} (children ast))))
  (:method ((ast non-homologous-ast) (indent integer))
    (format t "~v@{ ~}~a~%" indent (ast-class ast))
    (format t "~v@{ ~}~s~%" indent (interleaved-text ast))
    (dolist (ss (child-slot-specifiers ast))
      (let ((s (ft::slot-specifier-slot ss))
            (n2 (+ indent 2)))
        (format t "~v@{ ~}~a:~%" indent s)
        (if (eql (ft::slot-specifier-arity ss) 1)
            (dump-ast* (slot-value ast s) n2)
            (mapc {dump-ast* _ n2} (slot-value ast s)))))))

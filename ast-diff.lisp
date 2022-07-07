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
   :resolve/core
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/simple
   :software-evolution-library/software/ir
   :resolve/string
   :metabang-bind)
  (:import-from :fare-quasiquote)
  (:import-from :trivia.fail :fail)
  (:shadowing-import-from :software-evolution-library/terminal
                          :+color-RED+ :+color-GRN+ :+color-RST+)
  (:shadowing-import-from :functional-trees
   :child-slots :slot-spec-slot
   :children-alist :slot-specifier
   :slot-specifier-slot)
  (:import-from :software-evolution-library/software/tree-sitter
   :tree-sitter-ast :output-transformation
   :computed-text :structured-text :choice-superclass)
  (:local-nicknames
   (:range :software-evolution-library/utility/range)
   (:ts :software-evolution-library/software/tree-sitter)
   (:iter :iterate))
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
   :*diff-strings-p*
   #+(or) :*ignore-whitespace*))
(in-package :resolve/ast-diff)
(in-readtable resolve-readtable)
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

(deftype edit-action ()
  '(member
    :bad
    :conflict
    :recurse :recurse-tail
    :insert :insert-sequence
    :delete :delete-sequence
    :replace
    :same :same-tail :same-sequence :same-or-recurse
    :wrap :wrap-sequence
    :unwrap :unwrap-sequence))

(defparameter *base-cost* 2
  "Basic cost of a diff, before adding costs of components.")

(defvar *ignore-whitespace* nil
  "If true, inserting or removing whitespace in a string has zero cost")

(declaim (boolean *diff-strings-p*))
(defvar *diff-strings-p* t
  "If true, descend into strings when computing diffs.")

(declaim (boolean *wrap* *wrap-sequences*))

(defvar *wrap* nil
  "If true, perform wrap/unwrap actions in diffs.")

(defvar *wrap-sequences* nil
  "If true, perform wrap-sequence/unwrap-sequence actions in diffs.")

(defvar *max-wrap-diff* 500
  "When *wrap* is true, this is the maximum size difference for
wrapping and unwrapping to be considered.")

(defun clength (x)
  "Compute the length of X in conses."
  (iter (while (consp x)) (pop x) (summing 1)))

(defgeneric ast-class (ast)
  (:documentation "Return the class of AST as a symbol.

For a real AST, this is just its class name, but it can be other
symbols for other types.")
  (:method ((x ast))
    (class-name-of x)))

(defun cons-cost (x)
  "Compute the cost of a tree of conses."
  (if (not (consp x)) 1
      (let ((conses nil))
        (let ((y x))
          (iter (while (consp y))
                (push y conses)
                (pop y)))
        (let ((cost 1))
          (iter (while conses)
                (incf cost (cons-cost (car (pop conses)))))
          cost))))

;; #+sbcl (declaim (optimize sb-cover:store-coverage-data))


;;; Interface functions.
(defgeneric ast-cost (ast)
  (:documentation "Return cost of AST.")
  (:method ((ast ast))
    (cl:reduce #'+ (children ast) :key #'ast-cost :initial-value 1))
  (:method ((ast tree-sitter-ast))
    (cl:reduce #'+ (standardized-children ast) :key #'ast-cost :initial-value 1))
  ;; Slot-specifiers are introduced as markers in the standardized-children
  ;; lists of tree-sitter-ast nodes, and should not contribute the
  ;; the cost.
  (:method ((ss slot-specifier)) 0)
  (:method ((ast t))
    1)
  (:method ((s string))
    (if *ignore-whitespace*
        (count-if-not #'whitespacep s)
        (length s)))
  (:method ((ast vector))
    (length ast))
  (:method ((ast null))
    1)
  (:method ((ast cons))
    (+ (iter (sum (ast-cost (pop ast)))
             (while (consp ast)))
       ;; cost of terminal NIL is 0
       (if ast (ast-cost ast) 0))))

(defgeneric ast-can-recurse (ast-a ast-b)
  (:documentation "Check if recursion is possible on AST-A and AST-B.  Strings
can be recursed on if `*diff-strings-p*' is true (defaults to true)")
  (:method ((ast-a string) (ast-b string))
    *diff-strings-p*)
  (:method ((ast-a t) (ast-b t))
    nil)
  (:method ((ast-a ast) (ast-b ast))
    t))

(defmethod source-text ((ast cons) &rest args &key stream)
  (iter (while (consp ast)) (apply #'source-text (pop ast) args))
  (when ast
    (write-string "." stream)
    (apply #'source-text ast args)))

(defmethod source-text ((ast slot-specifier) &key stream)
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
(defmethod source-text ((ast simple-lisp-ast) &key stream)
  (let ((v (unastify ast)))
    (if v
        (format stream "~a" (unastify v))
        (write-string "()" stream))))
(defmethod equal? ((a simple-lisp-ast) (b simple-lisp-ast))
  (equalp (unastify a) (unastify b)))

(defmethod ast-cost :around ((ast simple-lisp-ast))
  (ensure (cost-cache ast) (call-next-method)))
(defmethod ast-size :around ((ast simple-lisp-ast))
  (ensure (size-cache ast) (call-next-method)))
(defmethod copy :around ((obj simple-lisp-ast) &rest args
                         &key &allow-other-keys)
  (apply #'call-next-method obj
         ;; Don't copy the cache slots.
         :cost-cache nil :size-cache nil :unastify-cache nil
         args))

(defgeneric astify (x)
  (:documentation "Convert a Lisp data structure to a SIMPLE-LISP-AST")
  (:method ((x software))
    (genome x))
  (:method ((x simple))
    (lines x)))
(defgeneric unastify (x)
  (:documentation "Convert a SIMPLE-LISP-AST to a Lisp data structure"))

(def +end-marker+ :nil
  "Value used to mark the end of a proper list, as AST-DIFF treats NIL
  as a list.")

(defmethod source-text ((x (eql +end-marker+)) &rest args &key)
  (apply #'source-text "" args))
(defmethod astify ((x list))
  (if (proper-list-p x)
      ;; Add an end marker to represent the NIL
      ;; (because AST-DIFF treats NIL as a list)
      (make-instance 'simple-lisp-ast
                     :children (nconc (mapcar #'astify x) (list +end-marker+))
                     :unastify-cache x)
      ;; Properize the list
      (let ((original-x x)
            (properized-x
             (iter (collecting
                      (if (consp x)
                          (car x)
                          (progn
                            (assert (not (eql x +end-marker+)) ()
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
         (if (eql last-c +end-marker+)
             (mapcar #'unastify (butlast c))
             (nconc (mapcar #'unastify (butlast c))
                    last-c)))))

(defun unastify-lisp-diff (d)
  (typecase d
    (simple-lisp-ast (unastify d))
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
;;; Each node is a mapping from one edit segment in the original tree
;;; to another edit segment in the target tree. An edit segment
;;; represents a piece of the tree that is being changed by the a part
;;; of the edit script; it is essentially a pointer into the tree with
;;; a target node, an offset, and a length.

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
   (start :type (integer 0)
          :initarg :start
          :accessor edit-segment-start
          :documentation "The index (starting at 0) of the first
child in the edit segment"))
  (:documentation "Common slots of edit-segment classes"))

(defclass edit-segment (edit-segment-common)
  ((length :type (integer 0)
           :initarg :length
           :accessor edit-segment-length
           :documentation "The number of children (possibly zero)
in the edit segment"))
  (:documentation "An edit-segment points to a subset of the children
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

(defmethod source-text ((segment edit-segment) &rest args &key)
  (with-accessors ((start edit-segment-start)
                   (len edit-segment-length)
                   (ast-node edit-segment-node))
      segment
    (mapc (lambda (ast)
            (apply #'source-text ast args))
          (subseq (standardized-children ast-node) start (+ start len)))))

(defmethod source-text ((segment string-edit-segment) &key stream)
  (with-slots (node start string-start string-length)
      segment
    (assert node)
    (write-string (elt (children node) start) stream
                  :start string-start
                  :end (+ string-start string-length))))

(defmethod ast-to-list-form ((segment string-edit-segment))
  (source-text segment))

(def +list-ellipsis+ '(|...|))

(defmethod ast-to-list-form ((segment edit-segment))
  (with-accessors ((start edit-segment-start)
                   (len edit-segment-length)
                   (ast-node edit-segment-node))
      segment
    (let ((children (standardized-children ast-node)))
      `(,(ast-class ast-node)
        ,@(unless (eql start 0) +list-ellipsis+)
        ,@(mapcar #'ast-to-list-form
                  (subseq children
                          start (+ start len)))
        ,@(unless (eql (+ start len)
                       (length children))
            +list-ellipsis+)))))

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
  (print-unreadable-object (node stream :type t :identity nil)
    (format stream "~s"
            (ellipsize (source-text (edit-tree-node-source node)) 30))))

(defmethod slot-unbound (class (node edit-tree-node) (slot (eql 'size)))
  (declare (ignorable class))
  (setf (slot-value node slot)
        (reduce #'+ (edit-tree-node-children node)
                :key #'ast-size :initial-value 1)))

(defmethod ast-size ((segment string-edit-segment)) 1)

;; Cache for SIZE slot, accessed by ast-size
(defmethod slot-unbound (c (segment edit-segment) (slot (eql 'size)))
  (declare (ignorable c))
  (nest
   (with-accessors ((node edit-segment-node)
                    (length edit-segment-length)
                    (start edit-segment-start))
       segment)
   (let* ((children (subseq (standardized-children node) start (+ start length)))
          (value (reduce #'+ children :key #'ast-size :initial-value 1)))
     (setf (slot-value segment slot) value))))


;;; Main interface to calculating ast differences.

(defgeneric ast-diff* (ast-a ast-b)
  (:documentation
   "Return a least-cost edit script which transforms AST-A into AST-B.
Also return a second value indicating the cost of the edit.

See `ast-patch' for more details on edit scripts.

The following generic functions may be specialized to configure
differencing of specialized AST structures.; `equal?',
`ast-cost' and `ast-can-recurse'."))

;; The cache maps key pairs to values and counts

(def +ast-diff-cache+ (make-hash-table :size 1021))
(def +ast-diff-counter+ 0)
(declaim (type (integer 0 2000000000) +ast-diff-counter+))
(def +hash-upper-limit+ 100000000)

(defparameter *bucket-size* 20)

(defmethod ast-diff* :around (ast-a ast-b)
  (let* ((key (cons ast-a ast-b))
         (hash (ast-hash key))
         ;; val-alist maps keys to (val . count) pairs
         (val-alist (gethash hash +ast-diff-cache+))
         (pair (assoc key val-alist :test #'equal)))
    (cond
      ;; There is already a cached value.
      (pair
       (let ((vals (cadr pair)))
         (values (car vals) (cdr vals))))
      ((>= (length val-alist) *bucket-size*)
       ;; If a bucket gets too big, just stop caching
       ;; things that map there
       (call-next-method))
      (t
       (flet ((consolidate-insert-delete-pairs (diff)
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
                  (if changed new-diff diff))))
         (mvlet* ((diff cost (call-next-method))
                  (new-diff (consolidate-insert-delete-pairs diff))
                  (cost (if (eq new-diff diff) cost
                            (diff-cost new-diff)))
                  (diff new-diff))
           ;; Thin the cache if necessary.
           (when (>= +ast-diff-counter+ +hash-upper-limit+)
             (setf +ast-diff-counter+
                   (thin-ast-diff-table +ast-diff-cache+ +ast-diff-counter+)))
           (assert (< +ast-diff-counter+ +hash-upper-limit+))
           ;; Push the diff and cost to a hash bucket.
           (push (list* key (cons diff cost) +ast-diff-counter+)
                 (gethash hash +ast-diff-cache+))
           (incf +ast-diff-counter+)
           (values diff cost)))))))

(defun thin-ast-diff-table (cache counter)
  "Thin out the AST diff cache when it gets too big by dropping the
oldest half of the entries."
  (assert (>= counter +hash-upper-limit+))
  (let* ((h2 (ash +hash-upper-limit+ -1))
         (d (- counter h2)))
    (maphash (lambda (k v)
               (let* ((head (cons nil v))
                      (p head)
                      (n (cdr p)))
                 (loop
                   (unless n (return))
                   ;; The entries are lists of the form (KEY (DIFF .
                   ;; COST) . SAVED_COUNTER), where SAVED_COUNTER is
                   ;; the value of `ast-diff-counter' when the diff
                   ;; and cost were cached. If the saved counter is
                   ;; less than H2, we drop the entry; otherwise we
                   ;; decrement the saved counter so the entry will be
                   ;; in the older half the next time we thin the
                   ;; table.
                   (symbol-macrolet ((saved-counter (cddar n)))
                     (if (< saved-counter d)
                         (setf (cdr p) (cdr n))
                         (progn
                           (decf saved-counter d)
                           (shiftf p n (cdr n))))))
                 (unless (eql (cdr head) v)
                   (setf (gethash k cache) (cdr head)))))
             cache)
    h2))

(defun clear-ast-diff-table ()
  (setf +ast-diff-counter+ 0)
  (clrhash +ast-diff-cache+))

(defun ast-diff (ast-a ast-b
                 &key
                   ((:ignore-whitespace *ignore-whitespace*)
                    *ignore-whitespace*)
                   ((:strings *diff-strings-p*) *diff-strings-p*)
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
      (clear-ast-diff-table))))

(defgeneric same-class-p (object-a object-b)
  (:documentation "Return T if OBJECT-1 and OBJECT-2 are of the same class.")
  (:method ((ast-a ast) (ast-b ast)) (eql (ast-class ast-a) (ast-class ast-b)))
  (:method ((ast-a structured-text) (ast-b structured-text))
    (or (eql (ast-class ast-a) (ast-class ast-b))
        ;; NOTE: choice expansion subclassing allows for an AST
        ;;       with multiple possible representations/choices
        ;;       to have each possibility represented as its own
        ;;       subclass. In these cases, it's the superclass
        ;;       that really matters.
        (and (slot-exists-p ast-a 'choice-superclass)
             (slot-exists-p ast-b 'choice-superclass)
             (eql (choice-superclass ast-a) (choice-superclass ast-b))))))

(defmethod ast-diff* ((ast-a ast) (ast-b ast))
  #+debug (format t "ast-diff[AST] AST-CAN-RECURSE: ~S~%"
                  (ast-can-recurse ast-a ast-b))
  (multiple-value-bind (diff cost)
      ;; Initial result from diffing the standardized children.
      (when (same-class-p ast-a ast-b)
        (ast-diff*-lists (standardized-children ast-a)
                         (standardized-children ast-b)
                         ast-a ast-b))
    (when *wrap*
      ;; If wrapping makes the diff cheaper, use it.
      (multiple-value-bind (wrap-diff wrap-cost)
          (ast-diff-wrap ast-a ast-b)
        (when (and wrap-cost (or (null cost) (< wrap-cost cost)))
          (setf diff wrap-diff
                cost wrap-cost)))
      ;; If unwrapping makes the diff (even?) cheaper, use it.
      (multiple-value-bind (unwrap-diff unwrap-cost)
          (ast-diff-unwrap ast-a ast-b)
        (when (and unwrap-cost (or (null cost) (< unwrap-cost cost)))
          (setf diff unwrap-diff
                cost unwrap-cost))))
    ;; At this point we have the cheapest of three possible diffs, or
    ;; none.
    (if diff
        (values diff cost)
        (call-next-method))))

(defun map-ast-while/path (ast fn &optional path)
  "Apply FN to the nodes of AST A, stopping
the descent when FN returns NIL.  FN is also passed a PATH
argument, which is a list (in reverse order) of the indices
of children leading down to the node."
  (when (funcall fn ast path)
    (iter (for c in (children ast))
          (for i from 0)
          (when (typep c 'ast) (map-ast-while/path c fn (cons i path))))))

(defgeneric ast-diff-wrap (ast-a ast-b &key skip-root first-ast-child)
  (:documentation
   "Find a minimum cost 'wrap' edit, which wraps an AST in a larger AST."))

(defmethod ast-diff-wrap ((ast-a ast) (ast-b ast)
                          &key (skip-root t) first-ast-child
                          &aux (max-wrap-diff *max-wrap-diff*))
  ;; search over the ASTs under ast-b that are the same class as ast-a,
  ;; and for which the size difference is not too large
  (let* ((ast-a-cost (ast-cost ast-a))
         (a-class (ast-class ast-a))
         (max-cost (+ ast-a-cost max-wrap-diff))
         (min-cost (- ast-a-cost max-wrap-diff))
         (best-candidate nil)
         (best-cost most-positive-fixnum)
         ;; Do not also search for wraps in the recursive calls
         #+(or) (*wrap* nil))
    (when (integerp first-ast-child)
      (setf first-ast-child (elt (children ast-a) first-ast-child)))
    #+ast-diff-wrap-debug (format t "(ast-class ast-a) = ~S~%" a-class)
    (nest
     (map-ast-while/path ast-b)
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
                            &key (skip-root t) first-ast-child
                            &aux (max-wrap-diff *max-wrap-diff*))
  ;; search over the ASTs under ast-a that are the same class as ast-b,
  ;; and for which the size difference is not too large
  (let* ((ast-b-cost (ast-cost ast-b))
         (b-class (ast-class ast-b))
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
     (map-ast-while/path ast-a)
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
  (iter (while path)
        (assert (typep ast 'ast))
        (let ((c (children ast))
              (i (pop path)))
          (assert (<= 0 i))
          (assert (length< i c))
          (collect (ast-class ast) into classes)
          (multiple-value-bind (left-half right-half)
              (halves c i)
            (collect left-half into left)
            (collect (rest right-half) into right))
          (setf ast (elt c i))
          (assert (typep ast 'ast)))
        (finally
         #+ast-diff-debug (format t "Result: ~s ~s ~s~%" left right classes)
         (return (values left right classes)))))

(defun cost-of-wrap (wrap &aux (base-cost *base-cost*))
  "Computes the sum of the costs of the objects in a wrap"
  (reduce #'+ wrap
          :initial-value 0
          :key (lambda (w) (reduce #'+ w :key #'ast-cost :initial-value base-cost))))

(defmethod ast-diff-wrap ((ast-a t) (ast-b t) &key skip-root first-ast-child)
  (declare (ignore skip-root first-ast-child))
  nil)

(defmethod ast-diff-unwrap ((ast-a t) (ast-b t) &key skip-root first-ast-child)
  (declare (ignore skip-root first-ast-child))
  nil)

(-> ast-diff-wrap-sequence (ast sequence ast)
    (values (or list (eql :bad))
            fixnum))
(defun ast-diff-wrap-sequence (ast-a sub-a ast-b &aux (len (length sub-a)))
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
          (values :bad most-positive-fixnum)))))

(-> ast-diff-unwrap-sequence ((or ast fixnum) ast sequence)
    (values (or list (eql :bad))
            fixnum))
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
(map-ast-while/path ast-b)
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
  (labels ((test (a b)
             (cond ((and (typep a 'ast) (typep b 'ast))
                    (equal? a b))
                   ((and (not (typep a 'ast)) (not (typep b 'ast)))
                    (equalp a b))
                   (t nil)))
           (prefix (list-a list-b)
             (gcp (list list-a list-b) :test #'test))
           (postfix (list-a list-b)
             (gcs (list list-a list-b) :test #'test)))
    (let* ((prefix (prefix list-a list-b))
           (pre-length (length prefix))
           (a (drop pre-length list-a))
           (b (drop pre-length list-b)))
      ;; If either list is completely consumed by the prefix, return here.
      (if (or (null a) (null b))
          (values a b prefix nil)
          ;; Calculate the postfix (less the prefix) if necessary.
          (let* ((postfix (postfix a b))
                 (post-length (length postfix)))
            (values (butlast a post-length)
                    (butlast b post-length)
                    prefix
                    postfix))))))

(defun make-cache (total-a total-b)
  (make-array (list (1+ (clength total-a)) (1+ (clength total-b)))
              :initial-element nil))

(defstruct rd-node
  "Node in the recursive-diff computation graph"
  ;; Coordinates of the node in the r-d graph
  (a 0 :type array-index :read-only t)
  (b 0 :type array-index :read-only t)
  (in-arcs nil :type list) ;; list of arcs into this node
  (out-arcs nil :type list) ;; list of arcs out of this node
  (open-pred-count 0 :type (integer 0)) ;; number of predecessors that are still open
  (best-in-arc nil) ;; The in arc that gave the lowest cost to this point
  (cost 0 :type (integer 0)) ;; total cost to reach this node along best path
  )

(defmethod print-object ((node rd-node) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (node stream :type t :identity t)
        (format stream ":A ~a :B ~a"
                (rd-node-a node) (rd-node-b node)))))

(defstruct rd-link
  "Link in the computation graph for edits on sequences"
  (src (required-argument :src) :read-only t) ;; a b ;; indices of source node
  (dest (required-argument :dest) :read-only t) ;; destination node
  (cost nil) ;; Cost of this operation
  ;; The kind of edit operation to corresponding to the link
  (kind (required-argument :kind) :type edit-action :read-only t)
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

(declaim (inline rd-link-a rd-link-b))
(defun rd-link-a (e) (rd-node-a (rd-link-src e)))
(defun rd-link-b (e) (rd-node-b (rd-link-src e)))

(-> build-rd-graph (list list
                         &key (:wrap-sequences boolean)
                         (:unwrap-sequences boolean))
    (values (simple-array t (* *))
            (simple-array t (*))
            (simple-array t (*))))
(defun build-rd-graph (total-a total-b &key (wrap-sequences *wrap-sequences*)
                                         (unwrap-sequences *wrap-sequences*))
  "Construct the graph of a pair of lists.
The graph will have (* (1+ (length total-a)) (1+ (length total-b)))
nodes.

Return the 2d array of the nodes."
  (let* ((len-a (length total-a))
         (len-b (length total-b))
         (nodes (make-array (list (1+ len-a) (1+ len-b))))
         (vec-a (coerce total-a 'vector))
         (vec-b (coerce total-b 'vector)))
    ;; For now, we eagerly construct all possible wrap/unwrap edges
    ;; Prune these if they take too long
    (nest
     ;; NB Using loop here because we can't declare the type of
     ;; iterate index variables.
     (loop for a of-type array-index from 0 to len-a do)
     (loop for b of-type array-index from 0 to len-b do)
     (let ((node (make-rd-node :a a :b b)))
       (cond-every
        ((> b 0)
         (push (make-rd-link ;; :a a :b (1- b)
                :src (aref nodes a (1- b))
                :dest node
                :kind :insert)
               (rd-node-in-arcs node)))
        ((> a 0)
         (push (make-rd-link
                ;; :a (1- a) :b b
                :src (aref nodes (1- a) b)
                :dest node
                :kind :delete)
               (rd-node-in-arcs node))
         (when (> b 0 ) ;; (and (> b 0) (ast-can-recurse (aref vec-a (1- a))
           ;;                     (aref vec-b (1- b))))
           (push (make-rd-link
                  ;; :a (1- a) :b (1- b)
                  :src (aref nodes (1- a) (1- b))
                  :dest node
                  :kind :same-or-recurse)
                 (rd-node-in-arcs node))))
        ((and wrap-sequences
              (> b 0)
              (> a 1))
         (iter (for x from 0 to (- a 2))
               (push (make-rd-link
                      ;; :a x :b b
                      :src (aref nodes x (1- b))
                      :dest node
                      :kind :wrap-sequence)
                     (rd-node-in-arcs node))))
        ((and unwrap-sequences
              (> a 0)
              (> b 1))
         (iter (for x from 0 to (- b 2))
               (push (make-rd-link
                      ;; :a a :b x
                      :src (aref nodes (1- a) x)
                      :dest node
                      :kind :unwrap-sequence)
                     (rd-node-in-arcs node)))))
       (setf (rd-node-open-pred-count node)
             (length (rd-node-in-arcs node)))
       (setf (aref nodes a b) node)))
    ;; Fill in out-arcs
    (nest
     ;; NB Using loop here because we can't declare the type of
     ;; iterate index variables.
     (loop for a of-type array-index from 0 to len-a do)
     (loop for b of-type array-index from 0 to len-b do)
     (let* ((node (aref nodes a b))
            (in-arcs (reverse (rd-node-in-arcs node))))
       (iter (for ia in in-arcs)
             (let ((pa (rd-link-a ia))
                   (pb (rd-link-b ia)))
               (push ia (rd-node-out-arcs (aref nodes pa pb)))))))

    (values nodes vec-a vec-b)))

(defun reconstruct-path-to-node (nodes node)
  (declare ((simple-array t (* *)) nodes)
           (rd-node node))
  (let ((ops nil))
    (iter (for pred-arc = (rd-node-best-in-arc node))
          (while pred-arc)
          (setf ops (append (rd-link-op pred-arc) ops))
          ;; (push (rd-link-op pred-arc) ops)
          (setf node (aref nodes
                           (rd-link-a pred-arc)
                           (rd-link-b pred-arc))))
    (assert (eql (rd-node-a node) 0))
    (assert (eql (rd-node-b node) 0))
    ops))

(defun compute-best-paths (nodes vec-a vec-b parent-a parent-b)
  "Compute the best paths across NODES, a 2d graph, using fringe search."
  (declare ((simple-array t (* *)) nodes)
           ((simple-array t (*)) vec-a vec-b))
  (let ((fringe (queue))
        (total-open 1))
    (declare (dynamic-extent fringe))
    ;; Start at the (0,0) node, which is the () -> ()
    ;; diff and has zero cost
    (let ((start (aref nodes 0 0)))
      (setf (rd-node-cost start) 0)
      (enq start fringe))
    (iter (for node = (deq fringe))
          (until (zerop total-open))
          (decf total-open)
          ;; Compute the costs of all arcs into this node
          (dolist (in-arc (rd-node-in-arcs node))
            (assert (eql (rd-link-dest in-arc) node))
            (compute-arc-cost in-arc nodes vec-a vec-b parent-a parent-b)
            (let ((best (rd-node-best-in-arc node))
                  (in-arc-cost
                   (+ (rd-node-cost (rd-link-src in-arc))
                      (rd-link-cost in-arc))))
              #+ast-diff-debug (format t "best = ~a~%" best)
              (when (or (null best)
                        (> (rd-node-cost node) in-arc-cost))
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
                                 (rd-link-cost in-arc)))
                      (format t "Best ~a (~a)~%" (rd-link-op in-arc) (rd-link-cost in-arc))))
                (setf (rd-node-best-in-arc node) in-arc
                      (rd-node-cost node) in-arc-cost))))
          ;; See if any successor are now ready to be handled
          (dolist (out-arc (rd-node-out-arcs node))
            (let ((dest (rd-link-dest out-arc)))
              (when (zerop (decf (rd-node-open-pred-count dest)))
                ;; All predecessors have been computed, queue this node
                (enq dest fringe)
                (incf total-open)))))))

(defun compute-arc-cost (arc nodes vec-a vec-b parent-a parent-b)
  "Compute the cost of an RD arc"
  (declare ((simple-array t (* *)) nodes)
           ((simple-array t (*)) vec-a)
           ((simple-array t (*)) vec-b))
  (let* ((src (aref nodes (rd-link-a arc) (rd-link-b arc)))
         (dest (rd-link-dest arc))
         (dest-a (rd-node-a dest))
         (dest-b (rd-node-b dest))
         (a (when (> dest-a 0) (aref vec-a (1- dest-a))))
         (b (when (> dest-b 0) (aref vec-b (1- dest-b)))))
    (declare (array-index dest-a dest-b))
    (let ((op
           (ecase (assure edit-action (rd-link-kind arc))
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
      (values op (setf (rd-link-cost arc) (reduce #'+ op :key #'diff-cost :initial-value 0))))))

(defun recursive-diff (total-a total-b parent-a parent-b)
  (flet ((%r (w uw)
           (multiple-value-bind (nodes vec-a vec-b)
               (build-rd-graph total-a total-b
                               :wrap-sequences w
                               :unwrap-sequences uw)
             (compute-best-paths nodes vec-a vec-b parent-a parent-b)
             (reconstruct-path-to-node nodes (aref nodes (length vec-a) (length vec-b))))))
    (let ((without-wrap-seq-diff (%r nil nil)))
      ;; Only try wrap-sequences if enabled and (un)wrapping actually helped
      (let ((w nil)
            (uw nil)
            (i 0) (j 0)
            #+ast-diff-wrap-sequence-debug
            (entered t))
        (declare (boolean w uw))
        (when *wrap-sequences*
          (iter (for d in without-wrap-seq-diff)
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
            without-wrap-seq-diff)))))

(defun diff-cost (diff &aux (base-cost *base-cost*))
  "Computes the cost of a diff"
  (cond
    ((eql diff :bad) most-positive-fixnum)
    ((not (consp diff)) 0)
    ((symbolp (car diff))
     (ecase (assure edit-action (car diff))
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
               (cost-of-wrap (sixth diff)))))))
    (t
     (reduce #'+ diff :key #'diff-cost :initial-value 0))))

(defun add-common (diff cost prefix postfix)
  #+ast-diff-debug (format t "add-common: ~a ~a~%" diff cost)
  (flet ((mark-same (list)
           (mapcar (op (cons :same _)) list)))
    (values
     (append
      (mark-same prefix)
      (if (equal '(:same-tail) (lastcar diff))
          (butlast diff)
          diff)
      (mark-same postfix))
     cost)))

(defun ast-diff-on-lists (ast-a ast-b parent-a parent-b)
  (assert (proper-list-p ast-a))
  (assert (proper-list-p ast-b))
  ;; Drop common prefix and postfix, just run the diff on different middle.
  (multiple-value-bind (unique-a unique-b prefix postfix)
      (remove-common-prefix-and-suffix ast-a ast-b)
    ;; NOTE: We assume that the top level is a list (not a cons tree).
    ;; This is true for any ASTs parsed from a source file as a
    ;; sequence of READs.
    (cond
      ((not (or unique-a unique-b))
       (add-common nil 0 prefix postfix))
      ((null unique-a)
       (add-common (mapcar (lambda (el) (cons :insert el)) unique-b)
                   (1- (cons-cost unique-b)) ; 1- for trailing nil.
                   prefix postfix))
      ((null unique-b)
       (add-common
        (mapcar (lambda (el) (cons :delete el)) unique-a)
        (1- (cons-cost unique-a))  ; 1- for trailing nil.
        prefix postfix))
      (t
       (let ((rdiff (recursive-diff unique-a unique-b parent-a parent-b)))
         (add-common rdiff (diff-cost rdiff) prefix postfix))))))

(defun ast-hash-with-check (ast table)
  "Calls AST-HASH, but checks that if two ASTs have the same hash value,
they are actually equal.  If not, the second one gets a new, fresh hash
value that is used instead."
  (mvlet* ((hash (ast-hash ast))
           (old-ast present? (gethash hash table)))
    (if (not present?)
        (setf (gethash hash table) ast)
        (when (and old-ast (not (equal? ast old-ast)))
          (iter (incf hash) ; may be >= sel/sw/parseable::+ast-hash-base+, but that's ok
                (while (gethash hash table)))
          (setf (gethash hash table) ast)))
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
  (ast-diff*-lists (coerce ast-a 'vector) (coerce ast-b 'vector)
                   parent-a parent-b))

(defmethod ast-diff*-lists ((ast-a vector) (ast-b vector) parent-a parent-b)
  #+ast-diff-debug (format t "ast-a = ~a~%ast-b = ~a~%" ast-a ast-b)
  (mvlet* ((table (make-hash-table :size (+ (length ast-a) (length ast-b))))
           (hashes-a (map '(simple-array fixnum (*))
                          (lambda (ast) (ast-hash-with-check ast table))
                          ast-a))
           (hashes-b (map '(simple-array fixnum (*))
                          (lambda (ast) (ast-hash-with-check ast table))
                          ast-b))
           (subseq-triples (good-common-subsequences2 hashes-a hashes-b))
           ;; split ast-a and ast-b into subsequences
           ;; Get lists of subsequences on which they differ, and subsequences on
           ;; which they are equal.  Some of the former may be empty.
           (diff-a common-a
            (split-into-subsequences
             ast-a
             (map 'list (lambda (x) (list (car x) (caddr x)))
                  subseq-triples)))
           (diff-b common-b
            (split-into-subsequences
             ast-b
             (map 'list (lambda (x) (list (cadr x) (caddr x)))
                  subseq-triples))))
    (assert (length= diff-a diff-b))
    (assert (length= common-a common-b))
    ;; (assert (= (length diff-a) (1+ (length common-a))))
    (let ((overall-diff nil)
          (overall-cost 0))
      (iter (for da in (mapcar (op (coerce _ 'list)) (reverse diff-a)))
            (for db in (mapcar (op (coerce _ 'list)) (reverse diff-b)))
            (for ca in (reverse (cons nil common-a)))
            ;; Add the differences.
            (multiple-value-bind (diff cost)
                (ast-diff-on-lists da db parent-a parent-b)
              (setf overall-diff (append diff overall-diff))
              (incf overall-cost cost))
            ;; Add the common segments.
            (setf overall-diff
                  (append (map 'list (lambda (it) (cons :same it)) ca)
                          overall-diff)))
      (values overall-diff overall-cost))))

(defmethod ast-diff* ((s1 string) (s2 string)
                      &aux (ignore-whitespace *ignore-whitespace*))
  "special diff method for strings"
  #+debug (format t "ast-diff[STRING]~%")
  (if *diff-strings-p*
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
            (ematch p
              (`((:code . ,(and string (type string))))
                string)))
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
  (ast-diff* (lines (genome-string soft))
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
            (follow (rest edit-script) (cons :d path))))))
    (follow edit-script nil)))

;;; Construct the edit tree from an ast and an edit script

(defgeneric create-edit-tree (source target script &key &allow-other-keys)
  (:documentation "Given a source that is the source of the edit
script SCRIPT, and a target object that is the result of the script on
the source, build the edit tree corresponding to the script."))

(defmethod create-edit-tree ((source ast) (target ast) (script cons)
                             &key &allow-other-keys)
  ;; The script is a list of actions on the AST's children
  (change-segments-on-seqs
   (standardized-children source)
   (standardized-children target)
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
   source target))

(defun change-segments-on-seqs (source target script segment-fn recurse-fn
                                source-node target-node)
  "Traverses two sequences and finds the change segments.
source -- AST being edited
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
        (changes 0)
        (segment-fn (ensure-function segment-fn))
        (recurse-fn (ensure-function recurse-fn)))
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
             (setf collected-recurse nil
                   collected-actions nil
                   changes 0
                   source-segment-start (incf source-position n)
                   target-segment-start (incf target-position n))))
      (iter (for action in script)
            (ecase (assure edit-action (car action))
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

(defun print-edit-tree (edit-tree &key print-asts coherence stream)
  (let ((*map-edit-tree-ancestors* nil))
    (map-edit-tree edit-tree
                   (lambda (node) (print-edit-tree-node
                                   node
                                   :print-asts print-asts
                                   :coherence coherence
                                   :stream stream)))))

(defgeneric print-edit-tree-node (node &key &allow-other-keys)
  (:documentation "Print fragment of an edit tree, properly indented"))

(defun coherence (node)
  (let ((c1 (ast-size (edit-tree-node-source node)))
        (c2 (ast-size (ast-size node))))
    (/ (float c2) (float c1))))

(defmethod print-edit-tree-node
    ((node edit-tree-node) &key print-asts coherence
                             stream)
  (assert (typep node 'edit-tree-node))
  ;; If COHERENCE is specified, print only the highest edit tree
  ;; nodes whose coherence is >= this limit
  (let ((node-coherence (coherence node))
        (parent (car *map-edit-tree-ancestors*))
        (*standard-output* (or stream *standard-output*)))
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

;;; The following control structures (`apply-values',
;;; `apply-values-meld', and their supporting funtions) are used to
;;; propagate multiple versions of an AST due to conflicts.

(defmacro apply-values (fn &rest arg-exprs)
  "Apply FN \(with `apply-values-fn') to the values returned by each arg-expr."
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
  "Use FN (with `apply-values-meld-fn') to combine the values returned
by FORM and the values returned by LIST-FORM.

FORM1 is a call (to `ast-patch*') that might return one value, if the
patch is successful OR if we are using conflict nodes, or two values,
if the patch fails and we are not using conflict nodes.

This will return either one or three values.

If there is no conflict, or the conflict is encapsulated, then we
return one value combining the results of FORM1 with the results of
LIST-FORM.

If there is a conflict, we return three values: the common list formed
by patching some tail of this list, and the partial lists of the
conflict versions."
  `(apply-values-meld-fn ,fn
                         (multiple-value-list ,form1)
                         (multiple-value-list ,list-form)))

(defun apply-values-meld-fn (fn vals list-vals)
  "Auxiliary function for `apply-value-meld'.

VALS must be a list of 1 or 2 elements; the length of LIST-VALS must
be in the interval [1,3].

If |VALS|=1, then we invoke FN on VALS and the appended LIST-VALS.

If |VALS|=2, then we return (1) the first val in LIST-VALS, (2) the
result of calling FN with the first val in VALS and the second val in
LIST-VALS, and (3) the result of calling FN with first val in VALS and
the third val in LIST-VALS."
  (let ((len1 (length vals))
        (len2 (length list-vals))
        (fn (ensure-function fn)))
    (assert (member len2 '(1 3)))
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
  "Cons the results of a merge on the results of a previous merge.
Given MELD, we may try to merge the previous versions."
  `(if ,meld?
       (apply-values-meld #'cons ,@args)
       (apply-values-extend #'cons ,@args)))

(defmacro append-values (meld? &rest args)
  "Prepend the results of a merge onto the results of a previous merge.
Given MELD, try to merge the previous versions."
  `(if ,meld?
       (apply-values-meld #'append ,@args)
       (apply-values-extend #'append ,@args)))

(defun ast-patch (original diff &rest keys &key &allow-other-keys)
  "Convert ORIGINAL into an AST, then apply `ast-patch*' to the AST,
DIFF, and KEYS."
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
(defun make-same-eop (e) (cons :same e))
(defpattern same-eop (e) `(cons :same ,e))

(deftype insert-eop () '(cons (eql :insert) t))
(defun insert-eop-p (e) (typep e 'insert-eop))
(defmacro insert-eop-new (e) `(cdr ,e))
(defun copy-insert-eop (e) (cons :insert (insert-eop-new e)))
(defun make-insert-eop (e) (cons :insert e))
(defpattern insert-eop (e) `(cons :insert ,e))

(deftype delete-eop () '(cons (eql :delete) t))
(defun delete-eop-p (e) (typep e 'delete-eop))
(defmacro delete-eop-old (e) `(cdr ,e))
(defun copy-delete-eop (e) (cons :delete (delete-eop-old e)))
(defun make-delete-eop (e) (cons :delete e))
(defpattern delete-eop (e) `(cons :delete ,e))

(deftype replace-eop () '(cons (eql :replace) (cons t (cons t null))))
(defun replace-eop-p (e) (typep e 'replace-eop))
(defmacro replace-eop-old (e) `(cadr ,e))
(defmacro replace-eop-new (e) `(caddr ,e))
(defmacro make-replace-eop (old new)
  (list :replace old new))
(defpattern replace-eop (old new) `(list :replace ,old ,new))

(deftype recurse-eop () '(cons (eql :recurse) t))
(defun recurse-eop-p (e) (typep e 'recurse-eop))
(defmacro recurse-eop-ops (e) `(cdr ,e))
(defun copy-recurse-eop (e) (cons :recurse (recurse-eop-ops e)))
(defun make-recurse-op (ops) (cons :recurse ops))
(defpattern recurse-eop (ops) `(cons :recurse ,ops))

(defstruct (:wrap (:type list) (:conc-name wrap-)
                  (:predicate wrap-p)
                  (:copier copy-wrap)
                  :named)
  sub path left right classes new)

(defpattern :wrap (sub path left right classes new)
  `(list :wrap ,sub ,path ,left ,right ,classes ,new))

(defstruct (:unwrap (:type list) (:conc-name unwrap-)
                    (:predicate unwrap-p)
                    :named)
  sub path left right)

(defpattern :unwrap (sub path left right)
  `(list :unwrap ,sub ,path ,left ,right))

(defstruct (:wrap-sequence (:type list) (:conc-name wrap-sequence-)
                           (:predicate wrap-sequence-p)
                           (:copier copy-wrap-sequence)
                           :named)
  length sub path left right classes)

(defpattern :wrap-sequence (length sub path left right classes)
  `(list :wrap-sequence ,length ,sub ,path ,left ,right ,classes))

(defstruct (:unwrap-sequence (:type list) (:conc-name unwrap-sequence-)
                             (:predicate unwrap-sequence-p)
                             (:copier copy-unwrap-sequence)
                             :named)
  sub path left right)

(defpattern :unwrap-sequence (sub path left right)
  `(list :unwrap-sequence ,sub ,path ,left ,right))


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
be applied in that context.

Note that this method is responsible for both applying a script to an
AST's children and for applying a step edit action from the script to
a single AST."))

(defmethod ast-patch* ((original null) (script null) &key &allow-other-keys)
  nil)

(defun actual-ast-patch (ast script &rest keys
                        &key delete? (meld? (ast-meld-p ast)) &allow-other-keys)
  "Patch AST using SCRIPT, potentially returning multiple versions (as
multiple values) in case of conflict."
  (declare (ignorable delete? meld?))
  (assert (typep ast 'ast))
  (assert (not (typep ast 'tree-sitter-ast)))
  (let* ((children (children ast))
         ;; For now, always meld
         ;; This may not give valid ASTs, but fix later
         (new-child-lists
          (multiple-value-list
           (apply #'ast-patch* children script :meld? meld? keys))))
    (apply #'values
           (iter (for new-children in new-child-lists)
                 (collect (copy ast :children new-children))))))

(defun actual-tree-sitter-ast-patch (ast script &rest keys
                                        &key delete? (meld? (ast-meld-p ast))
                                        &allow-other-keys)
  "Like `actual-ast-patch' for tree-sitter ASTs."
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
            (apply #'ast-patch* schildren script keys))))
    (apply #'values
           (iter (for new-schildren in new-schild-lists)
                 (collect (copy-with-standardized-children ast new-schildren))))))

(defmethod ast-patch* :around ((ast ast) (script list) &rest keys
                               &key &allow-other-keys)
  (if (listp (car script))
      (apply #'actual-ast-patch ast script keys)
      (call-next-method)))

(defmethod ast-patch* :around ((ast tree-sitter-ast) (script list) &rest keys
                               &key &allow-other-keys)
  (if (listp (car script))
      (apply #'actual-tree-sitter-ast-patch ast script keys)
      (call-next-method)))

(defmethod ast-patch* ((original ast) (script cons)
                       &rest keys &key &allow-other-keys)
  "Perform a single step of the SCRIPT on ORIGINAL."
  (case (car script)
    (:wrap
     (apply #'ast-patch-wrap original (cdr script) keys))
    (:unwrap
     (apply #'ast-patch-unwrap original (cdr script) keys))
    (t
     (apply #'actual-ast-patch original script keys))))

(defmethod ast-patch* ((original t) (script cons)
                       &rest keys &key (delete? t) &allow-other-keys)
  (declare (ignorable delete? keys))
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
         (t (error "Invalid diff on atom: ~a" script)))))))

(defun ast-patch-conflict-action (asts args)
  "Perform a patch of the action (:conflict . args) on ASTS.
Returns the conflict node and the list of remaining asts to
process with the rest of the script."
  ;; Special case: one conflict action is :SAME, the other is :RECURSE
  ;; In that case, we need to propagate the changes down the tree IFF there
  ;; is a nested conflict AST.
  (let ((special-case
         (match args
           (`(((:same . _) ((:recurse . ,cdaadr))) . _)
             (ast-patch-same-recurse (car asts) cdaadr
                                     :your))
           (`(((:recurse . ,cdaar)) ((:same . _)) . _)
             (ast-patch-same-recurse (car asts) cdaar
                                     :my)))))
    (if (or (typep special-case 'conflict-ast)
            (and (listp special-case) (some {typep _ 'conflict-ast} special-case)))
        ;; Special case
        (values special-case (cdr asts))
        ;; Base case
        (let ((consume nil)) ;; If set to true, consume an element of ASTS
          (flet ((%process (action)
                   "Process an inner action.  Returns a list of asts"
                   (when (car action)
                     (ematch action
                       ((cons :insert cdr)
                        (list cdr))
                       ((list* :replace _ caddr _)
                        (setf consume t)
                        (list caddr))
                       ((cons :delete _)
                        (setf consume t)
                        nil)
                       ((cons :same _)
                        (setf consume t)
                        (list (car asts)))
                       ((cons :recurse cdr)
                        (setf consume t)
                        ;; Don't process recursive conflicts
                        ;; In particular, this means :delete actions in
                        ;; the conflict branch do not cause recording
                        ;; of the original version in conflict nodes.
                        ;; This is arguably wrong, but for now we do it
                        ;; this way.
                        (list (ast-patch* (car asts) cdr
                                          :conflict nil)))))))
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
    (assert (length= path left-wrap right-wrap))
    (values-list
     (mapcar
      (lambda (a) (ast-wrap a left-wrap right-wrap classes base-ast))
      (multiple-value-list (apply #'ast-patch* ast sub-action keys))))))

(defmethod ast-patch-wrap ((ast list) (args list) &rest keys &key &allow-other-keys)
  (destructuring-bind (sub-action path left-wrap right-wrap classes base-ast)
      args
    (assert (length= path left-wrap right-wrap))
    (values-list
     (mapcar
      (lambda (a) (ast-wrap (list a) left-wrap right-wrap classes base-ast))
      (multiple-value-list (apply #'ast-patch* ast sub-action keys))))))

(defgeneric ast-patch-unwrap (ast args &key &allow-other-keys))

(defmethod ast-patch-unwrap ((ast ast) (args list) &rest keys
                             &key &allow-other-keys)
  (destructuring-bind (sub-action path left-wrap right-wrap)
      args
    (assert (length= path left-wrap right-wrap))
    (iter (while path)
          (setf ast (nth (pop path) (children ast))))
    (apply #'ast-patch* ast sub-action keys)))

(defgeneric ast-wrap (ast left-wrap right-wrap classes base-ast)
  (:documentation "Wrap AST with the first elements of LEFT-WRAP and
  RIGHT-WRAP, recursively.")
  (:method :before (ast left-wrap right-wrap classes base-ast)
    (assert (length= left-wrap right-wrap classes))
    (assert left-wrap)))

(defmethod ast-wrap ((ast ast) left-wrap right-wrap classes base-ast)
  (setf left-wrap (reverse left-wrap)
        right-wrap (reverse right-wrap))
  (iter (while left-wrap)
        (let ((class (pop classes)))
          (assert class)
          (setf ast
                (copy
                 ;; Use tree-copy to avoid possible interval
                 ;; collisions.
                 (tree-copy base-ast)
                 :serial-number nil
                 :class class
                 :children
                 (append (pop left-wrap)
                         (list ast)
                         (pop right-wrap))))))
  #+ast-diff-debug (format t "AST-WRAP returned:~%~s~%" (source-text ast))
  ast)

(defmethod ast-wrap ((ast structured-text) left-wrap right-wrap classes base-ast)
  (setf left-wrap (reverse left-wrap)
        right-wrap (reverse right-wrap))
  (iter (while left-wrap)
        (let ((class (pop classes)))
          (assert class)
          (setf ast
                (copy-with-standardized-children
                 ;; Use tree-copy to avoid possible interval
                 ;; collisions.
                 (tree-copy base-ast)
                 (let* ((children (standardized-children base-ast))
                        (start (position ast children :test #'equal?)))
                   (assert start)
                   (splice-seq children
                               :new
                               (append (pop left-wrap)
                                       (list ast)
                                       (pop right-wrap))
                               :start start
                               :end (1+ start)))
                 :serial-number nil
                 :class class))))
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

(defgeneric ast-wrap-sequence (ast left-wrap right-wrap classes base-ast)
  (:documentation "Like `ast-wrap', but wrap all children rather than just one."))

(defmethod ast-wrap-sequence ((ast ast) left-wrap right-wrap classes base-ast)
  (assert (length= left-wrap right-wrap classes))
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

(defmethod ast-wrap-sequence ((ast structured-text) left-wrap right-wrap classes base-ast)
  (assert (length= left-wrap right-wrap classes))
  (setf left-wrap (reverse left-wrap))
  (setf right-wrap (reverse right-wrap))
  (let ((asts (children ast)))
    (iter (while left-wrap)
          (let ((class (pop classes)))
            (assert class)
            (setf asts
                  (list
                   (copy-with-standardized-children
                    base-ast
                    ;; Insert the new children at the correct place in
                    ;; the list of standardized children.
                    (iter (iter:with children = (children base-ast))
                          (iter:with last-child = (lastcar children))
                          (for child in (standardized-children base-ast))
                          (when (eql child (first children))
                            (appending (pop left-wrap) into new-children)
                            (sum 1 into count))
                          (collect child into new-children)
                          (when (eql child last-child)
                            (appending (pop right-wrap) into new-children)
                            (sum 1 into count))
                          (finally
                           (assert (= count 2))
                           (return new-children)))
                    :class class)))))
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
           (apply #'ast-patch* new-ast sub-action keys))))))
  (:method ((ast structured-text) (args list) &rest keys &key &allow-other-keys)
    (destructuring-bind (sub-action path left-wrap right-wrap)
        args
      (iter (while path)
            (setf ast (nth (pop path) (children ast)))
            (pop left-wrap)
            (pop right-wrap))
      (let* ((c (children ast))
             (sc (standardized-children ast))
             (total-len (length c))
             (left-len (length (car left-wrap)))
             (right-len (length (car right-wrap)))
             (relevant-children
              (subseq c left-len (- total-len right-len))))
        (assert (>= total-len (+ left-len right-len)))
        (let* ((start-pos (position (first relevant-children) sc))
               (end-pos (position (lastcar relevant-children) sc))
               (new-children
                (splice-seq sc
                            :new (car left-wrap)
                            :start start-pos
                            :end start-pos))
               (new-children
                (splice-seq new-children
                            :new (car right-wrap)
                            :start end-pos
                            :end end-pos))
               (new-ast
                (copy-with-standardized-children
                 ast
                 new-children)))
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
  (check-type tag (member nil :my :old :your))
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
         ;; conflicts, otherwise if CONFLICT is false, return the conflict
         ;; versions.  If CONFLICT is true, return a single version
         ;; with conflict nodes.
         ;; When MELD is true, returns three values: the common list
         ;; formed by patching some tail of this list, and the partial
         ;; lists of the conflict versions (this will be three values).
         (when script
           (destructuring-bind (action . args) (car script)
             (ecase (assure edit-action action)
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
    (cond ((member (car script) '(:wrap :unwrap))
           (call-next-method))
          ;; Avoid the problem that, since each `:same' results in non-tail
          ;; recursion, comparing two large, identical files can overflow the
          ;; stack.
          ((every {starts-with :same} script)
           original)
          ;; cause various unmerged subsequences to be combined before
          ;; returning, if meld? is true
          ((listp (car script))
           (append-values meld? nil (edit original script)))
          (t (call-next-method)))))

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
            (let* ((action1 (caar script1))
                   (action2 (caar script2))
                   (val (list action1 action2)))
              (declare (type (member :same :delete :recurse :replace)
                             action1 action2))
              ;; actions are one of: :same, :delete, :recurse, :replace
              ;; Don't do :same-tail, :recurse-tail here
              (flet ((%check (s1 s2)
                       (assert (equal? s1 s2) () "MELD-SCRIPTS ~a: ~
                                 should have been the same: ~a, ~a" val s1 s2)))
                (dispatch-caseql ((action1
                                   (member :same :delete :recurse :replace))
                                  (action2
                                   (member :same :delete :recurse :replace)))
                  ((:same :same)
                   (%check (cdar script1) (cdar script2))
                   (collect (pop script1))
                   (pop script2))
                  ((:delete :delete)
                   (%check (cdar script1) (cdar script2))
                   (collect (pop script1))
                   (pop script2))
                  ((:delete :same)
                   (%check (cdar script1) (cdar script2))
                   (collect (pop script1))
                   (pop script2))
                  ((:recurse :same)
                   (collect (pop script1))
                   (pop script2))
                  ((:recurse :delete)
                   (collect (pop script1))
                   (pop script2))
                  ((:same :delete)
                   (%check (cdar script1) (cdar script2))
                   (pop script1)
                   (collect (pop script2)))
                  ((:same :recurse)
                   (pop script1)
                   (collect (pop script2)))
                  ((:delete :recurse)
                   (pop script1)
                   (collect (pop script2)))
                  ((:recurse :recurse)
                   ;; should not happen?
                   (pop script2)
                   (collect (pop script1)))
                  ((:replace :same)
                   (%check (cadar script1) (cdar script2))
                   (collect (pop script1))
                   (pop script2))
                  ((:replace :delete)
                   (%check (cadar script1) (cdar script2))
                   (collect (pop script1)))
                  ((:replace :replace)
                   (%check (cadar script1) (cadar script2))
                   (pop script2)
                   (collect (pop script1)))
                  ((:replace :recurse)
                   (pop script2)
                   (collect (pop script1)))
                  ((:same :replace)
                   (%check (cdar script1) (cadar script2))
                   (pop script1)
                   (collect (pop script2)))
                  ((:delete :replace)
                   (%check (cdar script1) (cadar script2))
                   (pop script1)
                   (collect (pop script2)))
                  ((:recurse :replace)
                   (pop script2)
                   (collect (pop script1)))))))
    (when (or script1 script2)
      (error
       "Could not meld scripts: different number of fixed location actions"))))

(defmethod ast-patch* :around ((original sequence) (script list)
                               &key delete? meld? conflict &allow-other-keys)
  (declare (ignorable delete?))
  (if (and (listp (car script))
           (find :conflict script :key #'car)
           (not (or meld? conflict)))
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
                (ast-patch* original script2)))
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
  ;; Specialize for different vector types (including strings).
  (with-vector-dispatch ((simple-array character (*))
                         (simple-array base-char (*)))
    original
    ;; Create a single result, with conflicts combined
    (let* ((len (length original))
           (etype (array-element-type original))
           (result (make-array (list len)
                               :element-type etype :adjustable t :fill-pointer 0))
           (i 0))
      (loop
        (unless script (return))
        (destructuring-bind (action . args) (pop script)
          (ecase (assure edit-action action)
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
      (copy-seq result))))

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
    (ast-class-meld? (ast-class ast) ast))
  (:method ((ast ts:root-ast))
    t))

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

(defun source-range->string-span (source string range &optional cache)
  (let* ((begin (range:begin range))
         (end (range:end range))
         (start (range:source-location->position string begin cache))
         (end (range:source-location->position string end cache))
         (length (- end start)))
    (vector source start length)))

(defun ast-range-table (root &aux (source (source-text root)))
  (let ((newlines (range:precompute-newline-offsets source)))
    (maphash-into
     (make-hash-table)
     (lambda (cons)
       (destructuring-bind (ast . range) cons
         (values ast
                 (cons (range:source-location->position source
                                                        (range:begin range)
                                                        newlines)
                       (range:source-location->position source
                                                        (range:end range)
                                                        newlines)))))
     (ast-source-ranges root))))

(-> range-table-starting-positions (hash-table)
    (values vector &optional))
(Defun range-table-starting-positions (table)
  (let (starting-positions)
    (do-hash-table (k v table)
      (declare (ignore k))
      (push (car v) starting-positions))
    (sort-new starting-positions #'<)))

(defun ast-concordance (diff root1 root2
                        &optional
                          (forward-map (make-hash-table))
                          (backward-map (make-hash-table)))
  (declare (optimize debug))            ;TODO
  (let ((children1 (standardized-children (genome root1)))
        (children2 (standardized-children (genome root2))))
    (flet ((add-to-concordance (ast1 ast2)
             (if (typep ast1 'slot-specifier)
                 (assert (typep ast2 'slot-specifier))
                 (progn
                   (when (typep (list ast1 ast2)
                                '(tuple tree-sitter-ast tree-sitter-ast)))
                   (setf (gethash ast1 forward-map) ast2
                         (gethash ast2 backward-map) ast1)))))
      (iter (while (or children1 children2))
            ;; If they get out of sync, there's a problem.
            (assert (and children1 children2))
            (while diff)
            (for operation = (pop diff))
            (ematch operation
              ((cons :same (type slot-specifier))
               (assure slot-specifier (pop children1))
               (assure slot-specifier (pop children2)))
              ((cons :same (and string (type string)))
               (assert (equal* string (pop children1) (pop children2))))
              ((cons :same ast)
               (assert (equal ast (car children1)))
               (add-to-concordance (car children1) (car children2))
               (pop children1)
               (pop children2))
              ((cons :insert ast)
               (assert (eql ast (car children2)))
               (pop children2))
              ((cons :delete ast)
               (assert (eql ast (car children1)))
               (pop children1))
              ((cons :recurse diff)
               (ematch* ((pop children1) (pop children2))
                 (((and ast1 (type ast))
                   (and ast2 (type ast)))
                  (add-to-concordance ast1 ast2)
                  (setf (values forward-map backward-map)
                        (ast-concordance diff
                                         ast1
                                         ast2
                                         forward-map
                                         backward-map)))
                 (((type (not ast))
                   (type (not ast))))))
              ((list :replace
                     (and ast1 (ast))
                     (and ast2 (ast)))
               (assert (equal ast1 (pop children1)))
               (assert (equal ast2 (pop children2)))))))
    (values forward-map backward-map)))

(defclass diff-printer ()
  ((script :initarg :script :type list)
   (my :initarg :my :type ast)
   (your :initarg :your :type ast)
   (my-text :initarg :my-text :type string)
   (your-text :initarg :your-text :type string)
   (concordance :initarg :concordance :type hash-table)
   (range-table :initarg :range-table :type hash-table)
   (starting-positions :initarg :starting-positions :type vector)
   (my-pos :initarg :my-pos :initform 0 :type array-index)
   (your-pos :initarg :your-pos :initform 0 :type array-index)
   (no-color :initarg :no-color :type boolean)
   (delete-start :initarg :delete-start :type string)
   (delete-end :initarg :delete-end :type string)
   (insert-start :initarg :insert-start :type string)
   (insert-end :initarg :insert-end :type string)
   (strings :initform (queue)))
  (:documentation "Encapsulation of the state and configuration
  required to print a diff."))

(defun make-diff-printer (script my your &rest kwargs &key &allow-other-keys)
  "Make a `diff-printer' instance from SCRIPT, MY, YOUR, and KWARGS."
  (let ((range-table
         (merge-tables
          (ast-range-table my)
          (ast-range-table your))))
    (apply #'make 'diff-printer
           :script script
           :my (astify my)
           :your (astify your)
           :my-text (source-text my)
           :your-text (source-text your)
           :concordance (ast-concordance script my your)
           :range-table range-table
           :starting-positions (range-table-starting-positions range-table)
           kwargs)))

(defgeneric save-intertext (diff v1 v2 &optional key)
  (:documentation "Record, in DIFF, the relationship between V1 and
  V2, if any, in DIFF, indexed by KEY for debugging purposes.

V1 and V2 are two strings that are known to be \"the same\", such that
any difference between them should be recorded as an insert, delete,
or insert-delete pair.")
  (:method ((diff diff-printer) (v1 string) (v2 string)
            &optional (key :unknown))
    (with-slots (insert-start insert-end delete-start delete-end strings) diff
      (cond
        ((equal v1 v2)
         (enq (cons key v1) strings))
        (t
         ;; TODO colorize.
         ;; NB Inserts before deletes.
         (unless (emptyp v2)
           (enq
            (cons key
                  (fmt "~a~a~a"
                       insert-start
                       v2
                       insert-end))
            strings))
         (unless (emptyp v1)
           (enq
            (cons key
                  (format nil
                          "~a~a~a"
                          delete-start
                          v1
                          delete-end))
            strings)))))))

(defun normalize-edit-for-print (edit)
  (flet ((normalize (edit)
           (match edit
             ((cons :delete (and char (type character)))
              (cons :delete (string char)))
             ((cons :insert (and char (type character)))
              (cons :insert (string char)))
             ((cons :same (and char (type character)))
              (cons :same (string char)))
             ((cons :same-sequence (and string (type string)))
              (cons :same string))
             ((cons :delete-sequence (and string (type string)))
              (cons :delete string))
             ((cons :insert-sequence (and string (type string)))
              (cons :insert string))
             ;; Treat inserting a string as replacing nothing
             ;; with something.
             ((cons :insert (and string (type string)))
              (list :replace "" string))
             ;; Treat deleting a string as replacing something
             ;; with nothing.
             ((cons :delete (and string (type string)))
              (list :replace string ""))
             ((list :replace
                    (and before (character))
                    (and after (character)))
              (list :replace
                    (string before)
                    (string after)))
             (otherwise edit))))
    (nlet fix ((edit edit))
      (let ((normalized (normalize edit)))
        (if (eql normalized edit) edit
            (fix normalized))))))

(defun get-range (diff ast)
  "Get the range of AST.
Unlike the return value of ast-source-ranges this range includes the
before and after ASTs."
  (declare (ast ast))
  (with-slots (range-table) diff
    (flet ((get-range (ast)
             (destructuring-bind (start . end) (@ range-table ast)
               (declare (array-index start end))
               (values start end))))
      (values (get-range
               (or (first (ts:before-asts ast))
                   ast))
              (nth-value 1
                         (get-range
                          (or (lastcar (ts:after-asts ast))
                              ast)))))))

(defun next-ast-start (diff pos)
  "Get the starting position of the next AST after POS.
Note that this does not include ASTs that start on POS."
  (flet ((next-ast-start (starting-positions pos)
           (let ((idx (bisect-right starting-positions pos #'<)))
             (aref starting-positions idx))))
    (with-slots (starting-positions) diff
      (next-ast-start starting-positions pos))))

(defgeneric print-diff-loop (diff script ast)
  (:documentation "Loop through SCRIPT, pointers in the AST and source
  text of both versions, using a precomputed \"concordance\" of AST
  that are the same between versions to isolate changed string
  segments for printing.")
  (:method ((diff diff-printer) (script list) (ast null))
    (error "This shouldn't happen."))
  (:method ((diff diff-printer) (script list) (string string))
    "Handle printing the diff of two strings."
    (declare #+debug-print-diff (optimize debug))
    (with-slots (my-pos your-pos
                 my-text your-text
                 strings
                 insert-start insert-end
                 delete-start delete-end)
        diff
      (dolist (edit script)
        (ematch (normalize-edit-for-print edit)
          ((cons :same (and string (type string)))
           (enq (cons :same-string string) strings)
           (incf my-pos (length string))
           (incf your-pos (length string)))
          ((list :replace "" (and y (type string)))
           (save-intertext diff "" y :insert))
          ((list :replace (and x (type string)) "")
           (save-intertext diff x "" :delete))
          ((list :replace (and x (type string)) (and y (type string)))
           (save-intertext diff x y :replace))))))
  (:method ((diff diff-printer) (script list) (ast ast))
    (declare #+debug-print-diff (optimize debug))
    (nest
     (with-slots (my your
                  my-text your-text
                  my-pos your-pos
                  concordance range-table strings
                  insert-start insert-end
                  delete-start delete-end)
         diff
       (assert (and my-pos your-pos)))
     (let ((children (standardized-children ast))))
     (macrolet ((subseq* (seq start &optional end)
                  "Make it easier to track down subseq bounds errors."
                  (if (and end (featurep :debug-print-diff))
                      (progn
                        (assert (every #'symbolp (list start end)))
                        `(progn
                           (assert (<= ,start ,end))
                           (subseq ,seq ,start ,end)))
                      `(subseq ,seq ,start ,@(and end (list end)))))))
     (flet ((get-range (ast) (get-range diff ast))
            (get-start (ast) (nth-value 0 (get-range diff ast)))
            (get-end (ast) (nth-value 1 (get-range diff ast))))
       (declare (inline get-range get-start get-end)
                (ignorable #'get-start #'get-end)))
     (loop (unless script
             (return))
           (let ((edit (pop script)))
             (nlet recurse ((edit edit))
               #+debug-print-diff
               (format t "~&BEFORE EDIT: ~s~%MY   | ~s~%~&YOUR | ~s~2%"
                       edit
                       (subseq* my-text my-pos)
                       (subseq* your-text your-pos))
               (ematch (normalize-edit-for-print edit)
                 ;; "Same" edits.

                 ;; TODO Disabled, but we're still going to need
                 ;; special handling of before-text and after-text
                 ;; because they can vary with indentation.
                 #+(or)
                 ((cons :same (slot-specifier
                               (slot-specifier-slot
                                (or (eql 'ts:before-text)
                                    (eql 'ts:after-text)))))
                  ;; Pop the value from the script.
                  (pop script)
                  ;; Pop the specifier and value from the children.
                  (assert (typep (pop children) 'slot-specifier))
                  (pop children))

                 ;; Skip all other slot specifiers.
                 ((cons :same (slot-specifier))
                  (assert (typep (pop children) 'slot-specifier)))
                 ;; ((cons :same (and string (type string)))
                 ;;  (assert (equal string (pop children)))
                 ;;  (enq (cons :same string) strings))
                 ((cons :same (and my-ast (ast)))
                  ;; Given an AST that is unchanged, use the
                  ;; concordance to find the same AST in the other
                  ;; tree, and collect edits accordingly.
                  (assert (eql my-ast (pop children)))
                  (mvlet* ((your-ast (@ concordance my-ast))
                           (my-start my-end (get-range my-ast))
                           (your-start your-end (get-range your-ast)))
                    ;; Record any changes before the AST.
                    (save-intertext
                     diff
                     (subseq* my-text my-pos my-start)
                     (subseq* your-text your-pos your-start)
                     :pre-same-ast)
                    ;; Record the text of the AST itself.
                    (enq (cons :same (subseq* my-text my-start my-end))
                         strings)
                    ;; Increment the pointers.
                    (setf my-pos my-end
                          your-pos your-end)))
                 ;; Print two strings that are the same.
                 ((cons (or :same :same-sequence)
                        (and string (type string)))
                  (assert (equal string (pop children)))
                  (enq (cons :same string) strings)
                  (incf my-pos (length string))
                  (incf your-pos (length string)))

                 ;; INSERTIONS.

                 ;; Skip slot specifiers.
                 ((cons :insert (slot-specifier)))
                 ;; Insert a new AST.
                 ((cons :insert (and new-ast (ast)))
                  (multiple-value-bind (start end) (get-range new-ast)
                    (enq (cons :insert insert-start) strings)
                    ;; Pick up structured text inserted before the AST.
                    (enq (cons :insert-pre (subseq* your-text your-pos start))
                         strings)
                    (enq (cons :insert (subseq* your-text start end))
                         strings)
                    (enq (cons :insert insert-end) strings)
                    (setf your-pos end)))

                 ;; DELETIONS.

                 ;; Skip slot specifiers.
                 ((cons :delete (slot-specifier))
                  (assert (typep (pop children) 'slot-specifier)))
                 ;; Delete an AST (without replacing it).
                 ((cons :delete (and old-ast (ast)))
                  (assert (eql old-ast (pop children)))
                  (multiple-value-bind (start end) (get-range old-ast)
                    (enq (cons :delete delete-start) strings)
                    ;; Catch structured text that was deleted along
                    ;; with the AST.
                    (enq (cons :delete-pre (subseq* my-text my-pos start))
                         strings)
                    (enq (cons :delete (subseq* my-text start end)) strings)
                    (enq (cons :delete delete-end) strings)
                    (setf my-pos end)))

                 ;; REPLACEMENTS

                 ;; Replace an AST with another AST.
                 ((list :replace
                        (and ast1 (ast))
                        (and ast2 (ast)))
                  (assert (eql ast1 (pop children)))
                  (mvlet ((start1 end1 (get-range ast1))
                          (start2 end2 (get-range ast2)))
                    (declare (array-index start1 end1 start2 end2))
                    ;; Pick up changes to the before text.
                    (save-intertext
                     diff
                     (subseq* my-text my-pos start1)
                     (subseq* your-text your-pos start2)
                     :pre-replace-asts)
                    ;; Record the changed AST.
                    (save-intertext
                     diff
                     (subseq* my-text start1 end1)
                     (subseq* your-text start2 end2)
                     :replace-ast)
                    (setf my-pos end1
                          your-pos end2)))
                 ;; Replace a string with another string.
                 ((list :replace
                        (and before (type string))
                        (and after (type string)))
                  (assert (string= before (pop children)))
                  (let ((before-start (search before my-text :start2 my-pos))
                        (after-start (search after your-text :start2 your-pos)))
                    (declare (array-index before-start after-start))
                    ;; Pick up changes to the before text.
                    (save-intertext diff
                                    (subseq* my-text my-pos before-start)
                                    (subseq* your-text your-pos after-start)
                                    :pre-replace-strings)
                    (setf my-pos before-start
                          your-pos after-start))
                  (save-intertext diff before after :replace)
                  (incf my-pos (length before))
                  (incf your-pos (length after)))

                 ;; RECURSION.

                 ;; Recurse on a single edit.
                 ((list :recurse edit)
                  (recurse edit))
                 ;; Recurse on a string.
                 ((cons :recurse script)
                  (unless (typep (car children) 'string)
                    (fail))
                  (let ((my-string (pop children)))
                    ;; TODO Find the start of the string by syncing
                    ;; it against the source text. Could this
                    ;; produce false positives?
                    (let* ((start1
                             (or (search my-string my-text :start2 my-pos)
                                 (error "Unable to sync on ~s" my-string)))
                           ;; Reconstruct the text of the second string.
                           (your-string (ast-patch my-string script))
                           (start2
                             (or (search your-string your-text
                                         :start2 your-pos)
                                 (error "Unable to sync on ~s (reconstructed)"
                                        your-string))))
                      (declare (array-index start1 start2))
                      ;; Pick up the before text.
                      (save-intertext
                       diff
                       (subseq* my-text my-pos start1)
                       (subseq* your-text your-pos start2)
                       :pre-string)
                      (setf my-pos start1
                            your-pos start2))
                    ;; Actually recurse into the string diff.
                    (print-diff-loop diff script my-string)))
                 ;; Recurse on an AST.
                 ((cons :recurse script)
                  (unless (typep (car children) 'ast)
                    (fail))
                  ;; This grabs the "before" and "after" text from
                  ;; the AST being recursed on.
                  (flet ((get-child-bounds (ast)
                           "Return four values: the start of
                           AST (inclusive of before-asts), the start
                           of AST's first child, the end of AST's last
                           child, and the end of AST (inclusive of
                           after-asts)."
                           (mvlet ((ast-start ast-end (get-range ast))
                                   (children
                                    (append (ts:before-asts ast)
                                            (children ast)
                                            (ts:after-asts ast))))
                             (values
                              ast-start
                              (if children
                                  (get-start (first children))
                                  ast-start)
                              (if children
                                  (get-end (lastcar children))
                                  ast-end)
                              ast-end))))
                    (mvlet*
                        ((ast1 (assure ast (pop children)))
                         (ast2 (assure ast (@ concordance ast1)))
                         (start1 first-child1-start last-child1-end end1
                          (get-child-bounds ast1))
                         (start2 first-child2-start last-child2-end end2
                          (get-child-bounds ast2))
                         ;; NB "pretext" is distinct from before-text
                         ;; in that it comes before the AST (it is
                         ;; the leading structured text).
                         (pretext1
                          pretext2
                          (values
                           (subseq* my-text my-pos start1)
                           (subseq* your-text your-pos start2)))p
                         (after-text1
                          after-text2
                          (values (subseq my-text last-child1-end end1)
                                  (subseq your-text last-child2-end end2))))
                      (declare (array-index
                                first-child1-start first-child2-start
                                last-child1-end last-child2-end)
                               (ignore first-child1-start first-child2-start))
                      ;; (assert (<= my-pos start1 first-child1-start last-child1-end))
                      ;; (assert (<= your-pos start2 first-child2-start last-child2-end))
                      ;; Record changes in the pretext.
                      (save-intertext diff pretext1 pretext2
                                      :recurse-pre-text)
                      ;; Record changes in the before text.
                      ;; (save-intertext diff before-text1 before-text2
                      ;;                 :recurse-before-text)
                      (incf my-pos (length pretext1))
                      (incf your-pos (length pretext2))
                      ;; Recurse on the children.
                      (print-diff-loop diff script ast1)
                      ;; Record changes in the after text. This is
                      ;; the only case where structured text inserted
                      ;; after a node is picked up.
                      (save-intertext diff after-text1 after-text2
                                      :recurse-after-text)
                      (setf my-pos end1
                            your-pos end2)))))
               #+debug-print-diff
               (format t "~&AFTER EDIT: ~s~%MY   | ~s~%YOUR | ~s~2%"
                       edit
                       (subseq my-text my-pos)
                       (subseq your-text your-pos))))))))

(defgeneric diff-printer-print (diff stream)
  (:documentation "Print DIFF (a `diff-printer' instance) to STREAM.")
  (:method ((diff diff-printer) (stream stream))
    (with-slots (my script strings) diff
      (print-diff-loop diff script my)
      (dolist (string (qlist strings))
        (write-string
         (if (listp string)
             (cdr string)
             string)
         stream)))))

(defgeneric print-diff
    (diff my your &key .
                    #1=(stream
                        no-color
                        delete-start
                        delete-end
                        insert-start
                        insert-end
                        sort-insert-delete))
  (:documentation
   "Return a string form of DIFF suitable for printing at the command line.
Numerous options are provided to control presentation.")
  (:method ((diff list) (my software) (your software)
            &rest kwargs
            &key . #1#)
    (declare (ignore . #1#))
    (apply #'print-diff diff (genome my) (genome your) kwargs))
  (:method
      ((diff list) (my ast) (your ast)
       &key
         (stream *standard-output*)
         (no-color nil)
         (delete-start (if no-color "[-" (format nil "~a[-" +color-RED+)))
         (delete-end (if no-color "-]" (format nil "-]~a" +color-RST+)))
         (insert-start (if no-color "{+" (format nil "~a{+" +color-GRN+)))
         (insert-end (if no-color "+}" (format nil "+}~a" +color-RST+)))
         (sort-insert-delete t))
    "Default method for ASTs that do not inherit from structured text."
    (nest
     (let ((*print-escape* nil)
           ;; These are used to track what color we should be printing
           ;; in.
           (*deletep* nil)
           (*insertp* nil)
           (insert-buffer nil)
           (delete-buffer nil))
       (declare (special *insertp* *deletep*)))
     (with-string (stream stream))
     (labels ((%p (c) "Print."
                (write-string
                 (typecase c
                   (null "()")
                   (structured-text
                    (continue-color
                     (string+
                      (ts:before-text c)
                      (source-text c)
                      (ts:after-text c))))
                   (slot-specifier "")
                   (t (continue-color (source-text c))))
                 stream))
              (continue-color (text)
                (cond
                  (no-color text)
                  (*deletep*
                   (string-replace-all (string #\Newline) text
                                       (format nil "~%~a" +color-RED+)))
                  (*insertp*
                   (string-replace-all (string #\Newline) text
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
                  (unless insert-buffer
                    (write insert-start :stream stream))
                  (push c insert-buffer)))
              (push-inserts (l) (mapc #'push-insert l))
              (push-delete (c)
                (unless (equal c "")
                  (purge-insert)
                  (unless delete-buffer
                    (write delete-start :stream stream))
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
  (:method ((diff list)
            (my structured-text)
            (your structured-text)
            &key
              (stream *standard-output*)
              (no-color nil)
              (delete-start (if no-color "[-" (format nil "~a[-" +color-RED+)))
              (delete-end (if no-color "-]" (format nil "-]~a" +color-RST+)))
              (insert-start (if no-color "{+" (format nil "~a{+" +color-GRN+)))
              (insert-end (if no-color "+}" (format nil "+}~a" +color-RST+)))
              sort-insert-delete)
    "Print a diff between structured text ASTs."
    (declare (ignore sort-insert-delete))
    (let* ((diff (make-diff-printer
                  diff
                  my your
                  :no-color no-color
                  :delete-start delete-start
                  :delete-end delete-end
                  :insert-start insert-start
                  :insert-end insert-end)))
      (with-string (stream stream)
        (diff-printer-print diff stream)))))

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
                  (push `(:delete . ,(cadr d)) saved-deletes))
                 (t
                  (%pop))))))
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
  (ematch o
    (`((:same-sequence . ,(type list)) ,_)
      o)
    (`((:same-sequence . ,seq) . ,cdr)
      (cons (cons :same-sequence (map 'list #'identity seq))
            cdr))))

(defun merge-diffs2  (orig-a orig-b &aux (o-a orig-a) (o-b orig-b))
  "Derived from CHUNK, but a bit smarter.

Produces an actual diff and not a list of chunks.

The last call to merge-diffs2-syms may return an improper list. Handle
it specially (appending cannot be used even if it is the last thing)."
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
     (ematch orig-a
       (`(:alist . _)
         (let ((diff (merge-diffs2 (list orig-a) (list orig-b))))
           ;; (format t "~A~%" diff)
           diff)))))) ; (car diff)

(defun merge-diffs2-syms (o-a o-b)
  (merge-diffs-on-syms (caar o-a) (caar o-b) o-a o-b))

;;; Another algorithm for good common subsequences, more robust in the
;;; face of elements that occur with high frequency.  Instead, focus
;;; on elements that occur just once in each list, and grow
;;; subsequences from those.

(defstruct good-common-subsequences
  (count 0 :type fixnum)
  (positions-1 nil :type list)
  (positions-2 nil :type list))

(-> good-common-subsequences2 ((simple-array fixnum (*))
                               (simple-array fixnum (*))
                               &key (:test function))
    vector)
(defun good-common-subsequences2 (v1 v2 &key (test #'eql))
  (with-two-arg-test (test)
    (let* ((table (make-hash-table :test test))
           (l1 (length v1))
           (l2 (length v2)))
      (macrolet ((init-table (v place)
                   `(iter (for x in-vector ,v)
                          (for i from 0)
                          (let ((g (gethash x table)))
                            (unless g
                              (setf (gethash x table)
                                    (setf g (make-good-common-subsequences))))
                            (incf (good-common-subsequences-count g))
                            (push i (,place g))))))
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
      (let ((candidates (vect))
            (i 0))
        (iter (while (< i l1))
              #+gcs2-debug (format t "i = ~A~%" i)
              (let* ((x (aref v1 i))
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
                              (while (test
                                      (aref v1 (1- start1))
                                      (aref v2 (1- start2))))
                              (decf start1)
                              (decf start2))
                        (iter (while (< end1 l1))
                              (while (< end2 l2))
                              (while (test (aref v1 end1)
                                           (aref v2 end2)))
                              (incf end1)
                              (incf end2))
                        ;; At this point, the subsequences of v1 and v2
                        ;; from start1 to end1-1 and start2 to end2-1 are
                        ;; maximal contiguous subsequences containing
                        ;; v1[i] and v2[j]. Record them.
                        (vector-push-extend (list start1 start2 (- end1 start1))
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
          (iter (for triple in-vector candidates)
                (for (s21 s22 l2) = triple)
                (when
                    ;; Reject triples when they break ordering with
                    ;; previous triples. The triples should never
                    ;; overlap.
                    (iter (for (s11 s12 l1) in selected-triples)
                          (assert (/= s11 s21))
                          (always (if (< s21 s11)
                                      (and (<= (+ s21 l2) s11)
                                           (<= (+ s22 l2) s12))
                                      (and (>= s21 (+ s11 l1))
                                           (>= s22 (+ s12 l1))))))
                  (push triple selected-triples)))
          (sort-new selected-triples #'< :key #'car))))))


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

(let ((text-slot-specifier
        (make-instance 'slot-specifier :class t :slot 'text :arity 1)))
  (defmethod standardized-children :around ((ast computed-text))
    ;; Add information in for the 'text slot.
    (let ((standardized-children (call-next-method)))
      (if (children ast)
          standardized-children
          (let ((insert-point
                  (member-if (lambda (child)
                               (match child
                                 ((slot-specifier :slot (eql 'children))
                                  child)))
                             standardized-children)))
            (if (no insert-point)
                `(,text-slot-specifier
                  ,(text ast)
                  ,@standardized-children)
                `(,@(ldiff standardized-children insert-point)
                  ,text-slot-specifier ,(text ast)
                  ,@insert-point)))))))

(let ((before-asts-slot-specifier
        (make-instance 'slot-specifier
                       :class t :slot 'ts::before-asts :arity 0))
      (before-text-slot-specifier
       (make-instance 'slot-specifier
                      :class t :slot 'ts::before-text :arity 1))
      (after-text-slot-specifier
        (make-instance 'slot-specifier
                       :class t :slot 'ts::after-text :arity 1))
      (after-asts-slot-specifier
       (make-instance 'slot-specifier
                      :class t :slot 'ts::after-asts :arity 0)))
  (defmethod standardized-children :around ((ast structured-text))
    ;; Add information in for the 'text slot.
    `(,before-asts-slot-specifier
      ,@(ts::before-asts ast)
      ,before-text-slot-specifier
      ,(ts::before-text ast)
      ,@(call-next-method)
      ,after-text-slot-specifier
      ,(ts::after-text ast)
      ,after-asts-slot-specifier
      ,@(ts::after-asts ast))))

(defmethod standardized-children ((ast tree-sitter-ast))
  (check-child-lists ast)
  (labels ((ordered-calist ()
             "Return an `alist' of (slot-spec . children) where the same
            slot-spec may occur more than once."
             (mapcar
              (lambda (ordering)
                `(,(caar ordering)
                  ,@(mappend #'cdr ordering)))
              (assort (children-slot-specifier-alist ast) :key #'car))))
    (flatten
     (remove '(ts::before-asts ts::after-asts)
             (ordered-calist)
             :key [#'slot-specifier-slot #'car]
             :test (flip #'memq)))))

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

(defmethod copy-with-standardized-children :around ((ast computed-text) (children list) &rest args)
  (declare (ignorable args))
  (labels ((text-slot-p (slot-specifier)
             "Return T if SLOT-SPECIFIER represents a 'text slot."
             (and (typep slot-specifier 'slot-specifier)
                  (eql 'text (slot-specifier-slot slot-specifier))))
           (set-text-slot (copy)
             "Set the text slot of COPY if a value is provided in CHILDREN."
             (prog1 copy
               (when-let* ((text-slot-position
                            (position-if #'text-slot-p children))
                           (text (nth (1+ text-slot-position) children)))
                 (setf (text copy) text)))))
    (set-text-slot (call-next-method))))

(defmethod copy-with-standardized-children ((ast tree-sitter-ast) (children list) &rest args)
  (declare (special *a* *c*))
  ;; Remember state; used for debugging on failure
  (setf *a* ast)
  (setf *c* children)
  (labels ((add-every-slot (child-alist)
             "Return a child alist which contains every slot even
              if the slot doesn't have any children."
             (map nil
                  (lambda (slot-specifier)
                    (unless (assoc slot-specifier child-alist
                                   :test (op (eql (slot-specifier-slot _)
                                                  (slot-specifier-slot _))))
                      (push (list slot-specifier) child-alist)))
                  (child-slot-specifiers ast))
             child-alist)
           (copy-ast (child-alist)
             "Create a copy of ast with values based on CHILD-ALIST."
             (lret ((new (multiple-value-call #'copy-with-children-alist
                           ast
                           child-alist
                           :stored-hash nil
                           (values-list args))))
               (check-child-lists new)))
           (get-amended-children-alist (ast)
             "Return a version of the child-alist of AST with surrounding text
              slots added and a text slot if the AST is a computed text node."
             `(,@(when (typep ast 'computed-text)
                   `((text ,(text ast))))
               (ts::before-asts ,(ts::before-asts ast))
               (ts::before-text ,(ts::before-text ast))
               (ts::after-text ,(ts::after-text ast))
               (ts::after-asts ,(ts::after-asts ast))
               ,@(children-alist ast)))
           (child-alist-equal? (child-alist)
             "Return T if CHILD-ALIST is #'equal? to children alist of the
              AST being copied."
             (not
              (occurs
               nil
               (mapcar (op (mapcar #'equal? _ _))
                       (mapcar (lambda (p)
                                 (cons (slot-specifier-slot (car p))
                                       (cdr p)))
                               child-alist)
                       (get-amended-children-alist ast)))))
           ;; TODO: find a better name for this function.
           (maybe-get-copy (child-alist)
             "Create a copy of AST if it would differ from the values
              already in AST. Otherwise, return AST."
             (if (and (child-alist-equal? child-alist) (null args))
                 ast
                 (copy-ast (add-every-slot child-alist)))))
    (if (or (some #'is-conflict-node-with-slot-specifier children)
            (has-conflict-node-before-slot-specifiers children))
        ;; Conflict that prevents creation of a node here
        ;; Instead, combine any conflict nodes and move up to
        ;; the parent
        (combine-all-conflict-asts ast children)
        (maybe-get-copy (unstandardize-children children)))))

(defmethod combine-all-conflict-asts ((parent tree-sitter-ast) (child-list list))
  (multiple-value-bind (alist def)
      (combine-conflict-asts-in-list child-list)
    (make-instance
     'conflict-ast
     :child-alist (iter (for (k . children) in alist)
                        (collecting (list k (copy-with-standardized-children parent children))))
     :default-children (list (copy-with-standardized-children parent def)))))

(defun unstandardize-children (children)
  " Return the new values of the child slots, grouped as an
alist of (slot . children), ordered by the first textual appearance of
each slot. ."
  ;; Note that this fails if there's a non-string child that occurs
  ;; before any slot-specifier.  This may happen, for example,
  ;; if a conflict node is generated that "swallows" slot-specifiers.
  ;; For that reason, conflict nodes need to be moved up to the parent
  ;; level when that happens.
  (let ((child-alist nil)
        (current-slot nil)
        (slot-pair nil)
        ;; Capture the current order of the first occurrence of each
        ;; of the slot specifiers.
        (ordering (ordering (filter (of-type 'slot-specifier) children)))
        (order nil))
    (loop
      (unless children (return))
      (let ((c (pop children)))
        (typecase c
          (slot-specifier
           (setf current-slot c)
           ;; TODO Is it actually possible for a specifier to occur
           ;; more than once?
           (if-let (pair (assoc c child-alist))
             (setf slot-pair pair)
             (progn
               (setf slot-pair (list c))
               (push slot-pair child-alist))))
          (otherwise
           (unless current-slot
             (error "Child ~a occurs before any slot-specifier" c))
           (push c (cdr slot-pair))
           ;; The extra layer here (a cons in a list) is to mimic
           ;; the format of `ft:position'.
           (let ((slot (slot-specifier-slot current-slot)))
             (push (list (cons slot (1- (length (cdr slot-pair)))))
                   order))))))
    (dolist (p child-alist)
      (setf (cdr p) (nreverse (cdr p))))
    (values (sort child-alist ordering :key #'car)
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
  (:method ((ast tree-sitter-ast) (indent integer))
    (format t "~v@{ ~}~a~%" indent (ast-class ast))
    (format t "~v@{ ~}~s~%" indent (interleaved-text ast))
    (dolist (ss (child-slot-specifiers ast))
      (let ((s (slot-specifier-slot ss))
            (n2 (+ indent 2)))
        (format t "~v@{ ~}~a:~%" indent s)
        (if (eql (ft::slot-specifier-arity ss) 1)
            (dump-ast* (slot-value ast s) n2)
            (mapc {dump-ast* _ n2} (slot-value ast s)))))))

;;; ast-diff.lisp --- diffs between ASTs and other tree structures
;;;
;;; The @code{software-evolution-library/ast-diff} library provides
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
   :common-lisp
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/ast
   :software-evolution-library/software/simple
   :resolve/string
   :alexandria
   :named-readtables
   :curry-compose-reader-macros
   :metabang-bind
   :iterate
   :cl-heap)
  (:shadowing-import-from :software-evolution-library/view
                          +color-RED+ +color-GRN+ +color-RST+)
  (:export
   :ast-equal-p
   :ast-cost
   :ast-can-recurse
   :ast-on-recurse
   :ast-un-recurse
   :ast-diff
   :ast-diff-elide-same
   :ast-patch
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
   :ast-to-list-form))
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

(defun clength (x) (iter (while (consp x)) (pop x) (summing 1)))

;; (defun make-costed (&key obj &allow-other-keys) obj)

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

(defmethod ccost (x)
  (declare (ignorable x))
  1)

;; #+sbcl (declaim (optimize sb-cover:store-coverage-data))

(defmethod clast (obj) (iter (while (consp obj)) (pop obj)))


;;; Interface functions.
(defgeneric ast-cost (ast)
  (:documentation "Return cost of AST."))

(defmethod ast-cost ((ast ast))
  (reduce #'+ (ast-children ast) :initial-value 0 :key #'ast-cost))

(defmethod ast-cost (ast) 1)

(defmethod ast-cost ((ast string))
  (length ast))

(defmethod ast-cost ((ast vector))
  (length ast))

(defmethod ast-cost ((ast null))
  1)

(defmethod ast-cost ((ast cons))
  (+ (iter (sum (ast-cost (pop ast)))
           (while (consp ast)))
     ;; cost of terminal NIL is 0
     (if ast (ast-cost ast) 0)))

(defgeneric ast-can-recurse (ast-a ast-b &optional strings)
  (:documentation "Check if recursion is possible on AST-A and AST-B.  Strings
can be recursed on if STRINGS is true (defaults to true)"))

(defmethod ast-can-recurse ((ast-a cons) (ast-b cons) &optional strings)
  (declare (ignorable ast-a ast-b strings))
  t)
(defmethod ast-can-recurse ((ast-a string) (ast-b string) &optional (strings t))
  (declare (ignorable ast-a ast-b))
  strings)
(defmethod ast-can-recurse (ast-a ast-b &optional strings)
  (declare (ignorable ast-a ast-b strings))
  nil)
(defmethod ast-can-recurse ((ast-a ast) (ast-b ast) &optional strings)
  (declare (ignore strings))
  (eq (ast-class ast-a) (ast-class ast-b)))

(defgeneric ast-on-recurse (ast)
  (:documentation "Possibly AST on recursion."))
(defmethod ast-on-recurse ((ast ast)) (ast-children ast))
(defmethod ast-on-recurse ((ast t)) ast)

(defgeneric ast-un-recurse (ast sub-ast)
  (:documentation
   "Reverse the effect of `ast-on-recurse' recombining SUB-AST into AST."))
(defmethod ast-un-recurse ((ast t) (sub-ast t))
  sub-ast)
(defmethod ast-un-recurse ((ast ast) sub-asts)
  (copy ast :children sub-asts))

(defmethod ast-equal-op ((s1 simple) (s2 simple))
  "Useful to treat simple objects as ASTs when calculating differences."
  (ast-equal-p (genome s1) (genome s2)))

(defmethod ast-text ((ast string))
  ast)

(defmethod ast-text ((ast cons))
  (let ((strings (iter (while (consp ast))
                       (collecting (ast-text (pop ast))))))
    (if ast
        (concatenate-strings (list strings "." (ast-text ast)))
        (concatenate-strings strings))))

(defun concatenate-strings (strings)
  (let* ((total-length (iter (for s in strings) (summing (length s))))
         (result (make-string total-length :initial-element #\Space))
         (i 0))
    (iter (for s in strings)
          (let ((l (length s)))
            (setf (subseq result i (+ i l)) s)
            (incf i l)))
    result))

(defgeneric ast-to-list-form (ast)
  (:documentation "Convert ast into a more readable list form"))

(defun actual-ast-to-list-form (ast)
  (cons (ast-class ast)
        (mapcar #'ast-to-list-form (ast-children ast))))

(defmethod ast-to-list-form ((ast ast))
  (actual-ast-to-list-form ast))

(defmethod ast-to-list-form (ast) ast)

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

(defmethod ast-text ((segment edit-segment))
  (let ((start (edit-segment-start segment))
        (len (edit-segment-length segment))
        (ast-node (edit-segment-node segment)))
    (apply #'concatenate 'string
           (mapcar #'ast-text
                   (subseq (ast-children ast-node)
                           start (+ start len))))))

(defmethod ast-text ((segment string-edit-segment))
  (with-slots (node start string-start string-length)
      segment
    (assert node)
    (subseq (elt (ast-children node) start)
            string-start (+ string-start string-length))))

(defmethod ast-to-list-form ((segment string-edit-segment))
  (ast-text segment))

(defmethod ast-to-list-form ((segment edit-segment))
  (let ((start (edit-segment-start segment))
        (len (edit-segment-length segment))
        (ast-node (edit-segment-node segment)))
    `(,(ast-class ast-node)
       ,@(unless (eql start 0) '(|...|))
       ,@(mapcar #'ast-to-list-form
                 (subseq (ast-children ast-node)
                         start (+ start len)))
       ,@(unless (eql (+ start len)
                      (length (ast-children ast-node)))
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
  (let ((s (ast-text (edit-tree-node-source node)))
        (bound 30))
    (when (> (length s) bound)
      (setf s (concatenate 'string (subseq s 0 (- bound 3)) "...")))
    (format stream "#<EDIT-TREE-NODE ~s>" s)))

(defgeneric ast-size (node)
  (:documentation "Number of nodes and leaves in an AST or
ast-like thing"))

(defmethod ast-size ((node ast))
  (reduce #'+ (ast-children node) :key #'ast-size :initial-value 1))
(defmethod ast-size (node) 1)

(defmethod slot-unbound (class (node edit-tree-node) (slot (eql 'size)))
  (let ((value (reduce #'+ (edit-tree-node-children node)
                       :key #'ast-size :initial-value 1)))
    (setf (slot-value node slot) value)
    value))

(defmethod ast-size ((segment string-edit-segment)) 1)

;; Cache for SIZE slot, accessed by ast-size
(defmethod slot-unbound (c (segment edit-segment) (slot (eql 'size)))
  (let* ((node (edit-segment-node segment))
         (length (edit-segment-length segment))
         (start (edit-segment-start segment))
         (children (subseq (ast-children node) start (+ start length)))
         (value (reduce #'+ children :key #'ast-size :initial-value 1)))
    (setf (slot-value segment slot) value)))


;;; Main interface to calculating ast differences.
(defgeneric ast-diff (ast-a ast-b &key &allow-other-keys)
  (:documentation
   "Return a least-cost edit script which transforms AST-A into AST-B.
Also return a second value indicating the cost of the edit.

See `ast-patch' for more details on edit scripts.

The following generic functions may be specialized to configure
differencing of specialized AST structures.; `ast-equal-p',
`ast-cost', `ast-can-recurse', and `ast-on-recurse'."))

(defmethod ast-diff ((ast-a ast) (ast-b ast) &rest args &key (strings t) &allow-other-keys)
  #+debug (format t "ast-diff[AST] AST-CAN-RECURSE: ~S~%" (ast-can-recurse ast-a ast-b))
  (if (ast-can-recurse ast-a ast-b strings)
      (apply #'ast-diff (ast-children ast-a) (ast-children ast-b) args)
      (call-next-method)))

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
                   (if (equalp a b)
                       (collect a into common)
                       (return common))
                   (finally (return common)))))
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

(defun recursive-diff (total-a total-b &rest args
                       &key (upper-bound most-positive-fixnum)
                         (strings t)
                       &allow-other-keys
                       &aux
                         (from (make-cache total-a total-b))
                         ;; FRINGE is a queue used to order
                         ;; visits of 'open' nodes.  An open node should only
                         ;; be put on the queue when all its
                         ;; predecessors are closed.
                         ;; (fringe (make-instance 'priority-queue))
                         (fringe (make-simple-queue))
                         ;; When T, the node is stored in the priority
                         ;; queue already
                         (open (make-cache total-a total-b))
                         (total-open 0)
                         ;; When CLOSED is T, the node has been processed
                         (closed (make-cache total-a total-b))
                         ;; For closed nodes, G is the actual minimum cost
                         ;; of reaching the node.
                         (g (make-cache total-a total-b))
                         (r-cache (make-cache total-a total-b))
                         (lta (clength total-a))
                         (ltb (clength total-b)))
  ;; UPPER-BOUND is a limit beyond which we give up on
  ;; pursuing edges.  This is not currently exploited.
  (labels
      ((%enqueue (node)
         ;; (enqueue fringe node cost)
         (simple-queue-enqueue fringe node)
         )
       (%dequeue ()
         ;; (dequeue fringe)
         (simple-queue-dequeue fringe))
       (reconstruct-path- (a b)
         #+ast-diff-debug (format t "reconstruct-path-: ~a ~a~%" a b)
         (let ((result
                (if (and (zerop a) (zerop b))
                    nil
                    (destructuring-bind ((new-a . new-b) . edge) (aref from a b)
                      (cons edge (reconstruct-path- new-a new-b))))))
           #+ast-diff-debug (format t "reconstruct-path- returns: ~a~%" result)
           result))
       (reconstruct-path (last-a last-b a b)
         #+ast-diff-debug
         (format t "reconstruct-path: ~a ~a ~a ~a~%" last-a last-b a b)
         (reverse
          ;; Handle cdr of final cons.  This must be special-cased
          ;; because when nil (a list) this is ignored by functions
          ;; expecting lists (not cons trees).
          ;;
          ;; Get rid of all of this!
          (if (ast-equal-p last-a last-b)
              (if last-a
                  `((:same-tail . ,last-a) . ,(reconstruct-path- a b))
                  (reconstruct-path- a b))
              `((:insert . ,last-b)
                (:delete . ,last-a)
                . ,(reconstruct-path- a b)))))
       (%pos-a (a) (- lta (clength a)))
       (%pos-b (b) (- ltb (clength b))))

    (setf (aref g 0 0) 0 ;; initial node reachable at zero cost
          (aref open 0 0) t
          total-open (1+ total-open))

    (%enqueue (cons total-a total-b))

    (do ((current (%dequeue) (%dequeue)))
        ((zerop total-open)
         (reconstruct-path (clast total-a) (clast total-b) lta ltb))

      (let* ((a (car current)) (b (cdr current))
             (pos-a (%pos-a a))
             (pos-b (%pos-b b)))
        #+ast-diff-debug (format t "pos-a = ~a, pos-b = ~a~%" pos-a pos-b)
        (when (and (zerop (clength a))
                   (zerop (clength b)))
          (reconstruct-path a b (clength a) (clength b)))

        (when (aref open pos-a pos-b) (decf total-open))
        (setf (aref open pos-a pos-b) nil
              (aref closed pos-a pos-b) t)

        (labels                         ; Handle all neighbors.
            ((add (neighbor edge)
               #+ast-diff-debug (format t "   add: ~a ~a~%" neighbor edge)
               (let ((next-a (%pos-a (car neighbor)))
                     (next-b (%pos-b (cdr neighbor))))
                 (unless (aref closed next-a next-b) ; should never happen?
                   (unless (aref open next-a next-b)
                     (incf total-open)
                     (setf (aref open next-a next-b) t))
                   (let ((tentative
                          (+ (aref g pos-a pos-b)
                             (diff-cost edge)))
                         (value (aref g next-a next-b)))
                     ;; Neighbor is an improvement.
                     (when (and (or (null value) (< tentative value))
                                (< tentative upper-bound))
                       #+ast-diff-debug
                       (format t "Improvement: tentative = ~a, value = ~a, ~
                                               next-a = ~a, next-b = ~a~%"
                               tentative value next-a next-b)
                       (setf (aref from next-a next-b)
                             (cons (cons pos-a pos-b) edge)
                             value tentative
                             (aref g next-a next-b) tentative))
                     ;; Only enqueue if ALL predecessors are closed
                     (when (and value
                                (if (= next-a 0)
                                    (aref closed next-a (1- next-b))
                                    (and (aref closed (1- next-a) next-b)
                                         (or (= next-b 0)
                                             (and (aref closed (1- next-a)
                                                        (1- next-b))
                                                  (aref closed next-a
                                                        (1- next-b)))))))
                       (%enqueue neighbor))))))
             (%recursive (a b)
               #+ast-diff-debug (format t "%recursive: ~a ~a~%" a b)
               (let ((i (%pos-a a))
                     (j (%pos-b b)))
                 (or (aref r-cache i j)
                     (setf (aref r-cache i j)
                           (apply #'ast-diff (car a) (car b) args))))))

          ;; Check neighbors: diagonal, recurse, insert, delete.
          (when (and (consp a) (consp b))
            #+ast-diff-debug (format t "check neighbors case 1: ~a ~a~%" a b)
            (cond
              ((ast-equal-p (car a) (car b)) ; Diagonal.
               #+ast-diff-debug (format t "  diagonal~%")
               (add (cons (cdr a) (cdr b))
                    (cons :same (car a))))
              ((ast-can-recurse (car a) (car b) strings) ; Recurse.
               #+ast-diff-debug (format t "  recurse~%")
               (add (cons (cdr a) (cdr b))
                    (cons  :recurse
                           (%recursive a b))))))
          (if (consp b)                 ; Insert.
              (add (cons a (cdr b))
                   (cons :insert (car b)))
              (add (cons a nil)
                   (cons :insert b)))
          (if (consp a)                 ; Delete.
              (add (cons (cdr a) b)
                   (cons :delete (car a)))
              (add (cons nil b)
                   (cons :delete a))))))))

(defun diff-cost (diff)
  "Computes the cost of a diff"
  (cond
    ((not (consp diff)) 0)
    ((symbolp (car diff))
     (case (car diff)
       (:insert (ast-cost (cdr diff)))
       (:delete (if (cdr diff) (ast-cost (cdr diff)) 1))
       (:recurse (diff-cost (cdr diff)))
       (:recurse-tail (diff-cost (cdr diff)))
       ((:insert-sequence :delete-sequence)
        (if (consp (cdr diff))
            (reduce #'+ (cdr diff) :key #'diff-cost :initial-value 0)
            1))
       ((:same :same-tail :same-sequence) 0)
       (t (diff-cost-car diff (car diff)))))
    (t
     (reduce #'+ diff :key #'diff-cost :initial-value 0))))

(defgeneric diff-cost-car (diff diff-car)
  (:documentation
   "Cost of DIFF, where DIFF is a cons cells with car DIFF-CAR, a symbol."))

(defmethod diff-cost-car ((diff cons) (diff-car symbol)) 0)

(defun ast-diff-on-lists (ast-a ast-b &rest args &key &allow-other-keys)
  (assert (proper-list-p ast-a))
  (assert (proper-list-p ast-b))
  ;; Drop common prefix and postfix, just run the diff on different middle.
  (multiple-value-bind (unique-a unique-b prefix postfix)
      (remove-common-prefix-and-suffix (ast-on-recurse ast-a)
                                       (ast-on-recurse ast-b))
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

      (let ((rdiff (apply #'recursive-diff unique-a unique-b args)))
        (add-common rdiff (diff-cost rdiff))))))

(defun properize (list)
  "Returns the proper part of list and the CDR of the last element"
  (if (proper-list-p list)
      (values list nil)
      (let (tail (e list))
        (values
         (iter (setf tail e)
               (while (consp e))
               (when (consp e) (collect (pop e))))
         tail))))

(defun ast-hash-with-check (ast table)
  "Calls AST-HASH, but checks that if two ASTs have the same hash value,
they are actually equal.  If not, the second one gets a new, fresh hash
value that is used instead."
  (let* ((hash (ast-hash ast))
         (old-ast (gethash hash table)))
    (when (and old-ast (not (ast-equal-p ast old-ast)))
      (iter (incf hash) ; this may be >= sel/sw/ast::+ast-hash-base+, but that's ok
            (while (gethash hash table)))
      (setf (gethash hash table) ast))
    hash))

(defmethod ast-diff (ast-a ast-b &rest args &key (strings t) &allow-other-keys)
  #+debug (format t "ast-diff[T] ~S~%" (mapcar #'class-of (list ast-a ast-b)))
  #+debug (format t "ast-diff[T] subtypep of parseable: ~S~%"
                  (mapcar {typep _ 'sel/sw/parseable:parseable}
                          (list ast-a ast-b)))
  (cond
    ((and (ast-p ast-a)
          (ast-p ast-b)
          (ast-can-recurse ast-a ast-b strings))
     (apply #'ast-diff (ast-children ast-a) (ast-children ast-b) args))
    ((equal ast-a ast-b)
     (values `((:same . ,ast-a)) 0))
    (t (values `((:delete . ,ast-a) (:insert . ,ast-b))
               (+ (ast-cost ast-a) (ast-cost ast-b))))))

(defmethod ast-diff ((ast-a list) (ast-b list) &rest args &key &allow-other-keys
                     &aux tail-a tail-b)
  #+debug (format t "ast-diff[LIST]~%")
  (let* ((new-ast-a (ast-on-recurse ast-a))
         (new-ast-b (ast-on-recurse ast-b)))
    (setf (values ast-a tail-a) (properize new-ast-a))
    (setf (values ast-b tail-b) (properize new-ast-b)))
  #+ast-diff-debug
  (format t "ast-a = ~a, tail-a = ~a~%ast-b = ~a, tail-b = ~a~%"
          ast-a tail-a ast-b tail-b)
  (let* ((table (make-hash-table))
         (hashes-a (mapcar (lambda (ast) (ast-hash-with-check ast table)) ast-a))
         (hashes-b (mapcar (lambda (ast) (ast-hash-with-check ast table)) ast-b))
         (subseq-triples (good-common-subsequences2 hashes-a hashes-b))
         diff-a common-a diff-b common-b)
    ;; split ast-a and ast-b into subsequences
    ;; Get lists of subsequences on which they differ, and subsequences on
    ;; which they are equal.  Some of the former may be empty.
    (setf (values diff-a common-a)
          (split-into-subsequences ast-a
                                   (mapcar (lambda (x) (list (car x) (caddr x)))
                                           subseq-triples)))
    (setf (values diff-b common-b)
          (split-into-subsequences ast-b
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
            ;; (for cb in (reverse (cons nil common-b)))
            ;; (assert (ast-equal-p ca cb))
            (multiple-value-bind (diff cost)
                (apply #'ast-diff-on-lists da db args)
              ;; get rid of this?
              #+nil
              (when (and overall-diff (equalp (lastcar diff) '(:same)))
                 (assert (>= cost 1))
                 (decf cost)
                 (setf diff (butlast diff)))
              (setf overall-diff (append diff overall-diff))
              (incf overall-cost cost))
            (setf overall-diff
                  (append (mapcar (lambda (it) (cons :same it)) ca) overall-diff)))
      ;; Now splice in the tails, if needed
      (if (equalp tail-a tail-b)
          (setf overall-diff
                (if tail-a (append overall-diff `((:same-tail . ,tail-a)))
                    overall-diff))
          (progn
            #+ast-diff-debug (format t "Diff on non-nil tail~%")
            (multiple-value-bind (diff tail-cost)
                (apply #'ast-diff tail-a tail-b args)
              (setf overall-diff
                    (append overall-diff `((:recurse-tail . ,diff))))
              (incf overall-cost tail-cost))))
      (values overall-diff overall-cost))))

(defmethod ast-diff ((s1 string) (s2 string) &key (strings t) &allow-other-keys)
  "special diff method for strings"
  #+debug (format t "ast-diff[STRING]~%")
  (cond
    ;; if STRINGS is true, descend into the strings for a fine-grained diff
    (strings
     (string-diff s1 s2))
    ;; Otherwise, treat the strings as single objects and replace
    ;; entirely if different
    ((string= s1 s2)
     `((:same-sequence ,s1)))
    ((string= s1 "")
     `((:insert-sequence . ,s2)))
    ((string= s2 "")
     `((:delete-sequence . ,s1)))
    (t
     `((:insert-sequence . ,s2)
       (:delete-sequence . ,s1)))))


(defun simple-genome-pack (unpacked-g)
  "Converts list of pairs into a SIMPLE genome"
  (mapcar (lambda (p)
            (assert (stringp p))
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

(defmethod ast-diff ((soft1 simple) (soft2 simple)
                     &rest args &key &allow-other-keys)
  #+debug (format t "ast-diff[SIMPLE]~%")
  (apply #'ast-diff
         (simple-genome-unpack (genome soft1))
         (simple-genome-unpack (genome soft2))
         args))

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
  (if (and (ast-p source)
           (ast-p target))
      (change-segments-on-seqs
       (ast-children source)
       (ast-children target)
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
  "Traverses two sequences and finds the change segments."
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
                 (incf target-position)))))
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

(defmethod print-edit-tree-node ((node edit-tree-node) &key print-asts coherence)
  (assert (typep node 'edit-tree-node))
  ;; If COHERENCE is specified, print only the highest edit tree
  ;; nodes whose coherence is >= this limit
  (let ((node-coherence (coherence node))
        (parent (car *map-edit-tree-ancestors*)))
    (when (or (not coherence)
              (and (>= node-coherence coherence)
                   (or (null parent)
                       (< (coherence parent) coherence))))
      (let ((source-text (ast-text (edit-tree-node-source node)))
            (target-text (ast-text (edit-tree-node-target node)))
            (per-line-prefix
             (coerce (loop for x in *map-edit-tree-ancestors*
                        collect #\> collect #\>) 'string)))
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
                (format t "~a~%---------------~%~a~&" source-text target-text))))
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
         (str (elt (ast-children (edit-segment-node edit-tree-segment))
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
  (let ((len (length list)))
    (if (< len desired-length)
        (append list (make-list (- desired-length len)
                                :initial-element extension-value))
        list)))

(defun apply-values-fn (fn arg-lists replicate?)
  (let* ((len (reduce #'max arg-lists :key #'length))
         (extended-arg-lists
          (iter (for arg-list in arg-lists)
                (collect
                    (extend-list
                     arg-list
                     len (when replicate?
                           (car (last arg-list))))))))
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
        (funcall (car vals) (cadr list-vals))
        (funcall (car vals) (caddr list-vals)))))))

(defmacro cons-values (meld? &rest args)
  `(if ,meld?
       (apply-values-meld #'cons ,@args)
       (apply-values-extend #'cons ,@args)))

(defmacro append-values (meld? &rest args)
  `(if ,meld?
       (apply-values-meld #'append ,@args)
       (apply-values-extend #'append ,@args)))

(defgeneric ast-patch (original diff &rest keys &key conflict &allow-other-keys)
  (:documentation "Create an edited AST by applying DIFF to ORIGINAL.

A diff is a sequence of actions as returned by `ast-diff' including:
:same A B  : keep the current AST
:insert B  : insert B at the current position
:delete A  : remove the current AST
:recurse S : recursively apply script S to the current AST"))

(defmethod ast-patch ((original null) (script null) &key &allow-other-keys)
  (declare (ignorable original script))
  nil)

(defun actual-ast-patch (ast script &rest keys
                        &key delete? (meld? (ast-meld-p ast)) &allow-other-keys)
  (declare (ignorable delete? meld?))
  (let* ((children (ast-children ast))
         ;; For now, always meld
         ;; This may not give valid ASTs, but fix later
         (new-child-lists
          (multiple-value-list
           (apply #'ast-patch children script :meld? meld? keys))))
    (apply #'values
           (iter (for new-children in new-child-lists)
                 (collect (copy ast :children new-children))))))

(defmethod ast-patch ((ast ast) script &rest keys &key &allow-other-keys)
  (apply #'actual-ast-patch ast script keys))

(defmethod ast-patch ((original t) (script cons)
                      &rest keys &key (delete? t) &allow-other-keys)
  (declare (ignorable delete? keys))
  (if (ast-p original)
      (apply #'actual-ast-patch original script keys)
      (case (car script)
        (:recurse-tail
         (ast-patch original (cdr script)))
        (:same
         (assert (ast-equal-p original (cdr script))
                 ()
                 "AST-PATCH: :SAME not same as in script: ~a, ~a"
                 original
                 (cdr script))
         (cdr script))
        (t
         (assert (proper-list-p script))
         (let ((keys (mapcar #'car script)))
           (cond
             ((equal keys '(:same))
              (assert (ast-equal-p original (cdar script))
                      ()
                      "AST-PATCH: :SAME not same as in script(2): ~a, ~a"
                      original
                      (cdar script))
              (cdar script))
             ((equal keys '(:insert :delete))
              (assert (ast-equal-p original (cdadr script))
                      ()
                      "AST-PATCH: ~a not same as in script(2): ~a, ~a"
                      keys
                      original (cdadr script))
              (cdar script))
             ((equal keys '(:delete :insert))
              (assert (ast-equal-p original (cdar script))
                      ()
                      "AST-PATCH: ~a not same as in script(2): ~a, ~a"
                      keys
                      original (cdar script))
              (cdadr script))
             ((member :conflict keys)
              (values-list (iter (for s in (cdr script))
                                 (collect (ast-patch original s)))))
             (t (error "Invalid diff on atom: ~a" script))))))))

#|
(defun create-conflict-node (ast args keys)
  ;; (:conflict . args) being applied to AST
  ;; Create a conflict node with option keys 1, 2, etc.
  (let ((alist
         (iter (for script in args)
               (for i from 1)
               (when script
                 (collecting
                   (list i
                         (apply #'ast-patch ast script keys)))))))
    (make-conflict-ast :children-alist alist)))
|#

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
    (if (or (conflict-ast-p sc) (and (listp sc) (some #'conflict-ast-p sc)))
        ;; Special case
        (values sc (cdr asts))
        ;; Base case
        (let ((consume nil)) ;; If set to true, consume an element of ASTS
          (flet ((%process (action)
                   "Process an inner action.  Returns a list of asts"
                   (ecase (car action)
                     ((nil) nil)
                     (:insert (list (cdr action)))
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
                               (list (ast-patch (car asts) (cdr action)
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
               (make-conflict-ast :child-alist child-alist)
               asts)))))))

(defun ast-patch-same-recurse (asts script tag)
  "Perform actions in SCRIPT in ASTS in parallel with implicit :SAME operations"
  ;; Should only happen when CONFLICT is true
  (ast-patch asts script :tag tag :conflict t))

(defmethod ast-patch ((original cons) (script list)
                      &rest keys &key (delete? t) (meld? t) conflict tag &allow-other-keys)
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
  (labels
      ((merge-conflict-ast (conflict-node rest)
         (if (and (conflict-ast-p (car rest))
                  (conflict-ast-p conflict-node))
             (cons (combine-conflict-asts conflict-node (car rest))
                   (cdr rest))
             (cons conflict-node rest)))
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
                                 (apply #'ast-patch (car asts) args keys)
                                 (edit (cdr asts) (cdr script)))))
               (:same (cons-values meld?
                                   (car asts)
                                   (edit (cdr asts) (cdr script))))
               (:same-tail
                (assert (null (cdr script))) ;; :same-tail always occurs last
                (assert (ast-equal-p asts args)
                        ()
                        "AST-PATCH (CONS): :SAME-TAIL not as as in script: ~a, ~a"
                        asts args)
                asts)
               (:recurse-tail
                (assert (null (cdr script)))
                (ast-patch asts args))
               (:delete
                (assert (ast-equal-p (car asts) args)
                        ()
                        "AST-PATCH (CONS): :DELETE not same as in script: ~a,~a"
                        (car asts) args)
                (cond
                  (tag
                   ;; Conducting an implicit :SAME
                   (let ((alist (iter (for i in '(:old :my :your))
                                      (unless (eql i tag)
                                        (collecting (list i (car asts)))))))
                     (merge-conflict-ast
                      (make-conflict-ast :child-alist alist)
                      (edit (cdr asts) (cdr script)))))
                  (conflict
                   ;; Record this, since it conflicts with :old
                   (multiple-value-bind (conflict-node asts-rest)
                       (ast-patch-conflict-action asts (list (list (car script))))
                     (let ((rest (edit asts-rest (cdr script))))
                       (merge-conflict-ast conflict-node rest))))
                  ;; The key DELETE?, if NIL (default T) will
                  ;; cause :DELETE edits to be ignored.  The
                  ;; use case for this is to do a kind of binary
                  ;; merge of two objects, sharing as much structure
                  ;; as possible
                  (delete?
                   (edit (cdr asts) (cdr script)))
                  (t
                   (cons-values meld? (car asts) (edit (cdr asts) (cdr script))))))
               (:insert
                (if tag
                    (let ((alist `((,tag ,args))))
                      (merge-conflict-ast
                       (make-conflict-ast :child-alist alist)
                       (edit asts (cdr script))))
                    (cons-values meld? args (edit asts (cdr script)))))
               (:insert-sequence
                (append-values meld? args (edit asts (cdr script))))
               (:delete-sequence
                (append-values
                 meld?
                 (iter (while (consp args))
                       (assert asts)
                       (assert (ast-equal-p (car asts) (car args))
                               ()
                               "AST-PATCH (CONS): :DELETE-SEQUENCE not same as in script: ~a, ~a"
                               (car asts) (car args))
                       (let ((a (pop asts)))
                         (when delete? (collect a)))
                       (pop args))
                 (edit asts (cdr script)))))))))
    ;; cause various unmerged subsequences to be combined before
    ;; returning, if meld? is true
    (append-values meld? nil (edit original script))))

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
                         (assert (ast-equal-p s1 s2)
                                 ()
                                 "MELD-SCRIPTS ~a: should have been the same: ~a, ~a"
                                 val s1 s2)))
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
                    (t (error "Do not recognize actions in meld-scripts: ~A, ~A"
                              action1 action2)))))))
    (when (or script1 script2)
      (error
       "Could not meld scripts: different number of fixed location actions"))))

(defmethod ast-patch :around ((original sequence) (script list)
                              &key delete? meld? conflict &allow-other-keys)
  (declare (ignorable delete?))
  (if (find :conflict script :key #'car)
      (if (or meld? conflict)
          (call-next-method)
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
            (values (ast-patch original script1)
                    (ast-patch original script2))))
      (call-next-method)))

(defmethod ast-patch ((original vector) (script list)
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
           (:recurse
            (assert (< i len))
            (let ((vals (multiple-value-list
                         (apply #'ast-patch (elt original i) args keys))))
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

#|
(defmethod ast-patch ((ast ast) script
                      &rest keys &key (delete? t) &allow-other-keys)
  (declare (ignorable delete?))
  (let* ((children (ast-children ast)))
    (let ((children-versions
           (multiple-value-list (apply #'ast-patch children script keys))))
      (apply #'values
             (iter (for patched-children in children-versions)
                   (collect (copy ast :children patched-children)))))))
|#

(defmethod ast-patch ((original simple) script
                      &rest keys &key &allow-other-keys)
  (let ((new-unpacked-genome
         (apply #'ast-patch (simple-genome-unpack (genome original))
                script :meld? t keys)))
    (let ((patched (copy original)))
      (setf (genome patched) (simple-genome-pack new-unpacked-genome))
      patched)))

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
            (insert-end (if no-color "+}" (format nil "+}~a" +color-RST+))))
  (let ((*print-escape* nil)
        (insert-buffer nil)
        (delete-buffer nil))
    (labels ((%p (c) (if (null c)
                         (princ "()" stream)
                         (write (ast-text c) :stream stream)))
             (purge-insert ()
               (when insert-buffer
                 (mapc #'%p (reverse insert-buffer))
                 (write insert-end :stream stream)
                 (setf insert-buffer nil)))
             (purge-delete ()
               (when delete-buffer
                 (mapc #'%p (reverse delete-buffer))
                 (write delete-end :stream stream)
                 (setf delete-buffer nil)))
             (push-insert (c)
               (purge-delete)
               (unless insert-buffer (write insert-start :stream stream))
               (push c insert-buffer))
             (push-delete (c)
               (purge-insert)
               (unless delete-buffer (write delete-start :stream stream))
               (push c delete-buffer))
             (purge ()
               (purge-insert)
               (purge-delete))
             (pr (c) (purge) (%p c))
             (%print-diff (diff)
               (mapc (lambda-bind ((type . content))
                                  (ecase type
                                    (:same (pr content))
                                    (:delete (push-delete content))
                                    (:insert (push-insert content))
                                    (:recurse (%print-diff content))
                                    (:same-sequence (map nil #'pr content))
                                    (:insert-sequence
                                     (map nil #'push-insert content))
                                    (:delete-sequence
                                     (map nil #'push-delete content))
                                    (:same-tail (map nil #'pr content))
                                    (:recurse-tail
                                     (%print-diff
                                      (remove-if
                                       (lambda (e)
                                         (or (equal e '(:delete))
                                             (equal e '(:insert))))
                                       content)))))
                     diff)))
      (%print-diff diff)
      (purge)
      (values))))


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
                     &key conflict &allow-other-keys)
  "Assumes the arguments are things that can be treated as ASTs or SEXPRs."
  (multiple-value-bind (diff problems)
      (merge3 original branch-a branch-b :conflict conflict)
    (values (ast-patch original diff :meld? (not conflict) :conflict conflict)
            problems)))

(declaim (special *unstable*))

(defun merge2 (branch-a branch-b &rest args &key &allow-other-keys)
  "Find an object that contains branch-a and branch-b as substructures.
Do this by computing the diff from branch-a to branch-b, then not performing
the deletions in that diff."
  (let ((diff (apply #'ast-diff branch-a branch-b args)))
    (ast-patch branch-a diff :delete? nil)))

(defvar *conflict* nil "Holds the CONFLICT parameter for MERGE3")

(defun merge3 (original branch-a branch-b &rest args &key conflict &allow-other-keys)
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
           `((:conflict ,(unless leave-a (%f o-a)) ,(unless leave-b (%f o-b)))))))
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
    (declare (ignorable sym-a sym-b))
    (handle-conflict o-a o-b))
  ;; default cases for :insert
  (:method ((sym-a (eql :insert)) (sym-b t) o-a o-b)
    (declare (ignorable sym-a sym-b))
    (handle-conflict o-a o-b :unstable nil :leave-b t))
  (:method (sym-a (sym-b (eql :insert)) o-a o-b)
    (declare (ignorable sym-a sym-b))
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
  
  (:method ((sym-a null) sym-b o-a o-b)
    (handle-conflict o-a o-b :leave-a t))

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

  (:method ((sym-a (eql :insert-sequence)) sym-b o-a o-b)
    (declare (ignorable sym-b))
    (let ((new-o-a (nconc (map 'list (lambda (x) (cons :insert x)) (cdar o-a))
                          (cdr o-a))))
      (merge-diffs2 new-o-a o-b)))
  (:method (sym-a (sym-b (eql :insert-sequence)) o-a o-b)
    (declare (ignorable sym-a))
    (let ((new-o-b (nconc (map 'list (lambda (x) (cons :insert x)) (cdar o-b))
                          (cdr o-b))))
      (merge-diffs2 o-a new-o-b)))

  (:method ((sym-a (eql :delete-sequence)) sym-b o-a o-b)
    (declare (ignorable sym-b))
    (let ((new-o-a (nconc (map 'list (lambda (x) (cons :delete x)) (cdar o-a))
                          (cdr o-a))))
      (merge-diffs2 new-o-a o-b)))
  (:method (sym-a (sym-b (eql :delete-sequence)) o-a o-b)
    (declare (ignorable sym-a))
    (let ((new-o-b (nconc (map 'list (lambda (x) (cons :delete x)) (cdar o-b))
                          (cdr o-b))))
      (merge-diffs2 o-a new-o-b)))

  (:method ((sym-a (eql :same-sequence)) sym-b o-a o-b)
    (declare (ignorable sym-b))
    (setf o-a (same-seq-to-list o-a))
    (merge-diffs2 (same-seq-to-sames o-a) o-b))
  (:method (sym-a (sym-b (eql :same-sequence)) o-a o-b)
    (declare (ignorable sym-a))
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

(defstruct gcs
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
                            (setf (gethash x table) (setf g (make-gcs))))
                          (incf (gcs-count g))
                          (push i (,fn g))))))
      (init-table v1 gcs-positions-1)
      (init-table v2 gcs-positions-2))
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
              (if (and (= (gcs-count g) 2)
                       (gcs-positions-1 g)
                       (gcs-positions-2 g))
                ;; x occurs precisely once in each sequence
                (let ((j (car (gcs-positions-2 g))))
                  (assert (= (car (gcs-positions-1 g)) i))
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

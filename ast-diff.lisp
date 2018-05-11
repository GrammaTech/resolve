;;; ast-diff.lisp --- diffs between ASTs and other tree structures
;;
;; From http://thume.ca/2017/06/17/tree-diffing/#a-tree-diff-optimizer
;;
;;; TODO:
;; Costed structure
;; - cache s expression length and cost
;; - struct outside algorithm
;;
;; Skipping identical prefixes and suffixes
;;
;; Add cost of switch, two grids
;;
;; Lazy “switch” creation
;;
;; Profile
;;
;; A-star
;;
;; Sub-diff for text (customizable)
;;
;; Lisp differ
;; - collect comments
;; - is diff in comments
;; - command line ex’s suitable for use by git
;;
;; C differ
;; - command line ex’s suitable for use by git
(defpackage :software-evolution-library/ast-diff
  (:nicknames :sel/ast-diff)
  (:use
   :common-lisp
   :software-evolution-library/utility
   :alexandria
   :cl-arrowz
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :cl-who)
  (:export
   :ast-diff
   :apply-edit-script
   :diff-elide-same
   :diff-to-html
   :ast-interface
   :ast-equal-p
   :ast-cost
   :ast-can-recurse
   :ast-on-recurse
   :ast-text))
(in-package :software-evolution-library/ast-diff)
(in-readtable :curry-compose-reader-macros)

(defclass ast-interface ()
  ((equal-p :initarg :equal-p
            :documentation "Function to check if two subtrees are equal.")
   (cost :initarg :cost
         :documentation "Function to calculate cost of an AST subtree.")
   (can-recurse :initarg :can-recurse
                :documentation "Function to check if a subtree is atomic.")
   (on-recurse :initarg :on-recurse :initform #'identity
               :documentation "Function to apply on recursion.")
   (text :initarg :text
         :documentation "Function to return the text of a subtree."))
  (:documentation "Collection of functions for interfacing with ast-diff."))

(defmethod ast-equal-p ((interface ast-interface) ast-a ast-b)
  (funcall (slot-value interface 'equal-p) ast-a ast-b))

(defmethod ast-cost ((interface ast-interface) ast)
  (funcall (slot-value interface 'cost) ast))

(defmethod ast-can-recurse ((interface ast-interface) ast-a ast-b)
  (funcall (slot-value interface 'can-recurse) ast-a ast-b))

(defmethod ast-on-recurse ((interface ast-interface) ast)
  (funcall (slot-value interface 'on-recurse) ast))

(defmethod ast-text ((interface ast-interface) ast)
  (funcall (slot-value interface 'text) ast))

(defun ast-diff (interface ast-a ast-b
                 &optional (depth 0))
  "Return a least-cost edit script which transforms AST-A into AST-B.
Also return a second value indicating the cost of the edit.

See APPLY-EDIT-SCRIPT for more details on edit scripts.

INTERFACE is an AST-INTERFACE object containing the functions need to
determine equality, cost, etc for ASTs.

ASTs should be AST structs from above or cons cells where the CAR
stores the node data (not used directly by the diff algorithm but
may be examined by the interface functions) and the CDR is the children.
"
  ;; Edit scripts are represented by a (length a) x (length b) grid.
  ;; Each position A,B on the grid stores the diff between (subseq
  ;; ast-a A) and (subseq ast-b B), along with its cost. The problem
  ;; then reduces to finding the least-cost path from the upper-left
  ;; to lower-right corners of the grid.

  ;; Along the way we can make the following moves:
  ;; Delete the head of AST-A (move right in the grid)
  ;; Insert the head of AST-B (move down in the grid)\
  ;; If the head of AST-A and AST-B are equal, move diagonally.
  ;; Recursively edit the head of AST-A to match the head of AST-B,
  ;; and move diagonally.

  ;; Intermediate results are cached a in two-dimensional array to
  ;; avoid redundant computation. Each dimension is padded by 1 to
  ;; simplify boundary conditions.
  (declare (optimize speed))
  (declare (type fixnum depth))
  (let* ((vec-a (coerce (append (tree-right-walk
                                 (ast-on-recurse interface ast-a)) '(nil))
                        'vector))
         (vec-b (coerce (append (tree-right-walk
                                 (ast-on-recurse interface ast-b)) '(nil))
                        'vector))
         (costs (make-array (list (length vec-a) (length vec-b))
                            :initial-element nil)))
    (labels
        ;; Result is a list of (script cost)
        ((cost (result)
           (if result (second result) infinity))
         (script (result)
           (when result (first result)))
         (extend (result script cost)
           (list (cons script (script result))
                 (+ cost (cost result))))
         (compute-diff (index-a index-b)
           (declare (type fixnum index-a))
           (declare (type fixnum index-b))
           ;; Moving off the grid is illegal/infinite cost.
           (when (some #'>= (list index-a index-b) (array-dimensions costs))
             (return-from compute-diff (list nil infinity)))

           ;; Use memoized value if available.
           (&>> (aref costs index-a index-b)
                (return-from compute-diff))

           (let* ((head-a (aref vec-a index-a))
                  (head-b (aref vec-b index-b))
                  (heads-equal (ast-equal-p interface head-a head-b))
                  (can-recurse (ast-can-recurse interface head-a head-b))
                  ;; Compute neighbors.
                  (down (compute-diff index-a (1+ index-b)))
                  (across (compute-diff (1+ index-a) index-b))
                  ;; Only compute diagonal and recursive costs if needed.
                  (diagonal (when (or heads-equal can-recurse)
                              (compute-diff (1+ index-a) (1+ index-b))))
                  (subtree (when (and can-recurse (not heads-equal))
                             (multiple-value-list
                              (ast-diff interface head-a head-b (1+ depth)))))
                  ;; Actions.
                  (same (when heads-equal
                          (extend diagonal
                                  (list :same head-a head-b)
                                  0)))
                  (recurse (extend diagonal
                                   (cons :recurse (script subtree))
                                   (cost subtree)))
                  (insert (extend down
                                  (cons :insert head-b)
                                  (ast-cost interface (aref vec-b index-b))) )
                  (delete (extend across
                                  (cons :delete head-a)
                                  (ast-cost interface (aref vec-a index-a)))))
             ;; Pick the best action.
             ;; Note: Illegal actions will have infinite cost so we
             ;;       don't have to consider them specially here.
             (setf (aref costs index-a index-b)
                   (let ((it (cond
                               ((and (<= (cost same) (cost insert))
                                     (<= (cost same) (cost delete)))
                                same)
                               ((and (<= (cost recurse) (cost insert))
                                     (<= (cost recurse) (cost delete)))
                                recurse)
                               ((< (cost insert) (cost delete))
                                insert)
                               (t delete))))
                     ;; (format t "~a ~a ~a ~a~%" depth index-a index-b (cost it))
                     it)))))
      ;; Pre-fill the grid for the target state.
      (setf (aref costs (1- (length vec-a)) (1- (length vec-b))) '(nil 0))
      ;; Compute diff from the start (top,left) to the target (bottom,right).
      (values-list (compute-diff 0 0)))))

(defun diff-elide-same (edit-script)
  "Return the non-same subset of EDIT-SCRIPT with path information.
Path's are represented as a sequence of car (:A) and cdr (:D) from the
root of the edit script (and implicitly also the program AST)."
  (labels
      ((follow (edit-script path)
         (when edit-script
           (append
            (case (caar edit-script)
              (:same nil)
              (:recurse (follow (cdar edit-script) (cons :a path)))
              (t (list (cons (reverse path) (car edit-script)))))
            (follow (cdr edit-script) (cons :d path))))))
    (follow edit-script nil)))

(defgeneric apply-edit-script (interface original script)
  (:documentation "Create an edited AST by applying SCRIPT to ORIGINAL.

An edit script is a sequence of actions, of the following types:
:same A B  : keep the current AST
:insert B  : insert B at the current position
:remove A  : remove the current AST
:recurse S : recursively apply script S to the current AST"))

(defmethod apply-edit-script ((interface ast-interface)
                              (original list)
                              (script list))
  (labels
      ((edit (asts script)
         (when script
           (destructuring-bind (action . args) (car script)
             (ecase action
               (:recurse (cons (apply-edit-script interface (car asts)
                                                  args)
                               (edit (cdr asts) (cdr script))))
               (:same (assert (apply #'ast-equal-p interface args))
                      (cons (car asts)
                            (edit (cdr asts) (cdr script))))
               (:delete (assert (ast-equal-p interface (car asts) args))
                        (edit (cdr asts) (cdr script)))
               (:insert (cons args
                              (edit asts (cdr script)))))))))
    (cons (car original) (edit (cdr original) script))))

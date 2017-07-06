;; Wilsen Kosasih
;; HW1

;; #1 TREE-CONTAINS : Check if N is in TREE
;; if atom, check if equal
;; if list, check if mid equal, if less, go left, if more, go right
(defun TREE-CONTAINS (N TREE)
  (cond ((AND (atom TREE) (equal N TREE)) t)
	((AND (atom TREE) (< N TREE)) NIL)
	((AND (atom TREE) (> N TREE)) NIL)
	((equal N (second TREE)) t)
	((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
	(t (TREE-CONTAINS N (third TREE)))))

;; #2 TREE-MAX : returns highest number in the TREE
;; base case: tree is an atom -> return it
;; otherwise, keep moving to the right most leaf
(defun TREE-MAX (TREE)
  (cond ((atom TREE) TREE)
	(t (TREE-MAX(third TREE)))))

;; #3 TREE-ORDER: returns the tree in in-order list
;; base case: tree is an atom -> return it
;; otherwise, append the list in-order (left, middle, right)
(defun TREE-ORDER (TREE)
  (cond ((atom TREE) (list TREE))
	(t (append (TREE-ORDER (first TREE))
		   (cons (second TREE) NIL)
		   (TREE-ORDER (third TREE))))))

;; #4 SUB-LIST: returns sub-list of L starting at position START and length LEN
;; base case: list is null -> return NIL
;;            LEN = 0 -> return NIL
;;            START /= 0 -> move to the right on the list until START = 0
;; Otherwise, construct a list beginning when START = 0 until L equal 0 
(defun SUB-LIST (L START LEN)
  (cond ((NULL L) NIL)
	((= LEN 0) NIL)
	((/= START 0) (SUB-LIST (rest L) (- START 1) LEN))
	(t (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))))

;; #5 SPLIT-LIST: returns two list split from the original one where the first list is 1 letter smaller or equal in length than the second one.
;; base case: list is even -> return a list of 2 list with the same length (0,N/2-1)&(N/2,N-1)
;; otherwise, return a list of 2 list with L1 a letter shorter than L2 (0,N-1/2-1)&(N-1/2,N-1)
(defun SPLIT-LIST(L)
  (cond ((evenp (length L)) 
	 (list (SUB-LIST L 0 (/ (length L) 2))
	       (SUB-LIST L (/ (length L) 2) (- (length L) (/ (length L) 2)))))
	(t (list (SUB-LIST L 0 (/ (- (length L) 1) 2))
		(SUB-LIST L (/ (- (length L) 1) 2) (- (length L) (/ (- (length L) 1) 2)))))))

;; #6 BTREE-HEIGHT: returns the height of the tree
;; base case: TREE is an atom -> return 0
;; otherwise, measure the max height of the tree
(defun BTREE-HEIGHT(TREE)
  (cond ((atom TREE) 0)
	(t
	 (let* (
		(right (+ 1 (BTREE-HEIGHT (second TREE))))
		(left (+ 1 (BTREE-HEIGHT (first TREE))))
	       )
	   (cond ((< left right) right)
		 (t left))))))

;; #7 LIST2BTREE: return binary tree from list LEAVES
;; base case: length = 1 -> return it
;; otherwise, keep splitting the list
(defun LIST2BTREE (LEAVES)
  (cond ((= (length LEAVES) 1) (first LEAVES))
	(t (let* (( l1l2 (SPLIT-LIST LEAVES)))
	     (list (LIST2BTREE (first l1l2)) (LIST2BTREE (second l1l2)))))))

;; #8 BTREE2LIST: return a list of atoms from btree TREE
;; keep appending the list
(defun BTREE2LIST (TREE)
  (cond ((listp TREE) (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
	(t (list TREE))))

;; #9 IS-SAME: return true iff the two input lists are the same
;; base case: both NULL -> true
;; base case: either not NULL -> false
;; check if the first element is equal and move on to the rest
(defun IS-SAME(E1 E2)
  (cond ((AND (NULL E1) (NULL E2)) t)
	((OR (NULL E1) (NULL E2)) NIL)
	(t (AND (equal (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))))
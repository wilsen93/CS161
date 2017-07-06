;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;

(defun goal-test (s)
  (cond ((null s) T)                 ;if state is null, return true
	((boxinrow (first s)) nil)   ;if boxinrow returns true, return false
	(t (goal-test (rest s)))))   ;otherwise, move to the next row

(defun boxinrow (r)
  (cond ((null r) nil)               ;if row is already null, return false
	((isBox (first r)) T)        ;if the box is at the beginning, return true
	(t (boxinrow (rest r)))))    ;move on the the rest of the column

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;

; This function returns the sucessor states of s
; It returns the list of all possible states after the keeper moves up left right down
(defun next-states (s)
  (let* ((UP 1)
	 (RIGHT 2)
	 (DOWN 3)
	 (LEFT 4)
	 (result (list (try-move s UP) (try-move s RIGHT) (try-move s DOWN) (try-move s LEFT))))
    (cleanUpList result)))


; This function returns the new state after a movement is done or NIL if the move is invalid
(defun try-move (s direction)
  (let* (
	 (x (first (getKeeperPosition s 0)))    ;store x-coordinate of keeper
	 (y (second (getKeeperPosition s 0)))   ;store y-coordinate of keeper
    )
  (cond ((cantmove s direction x y) nil)        ;if the move is illegal, return nil
	(t (move s direction x y))              ;otherwise return the new state
  ))
)

;; This function returns Nil if a movement is valid, it returns true otherwise
;; For each direction it checks
;; 1) If the movement causes the keeper to end up in a wall -> returns true
;; 2) If the movement causes a box to end up in the same grid as a wall, boxstar or another box -> returns true
;; Otherwise, returns false (movement is valid)
(defun cantmove (s direction x y)
  (cond ((= direction 1)
	 (cond ((isWall (get-square s (- y 1) x)) t)
	       ((and (or (isBox (get-square s (- y 1) x)) (isBoxStar (get-square s (- y 1) x))) 
		     (or (isBox (get-square s (- y 2) x)) (or (isWall (get-square s (- y 2) x)) (isBoxStar (get-square s (- y 2) x)))))t)
	       (t nil)))
	 ((= direction 2)
	 (cond ((isWall (get-square s y (+ x 1))) t)
	       ((and (or (isBox (get-square s y (+ x 1))) (isBoxStar (get-square s y (+ x 1)))) 
		     (or (isBox (get-square s y (+ x 2))) (or (isWall (get-square s y (+ x 2))) (isBoxStar (get-square s y (+ x 2))))))t)
	       (t nil)))
	 ((= direction 3)
	 (cond ((isWall (get-square s (+ y 1) x)) t)
	       ((and (or (isBox (get-square s (+ y 1) x)) (isBoxStar (get-square s (+ y 1) x))) 
		     (or (isBox (get-square s (+ y 2) x)) (or (isWall (get-square s (+ y 2) x)) (isBoxStar (get-square s (+ y 2) x)))))t)
	       (t nil)))
	 ((= direction 4)
	 (cond ((isWall (get-square s y (- x 1))) t)
	       ((and (or (isBox (get-square s y (- x 1))) (isBoxStar (get-square s y (- x 1)))) 
		     (or (isBox (get-square s y (- x 2)))  (or (isWall (get-square s y (- x 2))) (isBoxStar (get-square s y (- x 2))))))t)
	       (t nil)))
	 (t t)
   )
)

;; This function returns the value present in a grid
(defun get-square (s y x)
  (cond ((null s) 1)                                                        ;if state is empty, assume the grid is a wall
	((atom s) s)                                                        ;if it's an atom, return it
	((< y 0) 1)                                                         ;if the y coordinate specified is neg, it's a wall
	((< x 0) 1)                                                         ;if the x coordinate specified is neg, it's a wall
	((> y (- (length s) 1)) 1)                                          ;if the y coordinate specified is not on map, it's a wall
	((> x (- (length (car s)) 1)) 1)                                    ;if the x coordinate specified is not on map, it's a wall
	((> y 0) (get-square (rest s) (- y 1) x))                           ;if y>0, move on to the next row
	((> x 0) (get-square (cons (rest (first s)) (rest s)) y (- x 1)))   ;if x>0, move on to the next column
	(t (get-square (first (first s)) y x))                              ;we found the grid
   )
)

;; This function check what happened to the grid that the keeper moved on from
(defun move (s direction x y)
  (cond ((isKeeperStar (get-square s y x)) (set-square (movek s direction x y) y x 4)) ;the grid was a keeperstar -> return it to a star
	(t (set-square (movek s direction x y) y x 0))                                 ;the grid was empty -> return it to empty
  ) 
)

;; This function returns the state after a change is made in a grid
(defun set-square (s y x n)
  (cond ((null s) nil)                                                                 ;the state is empty, return it
	((> y (- (length s) 1)) s)                                                     ;the y coor is neg, return original state
	((> x (- (length (car s)) 1)) s)                                               ;the x coor is neg, return original state
	((> y 0) (cons (first s) (set-square (rest s) (- y 1) x n)))                   ;y > 0, move on to the next row
	((> x 0) (let* ((z (set-square (cons (rest (first s)) (rest s)) y (- x 1) n))) ;x > 0, move on to the next column
		   (cons (cons(first ( first s)) (first z)) (rest z))))                ;change the grid located at x,y to value n
	(t (cons (cons n (rest (first s))) (rest s)))
  )
)

;; This function moves the seeker into the appropriate grid and return the state
;; For each direction, it checks
;; 1) The grid it's moving into is a star -> the grid becomes seekerstar
;; 2) The grid it's moving into is a boxstar -> the grid becomes seekerstar & move the box
;; 3) Otherwise, the grid just become a seeker grid (also moves the box if there's one in the grid)
;; Please be reminded that whether or not the movement is valid is already checked by cantmove function
(defun movek(s direction x y) 
  (cond ((= direction 1)
	 (cond ((isStar (get-square s (- y 1) x)) (set-square s (- y 1) x 6))
	       ((isBoxStar (get-square s (- y 1) x)) (set-square (moveb s direction x y) (- y 1) x 6))
	       (t (set-square (moveb s direction x y) (- y 1) x 3)))
	 )
	((= direction 2)
	 (cond ((isStar (get-square s y (+ x 1))) (set-square s y (+ x 1) 6))
	       ((isBoxStar (get-square s y (+ x 1))) (set-square (moveb s direction x y) y (+ x 1) 6))
	       (t (set-square (moveb s direction x y) y (+ x 1) 3)))
	 )
	((= direction 3)
	 (cond ((isStar (get-square s (+ y 1) x)) (set-square s (+ y 1) x 6))
	       ((isBoxStar (get-square s (+ y 1) x)) (set-square (moveb s direction x y) (+ y 1) x 6))
	       (t (set-square (moveb s direction x y) (+ y 1) x 3)))
	 )
	((= direction 4)
	 (cond ((isStar (get-square s y (- x 1))) (set-square s y (- x 1) 6))
	       ((isBoxStar (get-square s y (- x 1))) (set-square (moveb s direction x y) y (- x 1) 6))
	       (t (set-square (moveb s direction x y) y (- x 1) 3)))
	 )
   )
)

;; This function moves the box into the appropriate grid and return the state
;; For each direction, it checks
;; 1) The grid is habitated by a box or a boxstar
;; 2) The grid it's moving into is a star -> the grid becomes boxstar
;; 3) The grid it's moving into is a boxstar -> the grid becomes boxstar
;; 4) Otherwise, the grid just become a box grid 
;; Please be reminded that whether or not the movement is valid is already checked by cantmove function
(defun moveb (s direction x y)
  (cond ((= direction 1)
	 (cond ((isBox (get-square s (- y 1) x)) 
		(cond ((isStar (get-square s (- y 2) x)) (set-square s (- y 2) x 5))
		      (t (set-square s (- y 2) x 2))))
	       ((isBoxStar (get-square s (+ y 1) x)) 
		(cond ((isStar (get-square s (- y 2) x)) (set-square s (- y 2) x 5))
		      (t (set-square s (- y 2) x 2))))
	       (t s))
	 )
	((= direction 2)
	 (cond ((isBox (get-square s y (+ x 1))) 
		(cond ((isStar (get-square s y (+ x 2))) (set-square s y (+ x 2) 5))
		      (t (set-square s y (+ x 2) 2))))
	       ((isBoxStar (get-square s y (+ x 1)))
		(cond ((isStar (get-square s y (+ x 2))) (set-square s y (+ x 2) 5))
		      (t (set-square s y (+ x 2) 2))))
	       (t s))
	 )
	((= direction 3)
	 (cond ((isBox (get-square s (+ y 1) x)) 
		(cond ((isStar (get-square s (+ y 2) x)) (set-square s (+ y 2) x 5))
		      (t (set-square s (+ y 2) x 2))))					     
	       ((isBoxStar (get-square s (+ y 1) x)) 
		(cond ((isStar (get-square s (+ y 2) x)) (set-square s (+ y 2) x 5))
		      (t (set-square s (+ y 2) x 2))))
	       (t s))
	 )
	((= direction 4)
	 (cond ((isBox (get-square s y (- x 1))) 
		(cond ((isStar (get-square s y (- x 2))) (set-square s y (- x 2) 5))
		      (t (set-square s y (- x 2) 2))))
	       ((isBoxStar (get-square s y (- x 1)))
		(cond ((isStar (get-square s y (- x 2))) (set-square s y (- x 2) 5))
		      (t (set-square s y (- x 2) 2))))
	       (t s))
	 )
   )
)
  
; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s)
  0)              ;return 0

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
; It is admissible because the number of misplaced boxes in s will be lesser than the number of steps
; required to finish the map

(defun h1 (s)
  (cond ((goal-test s) 0)       ;if s is a goal-state, return 0
	(t (countbox s 0))))    ;otherwise, use helper function

(defun countbox (s total)
  (cond ((null s) total)                                           ;if s is null, return total
	(t (countbox (rest s) (+ (count 2 (first s)) total)))))    ;count #box in the row and add it to #box on the rest of rows

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

; For this problem, my idea was to return the total minimum distance of all boxes
; to the closest goal grid

(defun h604501335 (s)
 (let (
       (a (remove nil (apply #'append (boxlocation s 0))))
       (b (remove nil (apply #'append (goallocation s 0))))
       )
   (totalmindistance a b)
 )
)

; return the location of all boxes
(defun boxlocation (s r)
  (cond ((null s) nil)
	(t (cons (boxlocationrow (first s) r 0) (boxlocation (rest s) (+ r 1)))))
)

; return the location of all boxes in a row
(defun boxlocationrow (s r c)
  (cond ((null s) nil)
	((= (first s) 2)  ( cons (list r c) (boxlocationrow (rest s) r (+ c 1))))
	(t (append (boxlocationrow (rest s) r (+ c 1)) nil)))
)

; return the location of all goal
(defun goallocation (s r)
  (cond ((null s) nil)
	(t (cons (goallocationrow (first s) r 0) (goallocation (rest s) (+ r 1)))))
)

; return the location of all goal in a row
(defun goallocationrow (s r c)
 (cond ((null s) nil)
	((= (first s) 4) ( cons (list r c) (goallocationrow (rest s) r (+ c 1))))
	(t (append (goallocationrow (rest s) r (+ c 1)) nil)))
)

; return the total min distance of a box to a goal
(defun totalmindistance (a b)
  (cond ((null a) 0)
	(t (+ (mindistance (first a) b) (totalmindistance (rest a) b))))
)

; return the min distance of a box to a goal
(defun mindistance (a b)
  (cond ((null b) 999)
	(t (min (distance a (first b)) (mindistance a (rest b)))))
)

; return the distance between a box and a goal
(defun distance (a b)
  (let* (
	 (ax (first a))
	 (ay (second a))
	 (bx (first b))
	 (by (second b))
	 (distancey (abs (- ay by)))
	 (distancex (abs (- ax bx)))
	 (distance (+ distancey distancex))
	 )
    distance
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

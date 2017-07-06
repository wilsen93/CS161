(defun sat? (numberofvar clauses)
  (check numberofvar clauses () numberofvar)
)

; check if any variable combination is satisfiable
(defun check (var clauses checked max)
  (cond ((= var 0) checked)
	;if var = 0 -> return checked
	((checkallclause clauses (append checked (list var))) (check (- var 1) clauses (append checked (list var)) max))
	;check if pos ok -> if ok go to next var
	((checkallclause clauses (append checked (list (- 0 var)))) (check (- var 1) clauses (append checked (list (- 0 var))) max))
	;check if neg ok -> if ox go to next var
	(t 
	 (cond ((not (null checked)) (helper var clauses checked max))
	       (t nil)
	       )
	 )
	)
  )

; Used for backtracking
(defun helper (var clauses checked max)
  (cond ((null checked) nil)
        ((> var max) nil)
	((< (first(last checked)) 0) (helper (+ var 1) clauses (remove (- 0 (+ var 1)) checked) max))
	((checkallclause clauses (append (remove (+ var 1) checked) (list (- 0 (+ 1 var))))) 
	 (check var clauses (append (remove (+ var 1) checked) (list (- 0 (+ 1 var)))) max))
	(t (helper (+ var 1) clauses (remove (+ var 1) checked) max))
	)
  )

; check if all clasues are satisfied by checked
(defun checkallclause (clauses checked)
  (cond ((null clauses) t)                                     ; if clauses empty, return true
	((null (checkoneclause (first clauses) checked)) nil)  ; if a clause does not satisy, return false
	(t (checkallclause (rest clauses) checked))            ; otherwise, check the rest of thr clauses
	)
)

; check if a clause satisfied by current check
(defun checkoneclause (clause checked)
  (cond ((null clause) nil)                             ; if clause empty, return false
	((not (member (- 0 (first clause)) checked)) t) ; if check doesnt contain the negation of a variable in clause, return true
	                                                ; This is used to assume that the variable in checked is valid as long as
	                                                ; all not variables in clause are in checked
	((member (first clause) checked) t)             ; if check contains the same variable as clause, return true
	(t (checkoneclause (rest clause) checked))      ; check the next variable
	)
)
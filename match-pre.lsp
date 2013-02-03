(defun is-submatch (token)
  (listp token))

(defun is-variable (token)
  (if (member token '(?x ?y ?z)) token NIL))

(defun is-any (token)
  (if (equal token '*) T NIL))
	
(defun is-single (token)
  (if (equal token '?) T NIL))

(defun match-single (pattern data)
  (match (cdr pattern) (cdr data)))

(defun match-any (pattern data)
  (or (match pattern (cdr data)) (match (cdr pattern) (cdr data))))

(defun has-multiple-bindings (bindings)
  (listp (car (car bindings))))

;; try to add current binding to existing ones
;; if conflict return NIL
;; if exists do nothing
;; if not add
(defun try-add-binding (key value binding)
  (if (null (assoc key binding))
	  (acons key value binding) ;; add new
	  (if (equal value (cdr (assoc key binding)))
		  binding ;; do nothing
		  NIL))) ;; conflict

(defun update-bindings (key value bindings)
  (let ((new-bindings
		 (if (null bindings) ;; if end of the binding list
			 ()
			 (if (null (try-add-binding key value (car bindings)))
				 (update-bindings key value (cdr bindings))
				 (cons (try-add-binding key value (car bindings)) (update-bindings key value (cdr bindings)))))))
	(if (null (cdr new-bindings)) (car new-bindings) new-bindings)))
	

(defun match-variable (pattern data)
  ;; check following matching
  ;; case 1: it returns T or NIL
  ;; case 2: it returns an a-list
  ;; case 3: it returns a list of a-list
  (let ((binding (match (cdr pattern) (cdr data)))
		(key (car pattern))
		(value (car data)))
	(cond
	  ((equal binding T) (acons key value ()))
	  ((null binding) NIL)
	  ((has-multiple-bindings binding) (update-bindings key value binding))
	  (T (try-add-binding key value binding)))))
;;  (setq result1 (cons (list (car pattern) (car data)) result1)) ;; add the new binding into result
;;  (match (cdr pattern) (cdr data))) ;; continue to match

(defun match-submatch (pattern data bindings)
  (and (match (car pattern) (car data)) (match (cdr pattern) (cdr data)))) ;; first match the subquery, then continue back
  
;; new interface
;; pattern
;; data
;; bindings: a list of alist 
(defun do-match (pattern data bindings)
  (cond
	((and (null pattern) (null data)) bindings) ;; case 1: pattern is nil, data is nil -> T
	((or (null pattern) (null data)) NIL) ;; case 2: pattern is nil, data is not nil -> NIL
	(T (cond
		 ((is-single (car pattern)) (match-single pattern data)) ;; matches single
		 ((is-any (car pattern)) (match-any pattern data)) ;; matches kleen star
		 ((is-variable (car pattern)) (match-variable pattern data)) ;; matches variable
		 ((is-submatch (car pattern)) (match-submatch pattern data)) ;; matches list
		 (T (if (equal (car pattern) (car data))
				(match (cdr pattern) (cdr data)) ;; match succeed
				NIL))))))						 ;; mismatch




(defun match (pattern data)
  (do-match pattern data '()))

;; revision 1: consider unifying the output to a list of a-list, do result rewrite later
;; revision 2: when seeing a list (around by parensis) in a pattern, it is a sub-query, BUT the result of the sub-query is not independent from following check. we have to carry the binding
;;             DFS problem: checking inconsistent binding during top-down traversal, or during traversing back??
;; revision 3: sanity check of data and pattern format, see if they are valid (parenthesis equal or not): it seems redundant. the lisp intepreter will check


;; method 1: top down, before matching, check current binding set
;; method 2: parallel match, then merge bindings
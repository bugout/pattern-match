;; Pattern Match with post-order DFS traversal

(defun is-submatch (token)
  (listp token))

(defun is-variable (token)
  (cond 
   ((not (symbolp token)) NIL)
   ((equal #\? (char (symbol-name token) 0)) T)
   (T NIL)))

(defun is-any (token)
  (if (equal token '*) T NIL))
	
(defun is-single (token)
  (if (equal token '?) T NIL))

(defun match-single (pattern data)
  (match (cdr pattern) (cdr data)))

(defun is-true (result)
  (if (equal result T) T NIL))

(defun is-false (result)
  (if (equal result NIL) T NIL))


(defun match-any (pattern data)
  (let ((match-zero-result (match (cdr pattern) data))
		(match-more-result (match pattern (cdr data))))
	(cond
	  ((or (is-true match-zero-result) (is-true match-more-result)) T) ;; if either is T, return T
	  ((and (is-false match-zero-result) (is-false match-more-result)) NIL) ;; if both are NIL, return NIL
	  ((is-false match-zero-result) match-more-result) ;; match-zero is NIL, match-more-result has binding
	  ((is-false match-more-result) match-zero-result) ;; match-more is NIL
	  (T (append match-zero-result match-more-result))))) ;; both has bindings

(defun join-bindings (left right)
  (cond
	((or (null left) (null right)) NIL) ;; if either is null, then NIL
	((is-true left) right)
	((is-true right) left)
	(T (cons (merge-binding (car left) right) (join-bindings (cdr left) right)))))

;; merge b1 and b2
;; if no conflict, return merged result, otherwise return nil
(defun merge-two (b1 b2)
  (if (null b1)
	  b2
	  (let ((result (try-add-binding (caar b1) (cdar b1) b2)))
		(if (null result)
			NIL
			(merge-two (cdr b1) result)))))


;; try to merge current binding into the binding set
(defun merge-binding (binding bindings)
  (if (null bindings)
	  NIL
	  (cons (merge-two binding (car bindings)) (merge-binding binding (cdr bindings)))))



;; try to add current binding to existing ones
;; if conflict return NIL
;; if exists do nothing
;; if not add
(defun try-add-binding (key value binding)
  (if (null (assoc key binding))
	  (cons (list key value) binding) ;; add new
	  (if (equal value (cadr (assoc key binding)))
		  binding ;; do nothing
		  NIL))) ;; conflict

;; bindings is always a list of alist
(defun update-bindings (key value bindings)
  (if (null bindings)
	  NIL
	  (if (null (try-add-binding key value (car bindings)))
		  (update-bindings key value (cdr bindings))
		  (cons (try-add-binding key value (car bindings)) (update-bindings key value (cdr bindings))))))

	
(defun match-variable (pattern data)
  (let ((result (match (cdr pattern) (cdr data)))
		(key (car pattern))
		(value (car data)))
	(cond
	  ((is-true result) (list (list (list key value)))) ;; if result is T, create a new result
	  ((null result) NIL) ;; if result is NIL, NIL
	  (T (update-bindings key value result))))) ;; otherwise

;; TODO: consider subquery has multiple bindings
(defun merge-bindings (b1 b2)
  (cond
	((or (null b1) (null b2)) NIL) ;; if either is nil, then nil
	((equal b1 T) b2) ;; if b1=T, it doesn't bind new variable, return b2
	((equal b2 T) b1) ;; if b2=T, similarly
	(T (join-bindings b1 b2)))) ;; if both bind new, we have to do a join


(defun match-submatch (pattern data)
  (if (not (listp (car data))) ;; if data is not a list. is this test necessary?
	  NIL
	  (merge-bindings (match (car pattern) (car data)) (match (cdr pattern) (cdr data))))) ;; match submatch, match next; then merge

(defun match-null-data (pattern)
  (if (null pattern)
	  T
	  (if (is-any (car pattern))
		  (match-null-data (cdr pattern))
		  NIL)))

(defun match-null-pattern (data)
  (null data))
  
(defun match (pattern data)
  (cond
	((null pattern) (match-null-pattern data)) ;; case 1: pattern is nil
	((null data) (match-null-data pattern)) ;; case 2: if data is nil
	(T (cond
		 ((is-single (car pattern)) (match-single pattern data)) ;; matches single
		 ((is-any (car pattern)) (match-any pattern data)) ;; matches kleen star
		 ((is-variable (car pattern)) (match-variable pattern data)) ;; matches variable
		 ((is-submatch (car pattern)) (match-submatch pattern data)) ;; matches list
		 (T (if (equal (car pattern) (car data)) ;; match atoms
				(match (cdr pattern) (cdr data)) ;; match succeed
				NIL))))))						 ;; mismatch

(defun only-one-binding (bindings)
  (null (cdr bindings)))

(defun pattern-match (pattern data)
  (let ((result (match pattern data)))
	(cond
	  ((is-true result) T)
	  ((is-false result) NIL)
	  ((only-one-binding result) (car result))
	  (T result))))
	

;; revision 1: consider unifying the output to a list of a-list, do result rewrite later
;; revision 2: when seeing a list (around by parensis) in a pattern, it is a sub-query, BUT the result of the sub-query is not independent from following check. we have to carry the binding
;;             DFS problem: checking inconsistent binding during top-down traversal, or during traversing back??
;; revision 3: sanity check of data and pattern format, see if they are valid (parenthesis equal or not): it seems redundant. the lisp intepreter will check


;; DO we need to check if input are both lists?
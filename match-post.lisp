;; Pattern Match with post-order DFS traversal

(defun is-subpattern (token)
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

(defun is-true (result)
  (if (equal result T) T NIL))

(defun is-false (result)
  (if (equal result NIL) T NIL))

(defun match-single (pattern data)
  (match (cdr pattern) (cdr data)))

(defun match-any (pattern data)
  (let ((match-zero-result (match (cdr pattern) data))
		(match-more-result (match pattern (cdr data))))
	(cond
	  ((or (is-true match-zero-result) (is-true match-more-result)) T) ;; if either is T, return T
	  ((and (is-false match-zero-result) (is-false match-more-result)) NIL) ;; if both are NIL, return NIL
	  ((is-false match-zero-result) match-more-result) ;; match-zero is NIL, match-more-result has binding
	  ((is-false match-more-result) match-zero-result) ;; match-more is NIL
	  (T (append match-zero-result match-more-result))))) ;; both has bindings

;; try to add current binding to existing ones
;; if conflict return NIL
;; if exists do nothing
;; if not add
(defun try-add-binding (key value binding)
  (if (null (assoc key binding))
	  (cons (list key value) binding) ;; add new
	  (if (equal value (cadr (assoc key binding)))
		  binding ;; if exists, do nothing
		  NIL))) ;; conflict

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

(defun join-bindings (left right)
  (cond
	((or (null left) (null right)) NIL) ;; if either is null, then NIL
	((is-true left) right)
	((is-true right) left)
	(T (cons (merge-binding (car left) right) (join-bindings (cdr left) right)))))

;; if no conflict, return merged result, otherwise return nil
(defun merge-two (b1 b2)
  (if (null b1)
	  b2
	  (let ((result (try-add-binding (caar b1) (cdar b1) b2)))
		(if (null result)
			NIL
			(merge-two (cdr b1) result)))))


(defun merge-binding (binding bindings)
  (if (null bindings)
	  NIL
	  (cons (merge-two binding (car bindings)) (merge-binding binding (cdr bindings)))))

;; (defun merge-bindings (b1 b2)
;;   (cond
;; 	((or (null b1) (null b2)) NIL) ;; if either is nil, then nil
;; 	((equal b1 T) b2) ;; if b1=T, it doesn't bind new variable, return b2
;; 	((equal b2 T) b1) ;; if b2=T, similarly
;; 	(T (join-bindings b1 b2)))) ;; if both bind new, we have to do a join


(defun match-subpattern (pattern data)
;;  (if (not (listp (car data))) ;; if data is not a list. is this test necessary?
;;	  NIL
	  (join-bindings (match (car pattern) (car data)) (match (cdr pattern) (cdr data)))) ;; match submatch, match next; then merge

(defun match-null-data (pattern)
  (if (null pattern)
	  T
	  (if (is-any (car pattern))
		  (match-null-data (cdr pattern))
		  NIL)))

(defun match-null-pattern (data)
  (null data))


;; Working function
;; Return value:
;;   T: matching succeeds and no binding
;;   NIL: matching fails
;;   list<a-list>: matching succeeds, and return the bindings
(defun match (pattern data)
  (cond
	((or (not (listp pattern)) (not (listp data))) NIL) ;; check if pattern and data are list
	((null pattern) (match-null-pattern data)) ;; case 1: pattern is nil
	((null data) (match-null-data pattern)) ;; case 2: if data is nil
	(T (cond
		 ((is-single (car pattern)) (match-single pattern data)) ;; matches ?
		 ((is-any (car pattern)) (match-any pattern data)) ;; matches *
		 ((is-variable (car pattern)) (match-variable pattern data)) ;; matches variable
		 ((is-subpattern (car pattern)) (match-subpattern pattern data)) ;; matches subpattern
		 (T (if (equal (car pattern) (car data)) ;; match atoms
				(match (cdr pattern) (cdr data)) ;; succeed
				NIL))))))						 ;; fail

;; return T if the binding set only has one binding
(defun only-one-binding (bindings)
  (null (cdr bindings)))


;; Match pattern with data
(defun pattern-match (pattern data)
  (let ((result (match pattern data)))
	(cond
	  ((is-true result) T)
	  ((is-false result) NIL)
	  ((only-one-binding result) (car result)) ;; if only has one binding, return an a-list
	  (T result)))) ;; if more than one bindings, return a list of a-list
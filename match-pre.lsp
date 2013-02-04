;; Pattern Match with pre-order DFS traversal

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

(defun match-single (pattern data bindings)
  (match (cdr pattern) (cdr data) bindings))

(defun is-true (result)
  (if (equal result T) T NIL))

(defun is-false (result)
  (if (equal result NIL) T NIL))


(defun match-any (pattern data bindings)
  (let ((match-zero-result (match (cdr pattern) data bindings))
		(match-more-result (match pattern (cdr data) bindings)))
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
		  binding ;; do nothing
		  NIL))) ;; conflict

;; bindings is always a list of alist
(defun update-bindings (key value bindings)
  (if (null bindings)
	  NIL
	  (if (null (try-add-binding key value (car bindings)))
		  (update-bindings key value (cdr bindings))
		  (cons (try-add-binding key value (car bindings)) (update-bindings key value (cdr bindings))))))

(defun match-variable (pattern data bindings)
  (let ((key (car pattern))
		(value (car data)))
	(if (is-true bindings)
		(match (cdr pattern) (cdr data) (list (list (list key value))))
		(let ((new-bindings (update-bindings key value bindings)))
		  (if (null new-bindings)
			  NIL
			  (match (cdr pattern) (cdr data) new-bindings))))))

(defun match-subpattern (pattern data bindings)
;;  (if (not (listp (car data))) ;; if data is not a list. is this test necessary?
;;	  NIL
	  (match (cdr pattern) (cdr data) (match (car pattern) (car data) bindings)))

(defun match-null-data (pattern bindings)
  (if (null pattern)
	  bindings
	  (if (is-any (car pattern))
		  (match-null-data (cdr pattern) bindings)
		  NIL)))

(defun match-null-pattern (data bindings)
  (if (null data) bindings NIL))

(defun match (pattern data bindings)
  (cond
	((or (not (listp pattern)) (not (listp data))) NIL) ;; check if pattern and data are list
	((null bindings) NIL) ;; if current bindings is NIL, no need to search further
	((null pattern) (match-null-pattern data bindings)) ;; case 1: pattern is nil
	((null data) (match-null-data pattern bindings)) ;; case 2: if data is nil
	(T (cond
		 ((is-single (car pattern)) (match-single pattern data bindings)) ;; matches ?
		 ((is-any (car pattern)) (match-any pattern data bindings)) ;; matches *
		 ((is-variable (car pattern)) (match-variable pattern data bindings)) ;; matches variable
		 ((is-subpattern (car pattern)) (match-subpattern pattern data bindings)) ;; matches subpattern
		 (T (if (equal (car pattern) (car data)) ;; match atoms
				(match (cdr pattern) (cdr data) bindings) ;; succeed
				NIL))))))						 ;; fail

(defun only-one-binding (bindings)
  (null (cdr bindings)))

(defun pattern-match (pattern data)
  (let ((result (match pattern data T)))
	(cond
	  ((is-true result) T)
	  ((is-false result) NIL)
	  ((only-one-binding result) (car result))
	  (T result))))
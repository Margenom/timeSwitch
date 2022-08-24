(define*(rex-match? code subject (offset 0)) 
	(define m (rex-match (if (string? code) (rex-compile code) code) subject offset))
	(and m (vector m  subject offset)))

(define (rex-matchl? subject . code_and_offset) (let ((code "") (offset 0)) 
	(map (lambda(P) (if (number? P) (set! offset (+ offset P)) (set! code (string-append code P)))) 
		code_and_offset)
	(rex-match? code subject offset)))

(define*(string-sub subject interv (offset 0)) (apply substring subject 
	(map (lambda(K) (+ offset K)) (list (car interv) (cdr interv)))))
(define (rex-sub match key) (and match (string-sub (match 1) (if (number? key) 
		(vector-ref (rex-match-index (match 0)) key)
		((lambda(T) (and T (cdr T))) (assoc (rex-match-names (match 0)) key)))
	(match 2))))

;assoc list
(define (vector->assoc vec) 
	(do ((i 0 (+ i 1)) (out '() (cons (cons i (vec i)) out))) 
			((>= i (length vec)) out)))
(define (rex-list match) (if (match 0) (let*(
		(index (rex-match-index (match 0))) 
		(names (rex-match-names (match 0))))
	(append names (vector->assoc index))) #f))

(define (list->let lst) (apply inlet (map values lst)))
(define (rex-let match) (list->let (map 
	(lambda(K) (let ((key (if (string? (car K)) (car K) (number->string (car K)))) (interv (cdr K)))
		(values (cons (string->symbol (string-append "$" key)) (string-sub (match 1) interv (match 2)))
			(cons (string->symbol (string-append "i" key)) interv))))
	(rex-list match))))

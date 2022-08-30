(define*(string-sub subject interv (offset 0)) (if (apply and (map (lambda(e) (> 0 e))  interv)) ""  (apply substring subject 
	(map (lambda(K) (+ offset K)) (list (car interv) (cdr interv))))))

;matching
(define*(rex-match? code subject (offset 0)) 
	(define m (rex-match (if (string? code) (rex-compile code) code) subject offset))
	(and m (list m  subject offset)))
(define (rex-matchl? subject . code_and_offset) (let ((code "") (offset 0)) 
	(map (lambda(P) (if (number? P) (set! offset (+ offset P)) (set! code (string-append code P)))) 
		code_and_offset)
	(rex-match? code subject offset)))

(define (rex-val match interv) (string-sub (cadr match) interv (caddr match)))
(define (rex-sub match key) (and match (string-sub (cadr match) (if (number? key) 
		(vector-ref (rex-match-index (car match)) key)
		((lambda(T) (and T (cdr T))) (assoc (rex-match-names (car match)) key)))
	(caddr match))))
(define*(rex-list-names match (moder rex-val)) (map (lambda(p) (cons (car p) (moder match (cdr p)))) (rex-match-names (car match))))
(define*(rex-list-nums match (moder rex-val)) (map (lambda(p) (moder match p)) (vector->list (rex-match-index (car match)))))

;assoc list
(define (vector->assoc vec) 
	(do ((i 0 (+ i 1)) (out '() (cons (cons i (vector-ref vec i)) out))) 
			((>= i (length vec)) (reverse out))))
(define*(rex-list match (converter vector->assoc)) (and match (let*(
		(index (rex-match-index (car match))) 
		(names (rex-match-names (car match))))
	(append (vector->assoc index) names))))
(define (list->let lst) (apply inlet (map values lst)))
(define (rex-let match) (list->let (map 
	(lambda(K) (let ((key (if (string? (car K)) (car K) (number->string (car K)))) (interv (cdr K)))
		(values (cons (string->symbol (string-append "$" key)) (string-sub (match 1) interv (match 2)))
			(cons (string->symbol (string-append "i" key)) interv))))
	(rex-list match))))

;test
(define (test)
(define rexs (map (lambda(k) (rex-match? "(\\d)(?<tst>\\d)?(\\d)?" k)) '("123" "12" "3")))
(print rexs)
(map print (map rex-list rexs))
)

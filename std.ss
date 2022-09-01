;io
(define (print . strs) (map display strs) (newline))
(define (read-lines filename) (with-input-from-file filename (lambda()
	(do ((line (read-line) (read-line)) (out '() (cons line out)))
		((equal? #<eof> line) (reverse out))))))

;strings
(define (string-split str sep) (let rec((ost (string->list str)) (word '()) (out '()))
	(if (null? ost) (reverse out)
		(rec (cdr ost) (if (member (car ost) (string->list sep)) '() (cons (car ost) word)) 
			(if (member (car ost) (string->list sep)) (cons (list->string (reverse word)) out) out)))))
(define (string-join sep . lst) 
	(do ((ost (cdr lst) (cdr ost)) (out (car lst) (string-append out sep (car ost)))) ((null? ost) out)))
;(fold (lambda(k v) (string-append v sep k)) (car lst) lst))

;lists
(define (filter pred lst)
	(if (null? lst) lst 
		(if (pred (car lst)) (let ((new-tail (filter pred (cdr lst))))
			(if (eq? (cdr lst) new-tail) lst (cons (car lst) new-tail)))
			(filter pred (cdr lst)))))

;args
(define*(need-args? off required optional . params) (define leng (- (length CLI_ARGS) off))
;	(print off 'r required 'o optional	) 
	(and (apply and (map (lambda(p) (member p (map car CLI_PARAMS))) params)) (>= (+ (if optional optional leng) required) leng required)))
(define*(param-or-val pname val (modif values)) ((lambda(p) (if p (modif (cdr p)) val)) (assoc pname CLI_PARAMS)))

;primitive repl
(define (repl) (do ((Ln "REPL" (read)) (i 0 (+ i 1))) ((eq? Ln #<eof>) (display "Bye..\n"))
	(write (eval Ln)) (newline) (display i) (display "> ")))

(define*(clock-pretty utime (format "%a %d.%m (%Y) %H:%M")) (clock-format format (clock utime))) 

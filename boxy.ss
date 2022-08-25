;libs
(load "rex.ss")

;globals

;functions
(define (print . strs) (map display strs) (newline))
(define (print-help) (print "help"))
(define (string-split str sep) (let rec((ost (string->list str)) (word '()) (out '()))
	(if (null? ost) (reverse out)
		(rec (cdr ost) (if (member (car ost) (string->list sep)) '() (cons (car ost) word)) 
			(if (member (car ost) (string->list sep)) (cons (list->string (reverse word)) out) out)))))
(define*(need-args? off required optional . params) (define leng (- (length CLI_ARGS) off))
	(print off 'v required 'b optional	)
	(and (apply and (map (lambda(p) (member p (map car CLI_PARAMS))) params)) (>= (+ (if optional optional leng) required) leng required)))
(define*(param-or-val pname val (modif values)) ((lambda(p) (modif (if p (cdr p) val))) (assoc pname CLI_PARAMS)))

(define now_test #(1 2 3 4 5 6 7))
(define (bisy-parse-line line) (define iform "((?:(?:\\d\\d?,)*(?:\\d\\d?)|\\*|\\d\\d?-\\d\\d?)(?:/\\d\\d?)?)\\s+")
	(define daln (rex-matchl? line "^" iform iform iform iform iform "(\\d+)\\s(.+)$"))
	(define*(asnum nums key ifnil (getter rex-sub)) ((lambda(p) (if (= 0 (string-length p)) 
		(if ifnil ifnil (print "bisy-parse-line asnum null")) (string->number p))) (getter nums key)))
	(define (tester razr) (lambda(val) (let*(
			(upper (vector-ref #(59 23 31 12 7 1440) (- razr 1)))
			(lower (vector-ref #( 0  0  1  1 0    0) (- razr 1)))
			(ranged (min (max lower val) upper)))
		(unless (= val ranged) (print "in " line "\n\tunranged value: " val " (" lower "," upper "), " ranged  " used"))ranged)))
	(if daln (let rec((dls (rex-list-nums daln)) (out '()) (tm #f) (itr 0))
		(if (null? dls) (cdr (reverse out)) (begin (rec (cdr dls) (cons (cond
			  ((set! tm (rex-match? "^((?:\\d\\d?,)*)(\\d\\d?)$" (car dls)))
				(let*((digstr (rex-sub tm 1)) (digend (list (rex-sub tm 2))) (digits (map string->number 
						(if (string=? "" digstr) digend (append (string-split digstr ",") digend)))))
					(lambda(now) (member now (map (tester itr) digits))))
			) ((set! tm (rex-match? "^(\\d\\d?)-(\\d\\d?)(?:/(\\d\\d?))?$" (car dls)))
				(do ((b (min (asnum tm 1 0) (asnum tm 2 0)) (+ b 1)) 
						(inr '() (if (= 0 (modulo b (asnum tm 3 b))) (cons b inr) inr)))
					((> b (max (asnum tm 1 59) (asnum tm 2 59)))
						(lambda(now) (member now (map (tester itr) inr))))) ; (vector-ref now_test (- itr 1)))))
			) ((set! tm (rex-match? "^\\*(?:/(\\d\\d?))?$" (car dls)))
				(lambda(now) (= 0 (modulo now ((tester itr) (asnum tm 1 now))))) ;(vector-ref now_test (- itr 1)))
			) ((and (= itr 6) (set! tm (rex-match? "^\\d+$" (car dls)))) ((tester itr) (string->number (car dls)))
			) ((= itr 7) (car dls) 
			) (else #f)) out) tm (+ 1 itr)))))))

;main
(case (string->symbol (if (need-args? 1 1 #f) (cadr CLI_ARGS) "help"))
	((select) (if (need-args? 2 1 0) (begin (print "test sel")) (print-help)))
	((list) (if (need-args? 2 0 #f) (begin (print "test list")) (print-help)))
	((check) (if (need-args? 2 0 0) (begin (print "test check")) (print-help)))
	((now) (if (need-args? 2 0 0) (begin (print "test now")) (print-help)))
	((stat) (if (need-args? 2 0 0) (begin (print "test stat")) (print-help)))
	((timer) (if (need-args? 2 0 #f) (begin (print "test timer")) (print-help)))
	((stun) (if (need-args? 2 0 0) (begin (print "test stun")) (print-help)))
	(else (print-help)))

(display (list CLI_ARGS CLI_PARAMS))
(newline)


(map print (map bisy-parse-line '(
	"23 */2 * * * 1032 'Выполняется в 0:23, 2:23, 4:23 и т. д.'"
	"5 4 * * 2 1032 'Выполняется в 4:05 в воскресенье'"
	"* 0 1 1 * 1032 'С новым годом!'"
	"15 10,13 * * 1,4 1032 'Эта надпись выводится в понедельник и четверг в 10:15 и 13:15'"
	"0-59 * * */44 * 1032 'Выполняется ежеминутно'"
	"0-59/2 * 3,4,5,23 * * 1032 'Выполняется по чётным минутам'"
	"1-59/2 * * * * 1032 'Выполняется по нечётным минутам'"
)))

(exit)
;tests
(define rexs (map (lambda(k) (rex-match? "(\\d)(?<tst>\\d)?(\\d)?" k)) '("123" "12" "3")))
(print rexs)
(map print (map rex-list rexs))


;repl
(do ((Ln "REPL" (read))) ((eq? Ln #<eof>) (display "Bye..\n"))
	(write (eval Ln)) (display "\n&> "))

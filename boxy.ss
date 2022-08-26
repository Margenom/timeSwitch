;libs
(load "rex.ss")

;paths
(define (BoxyHome path) (string-append CLI_PATH "/" path))
(define (BizyHome path) (string-append (getenv "HOME") "/me/" path))

;functions
(define (print . strs) (map display strs) (newline))
(define (string-split str sep) (let rec((ost (string->list str)) (word '()) (out '()))
	(if (null? ost) (reverse out)
		(rec (cdr ost) (if (member (car ost) (string->list sep)) '() (cons (car ost) word)) 
			(if (member (car ost) (string->list sep)) (cons (list->string (reverse word)) out) out)))))
(define*(need-args? off required optional . params) (define leng (- (length CLI_ARGS) off))
	(print off 'r required 'o optional	)
	(and (apply and (map (lambda(p) (member p (map car CLI_PARAMS))) params)) (>= (+ (if optional optional leng) required) leng required)))
(define*(param-or-val pname val (modif values)) ((lambda(p) (modif (if p (cdr p) val))) (assoc pname CLI_PARAMS)))


(define (print-help) (with-input-from-file (BoxyHome "README.txt") (lambda()
	(do ((ln (read-line) (read-line))) ((equal? ln #<eof>)) (print ln)))))
(define (bisy-parse-line line) 
	(define*(asnum nums key ifnil (getter rex-sub)) ((lambda(p) (if (= 0 (string-length p)) 
		(if ifnil ifnil (print "bisy-parse-line asnum null")) (string->number p))) (getter nums key)))
	;generate rang correcter for type  of value
	(define (tester razr) (lambda(val) (let*(
			(upper (vector-ref #(59 23 31 12 7 1440) (- razr 1))) ;1440 - count minutes in 24 hours
			(lower (vector-ref #( 0  0  1  1 0    0) (- razr 1)))
			(ranged (min (max lower val) upper)))
		(unless (= val ranged) (print "in " line "\n\tunranged value: " val " (" lower "," upper "), " ranged  " used"))
		;week day 0-6 and 7 = 0
		((if (= 5 razr) (lambda(w) (modulo w 7)) values)  ranged))))

	(define iform "((?:(?:\\d\\d?,)*(?:\\d\\d?)|\\*|\\d\\d?-\\d\\d?)(?:/\\d\\d?)?)\\s+")
	;0 sur, 1-5 min hour day mounth weekday, 6 minutes, 7+ messg
	(define daln (rex-matchl? line "^" iform iform iform iform iform "(\\d+)\\s(.+)$"))
	(and daln (let rec((dls (rex-list-nums daln)) (out '()) (tm #f) (itr 0))
		(if (null? dls) (cdr (reverse out)) (begin (rec (cdr dls) (cons (cond
		;number list
			((set! tm (rex-match? "^((?:\\d\\d?,)*)(\\d\\d?)$" (car dls)))
				(let*((digstr (rex-sub tm 1)) (digend (list (rex-sub tm 2))) (digits (map string->number 
						(if (string=? "" digstr) digend (append (string-split digstr ",") digend)))))
					(lambda(now) (member now (map (tester itr) digits))))
		;number range
			) ((set! tm (rex-match? "^(\\d\\d?)-(\\d\\d?)(?:/(\\d\\d?))?$" (car dls)))
				(do ((b (min (asnum tm 1 0) (asnum tm 2 0)) (+ b 1)) 
						(inr '() (if (= 0 (modulo b (asnum tm 3 b))) (cons b inr) inr)))
					((> b (max (asnum tm 1 59) (asnum tm 2 59)))
						(lambda(now) (member now (map (tester itr) inr)))))
		;any number *
			) ((set! tm (rex-match? "^\\*(?:/(\\d\\d?))?$" (car dls))) (lambda(now) (= 0 (modulo now ((tester itr) (asnum tm 1 now))))) 
		;minutes
			) ((and (= itr 6) (set! tm (rex-match? "^\\d+$" (car dls)))) ((tester itr) (string->number (car dls)))
		;message
			) ((= itr 7) (car dls) 
			) (else #f)) out) tm (+ 1 itr)))))))

(define doeslog print)
(define doeslog-read read)

;globals
(define Bisy (with-input-from-file (BizyHome "bisy.set") (lambda() 
	(do ((line (read-line) (read-line)) (parsed #f (bisy-parse-line line)) 
			(out '() (if parsed (cons parsed out) out)))
		((equal? #<eof> line) (reverse out))))))

;main
(case (string->symbol (if (need-args? 1 1 #f) (cadr CLI_ARGS) "help"))
;planed
	((select) (if (need-args? 2 1 0) (begin (print "test sel")) (print-help)))
	((list) (if (need-args? 2 0 #f) (begin (print "test list")) (print-help)))
	((timer) (if (need-args? 2 0 #f) (begin (print "test timer")) (print-help)))
	((end) (if (need-args? 2 0 #f) (begin (print "test end")) (print-help)))
;unplaned
	((stun) (if (need-args? 2 0 0) (begin (print "test stun")) (print-help)))
	((tell) (if (need-args? 2 0 #f) (begin (print "test tell")) (print-help)))
	((wait) (if (need-args? 2 0 #f) (begin (print "test wait")) (print-help)))
;other
	((stat) (if (need-args? 2 0 0) (begin (print "test stat")) (print-help)))
	((check) (if (need-args? 2 0 0) (begin (print "test check")) (print-help)))
	((now) (if (need-args? 2 0 0) (begin (print "test now")) (print-help)))
	(else (print-help)))


(display (list CLI_ARGS CLI_PARAMS CLI_PATH CLI_EXEC)) (newline)
(map print Bisy)

(exit)
;repl
(do ((Ln "REPL" (read))) ((eq? Ln #<eof>) (display "Bye..\n"))
	(write (eval Ln)) (display "\n&> "))

;tests
(print "ln: " Bisy)
(map print (map bisy-parse-line '(
	"23 */2 * * * 1032 'Выполняется в 0:23, 2:23, 4:23 и т. д.'"
	"5 4 * * 2 1032 'Выполняется в 4:05 в воскресенье'"
	"* 0 1 1 * 1032 'С новым годом!'"
	"15 10,13 * * 1,4 1032 'Эта надпись выводится в понедельник и четверг в 10:15 и 13:15'"
	"0-59 * * */44 * 1032 'Выполняется ежеминутно'"
	"0-59/2 * 3,4,5,23 * * 1032 'Выполняется по чётным минутам'"
	"1-59/2 * * * * 1032 'Выполняется по нечётным минутам'"
)))

(define rexs (map (lambda(k) (rex-match? "(\\d)(?<tst>\\d)?(\\d)?" k)) '("123" "12" "3")))
(print rexs)
(map print (map rex-list rexs))

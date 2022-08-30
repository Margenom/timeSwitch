;libs
(load "rex.ss")

;constant
(define DoneLogFile ".donelog")
(define BusyFile "busy.set")

;paths
(define (BoxyHome path) (string-append CLI_PATH "/" path))
(define (BizyHome path) (string-append (getenv "HOME") "/me/" path))

;functions
(define (print . strs) (map display strs) (newline))
(define (string-split str sep) (let rec((ost (string->list str)) (word '()) (out '()))
	(if (null? ost) (reverse out)
		(rec (cdr ost) (if (member (car ost) (string->list sep)) '() (cons (car ost) word)) 
			(if (member (car ost) (string->list sep)) (cons (list->string (reverse word)) out) out)))))
(define (string-join sep . lst) 
	(do ((ost (cdr lst) (cdr ost)) (out (car lst) (string-append out sep (car ost)))) ((null? ost) out)))
;(fold (lambda(k v) (string-append v sep k)) (car lst) lst))
(define*(need-args? off required optional . params) (define leng (- (length CLI_ARGS) off))
	(print off 'r required 'o optional	)
	(and (apply and (map (lambda(p) (member p (map car CLI_PARAMS))) params)) (>= (+ (if optional optional leng) required) leng required)))
(define*(param-or-val pname val (modif values)) ((lambda(p) (modif (if p (cdr p) val))) (assoc pname CLI_PARAMS)))


(define (print-help) (with-input-from-file (BoxyHome "README.txt") (lambda()
	(do ((ln (read-line) (read-line))) ((equal? ln #<eof>)) (print ln)))))
(define (busy-parse-line line) 
	(define*(asnum nums key ifnil (getter rex-sub)) ((lambda(p) (if (= 0 (string-length p)) 
		(if ifnil ifnil (print "busy-parse-line asnum null")) (string->number p))) (getter nums key)))
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
(define*(donelog-parse-line line (partline #t))
	(define trms #f)
	(cond 
		((set! trms (rex-match? "^([\t<])(\\d+)\t(\\d+)\t(.+)$" line)) 
			(map (lambda (f v) (f v)) (list values string->number string->number values) 
				(cdr (rex-list-nums trms))))
		((set! trms (rex-match? "^(>)(\\d+)\t(.+)$" line)) 
			(map (lambda (f v) (f v)) (list values string->number values) (cdr (rex-list-nums trms))))
		((and partline (set! trms (rex-match? "^(\t)(\\d+)$" line))) 
			(list "p" (string->number (rex-sub tm 1))))
		(else #f)))
; head right
(define (donelog-record-agregate parsed)
	(let rec((ost parsed) (out '()) (waits '()) (head '()))
		(if (null? ost) (reverse out) (if (car ost) (case (string-ref (caar ost) 0)
				; calc length
			((#\tab) (rec (cdr ost) out (if (> 0 (- (caddar ost) (cadar ost))) waits 
				(cons (list (cadar ost) (- (caddar ost) (cadar ost))  (cdddar ost)) waits))  head) 
				; begin
			) ((#\<) (rec (cdr ost) out '() (cdar ost))
				; end
			) ((#\>) (rec (cdr ost) (cons (list head (reverse waits) (cdar ost)) out) '() #f)
			) (else (rec (cdr ost) out waits head))) (rec (cdr ost) out waits head)))))
; head left
(define (donelog-uncomplite parsed) (let rec((ost parsed) (out '()))
	(if (or (null? ost) (and (car ost) (string=? (caar ost) ">"))) out (rec (cdr ost) (cons (car ost) out))))) 
(define (donelog-record-length rec)
	(define rec_begin (caar rec))
	(define rec_end (caaddr rec))
	(define rec_waits_length (apply + (map cadr (cadr rec))))) 
(define (donelog-record-planed-diff rec) (- (donelog-record-length rec) (cadar rec)))
(define (donelog-last) (with-input-from-file (BusyHome DoesLogFile) (lambda()
	(do ((line (read-line) (read-line)) (last "" (donelog-parse-line line)))
		((equal? line #<eof>) last)))))
(define (donelog-load) (with-input-from-file (BusyHome DoesLogFile) (lambda()
	(do ((line (read-line) (read-line)) (parsed '() (cons (donelog-parse-line line #t) parsed)))
		((equal? line #<eof>) parsed))))) ;reversed and parsed



;globals
(define Busy (with-input-from-file (BizyHome BusyFile) (lambda() 
	(do ((line (read-line) (read-line)) (parsed #f (busy-parse-line line)) 
			(out '() (if parsed (cons parsed out) out)))
		((equal? #<eof> line) (reverse out))))))

;main
(define (args-check need) (unless (need-args? 2 (or need 0) (if need 0 #f)) (begin (print-help) (exit))))
(define (donelog-part-get-last) (list 345636534))
(define (donelog-append-begin busyrec) (print "<" (time-seconds) "\t" (list-ref busy 5) "\t" (list-ref busy 6)))
(define (donelog-append-end comment) (print ">" (time-seconds) "\t" comment))
(define (donelog-append-head) (display "\t") (display (time-seconds)))
(define (donelog-append-tail . mesg) (print "\t" (time-seconds) "\t" (apply string-join " " mesg)))
(define (busy-now-filter busy) busy)
(case (string->symbol (if (need-args? 1 1 #f) (cadr CLI_ARGS) "help"))
;planed
	((select) (args-check 1) 
		(donelog-append-begin (list-ref (busy-now-filter Busy) (string->number (list-ref CLI_ARGS 3)))) 
	)((list)  (args-check 0)
		(do ((i 0 (+ i 1)) (ost (busy-now-filter Busy) (cdr ost))) ((null? ost))
			(print i ":\t" (list-ref (car ost) 6)))
	)((timer) (args-check #f) ;system command
		(print (apply string-join " " (cddr CLI_ARGS)))
	)((end) (args-check #f) ;comment
;		(let ((time_begin (donelog-part-get-last)
		(print (apply string-join " " (cddr CLI_ARGS)))
;unplaned
	)((stun)(args-check 0) (donelog-append-head)
	)((tell)(args-check #f) ;mesg
		(donelog-append-tail (cddr CLI_ARGS))
	)((wait)(args-check #f) ;mesg
		(donelog-append-head)
		(read-line)
		(donelog-append-tail (cddr CLI_ARGS))
;other
	)((stat) (args-check 0) ;pattern match
		(print (donelog-uncomplite (map donelog-parse-line (donelog-load))))
	)((check) (args-check 0) 
	)((now) (args-check 0)
	)(else (print-help)))


(display (list CLI_ARGS CLI_PARAMS CLI_PATH CLI_EXEC)) (newline)

;	log file, record consist from:
;	- begin_utime
;	- planed_length copy from busy file but in sec
; - description also copy
;	- waits while waits will be split into parts like begin_utime and length.. no begin and end,name 
;		- begin_utime
;		<- length = end_utime - begin_utime
;		>- end_utime
;		- name
;	- end_utime
;	<- length = end_utime - begin_utime - (sum waits.end_utime - waits.begin_utime)
;	- comment
(define tdl (map donelog-parse-line '(
; begin :pass but ignore 
	"<3945348953	3434	какаето запись еаввлпво"
; use it
	"<3945348953	3434	какаето запись еаввлпво"
; correct=head+tail :pass
	"	3453634543	5748379863	какаето запись еаввлпво"
; correct+tail :pass
	"	3453634543	5748379863	какаето запись еаввлпво	5748379863	какаето запись еаввлпво"
; head+correct :pass
	"	3453634543	3453634543 5748379863	какаето запись еаввлпво"
; tail :skip
	"	5748379863	какаето запись еаввлпво"
; head+end+tail :skip
	"	3453634543 >3945348953	1434	какаето запиslfkсь еаввлпво	5748379863	какаето запись еаввлпво"
; correct :pass 
	"	3455930853	3453459863	какdsfjаето запись еаввлпво"
; end+tail :pass 
	">3945348953	1434	какаето запиslfkсь еаввлпво	5748379863	какаето запись еаввлпво"
; begin :pass  but ignore 
	"<4536747453	0434	какаето запись sldfjеаввлпво"
; head+begin+tail :skip
	"	3453634543<3945348953	4434	какаето запись еаввлпво	5748379863	какаето запись еаввлпво"
)))
(map print tdl)
(map print (donelog-record-agregate tdl))
(print (donelog-record-planed-diff (car (donelog-record-agregate tdl))))
(print (donelog-uncomplite (reverse tdl)))

(exit)
;repl
(do ((Ln "REPL" (read))) ((eq? Ln #<eof>) (display "Bye..\n"))
	(write (eval Ln)) (display "\n&> "))

;tests
(print "ln: " Busy)
;	busy file
(map print (map busy-parse-line '(
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

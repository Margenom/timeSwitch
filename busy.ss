(load "std.ss")

(define (mapi foo . lists) 
	(define (anynull? lst) (apply or (map null? lst)))
	(if (and (null? lists) (anynull? lists)) '() 
		(let rec((i 0) (ost lists)) (if (anynull? ost) '()
			(cons (apply foo i (map car ost)) (rec (+ i 1) (map cdr ost)))))))

(define (busy-load busy-file) 
(define (busy-parse-line line) 
	(define Limup	#(-1 59 23 31 12 7 1440 -1)) ;1440 - count minutes in 24 hours
	(define Limdw	#(-1  0  0  1  1 0    0 -1))

	(define*(asnum nums key ifnil (getter rex-sub)) ((lambda(p) (if (= 0 (string-length p)) 
		(if ifnil ifnil (print "busy-parse-line asnum null")) (string->number p))) (getter nums key)))

	(define iform "((?:(?:\\d\\d?,)*(?:\\d\\d?)|\\*|\\d\\d?-\\d\\d?)(?:/\\d\\d?)?)\\s+")
	;0 sur, 1-5 min hour day mounth weekday, 6 minutes, 7+ messg
	(define daln (rex-matchl? line "^" iform iform iform iform iform "(\\d+)\\s(.+)$"))
	(and daln (let rec((dls (rex-list-nums daln)) (out '()) (tm #f) (itr 0))
		(if (null? dls) out (let*( 
			(upper (vector-ref Limup itr)) 
			(lower (vector-ref Limdw itr))
			;generate rang correcter for type  of value
			(ranged (lambda(val) (min (max lower val) upper)))
			(tester (lambda(val)
				(unless (= val (ranged val)) 
					(print "in " line "\n\tunranged value: " val " (" lower "," upper "), " (ranged val)  " used"))
				;week day 0-6 and 7 = 0
				(if (= 5 itr) (modulo (ranged val) 7) (ranged val))))
			(value (cond
		;minutes
			((and (= itr 6) (set! tm (rex-match? "^\\d+$" (car dls)))) (tester (string->number (car dls)))
		;message
			) ((= itr 7) (car dls) 
		;number list
			) ((set! tm (rex-match? "^((?:\\d\\d?,)*)(\\d\\d?)$" (car dls)))
				(let*((digstr (rex-sub tm 1)) (digend (list (rex-sub tm 2))) (digits (map string->number 
						(if (string=? "" digstr) digend (append (string-split digstr ",") digend)))))
					(map tester digits))
		;number range
			) ((set! tm (rex-match? "^(\\d\\d?)-(\\d\\d?)(?:/(\\d\\d?))?$" (car dls)))
				(do ((b (min (asnum tm 1 0) (asnum tm 2 0)) (+ b 1)) 
						(inr '() (if (= 0 (modulo b (asnum tm 3 b))) (cons b inr) inr)))
					((> b (max (asnum tm 1 59) (asnum tm 2 59)))
						(map tester inr)))
		;any number *
			) ((set! tm (rex-match? "^\\*(?:/(\\d\\d?))?$" (car dls))) 
				(do ((i lower (+ 1 i)) (out '() (if (= 0 (modulo i (tester (asnum tm 1 i)))) (cons i out) out)))
					((> i upper) out))
			) (else #f)))
		) (rec (cdr dls) (cons value out) tm (+ 1 itr)))))))
	; add id
	(mapi (lambda(i b) (cdr (reverse (cons i b))))
		(filter values (map busy-parse-line (read-lines busy-file)))))

(define (busy-id busy) (list-ref busy 7))
(define (busy-name busy) (list-ref busy 6))
(define (busy-length busy) (list-ref busy 5))
(define (busy-time busy) busy)

(define (busy-week busy) (list-ref busy 4))
(define (busy-month busy) (list-ref busy 3))
(define (busy-day busy) (list-ref busy 2))
(define (busy-hour busy) (list-ref busy 1))
(define (busy-min busy) (list-ref busy 0))

(define (busy-clock now) (map (lambda(i) (vector-ref (clock now) i)) '(1 2 3 4 6)))

; brud forse
(define (busy-now? busy now) (apply and (map list? (map member (busy-clock now) (busy-time busy)))))

;	busy file
(define (test)
(map print (map busy-parse-line '(
	"23 */2 * * * 1032 'Выполняется в 0:23, 2:23, 4:23 и т. д.'"
	"5 4 * * 2 1032 'Выполняется в 4:05 в воскресенье'"
	"* 0 1 1 * 1032 'С новым годом!'"
	"15 10,13 * * 1,4 1032 'Эта надпись выводится в понедельник и четверг в 10:15 и 13:15'"
	"0-59 * * */44 * 1032 'Выполняется ежеминутно'"
	"0-59/2 * 3,4,5,23 * * 1032 'Выполняется по чётным минутам'"
	"1-59/2 * * * * 1032 'Выполняется по нечётным минутам'"
))))

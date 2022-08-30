;libs
(load "rex.ss")

;subs
(load "std.ss")
(load "busy.ss")
(load "donelog.ss")

;paths
(define (BoxyHome path) (string-append CLI_PATH "/" path))
(define (BusyHome path) (string-append (getenv "HOME") "/me/" path))
(define DoneLogFile (BusyHome ".donelog"))
(define BusyFile (BusyHome "busy.set"))

;globals
(define Busy (filter values (map busy-parse-line (read-lines BusyFile))))
;(map print Busy)

(define (print-help) (with-input-from-file (BoxyHome "README.txt") (lambda()
	(do ((ln (read-line) (read-line))) ((equal? ln #<eof>)) (print ln)))))

;main
(define (args-check need) (unless (need-args? 2 (or need 0) (if need 0 #f)) (begin (print-help) (exit))))
(define (donelog-part-get-last) (list 345636534))
(define (donelog-append-begin busy) (print "<" (time-seconds) "\t" (* 60 (list-ref busy 5)) "\t" (list-ref busy 6)))
(define (donelog-append-end comment) (print ">" (time-seconds) "\t" comment))
(define (donelog-append-head) (display "\t") (display (time-seconds)))
(define (donelog-append-tail . mesg) (print "\t" (time-seconds) "\t" (apply string-join " " mesg)))
(define (busy-now-filter busy) busy)

(display (list CLI_ARGS CLI_PARAMS CLI_PATH CLI_EXEC)) (newline)
(case (string->symbol (if (need-args? 1 1 #f) (cadr CLI_ARGS) "help"))
;planed
	((select) (args-check 1) 
		(donelog-append-begin (list-ref (busy-now-filter Busy) (string->number (list-ref CLI_ARGS 2)))) 
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

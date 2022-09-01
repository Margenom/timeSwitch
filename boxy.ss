(set! *load-path* (cons CLI_PATH *load-path*))

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

(define (print-help) (with-input-from-file (BoxyHome "README.txt") (lambda()
	(do ((ln (read-line) (read-line))) ((equal? ln #<eof>)) (print ln)))))

;main
(define (args-check need) (unless (need-args? 2 (or need 0) (if need 0 #f)) (begin (print-help) (exit))))
(define (part-check) (if (donelog-part-check? DoneLogFile) (begin (print "boxy stuned") (exit))))
(define*(proc-check (type values) (mesg "boxy in procesed")) (if (type (donelog-proc-check? DoneLogFile)) (begin (print mesg) (exit))))
(case (string->symbol (if (need-args? 1 1 #f) (cadr CLI_ARGS) "help"))
;planed
	((select) (args-check 1) (part-check) (proc-check)
		(donelog-append-begin DoneLogFile (list-ref Busy (string->number (list-ref CLI_ARGS 2)))) 
	)((list)  (args-check 0) (do ((i 0 (+ i 1)) (ost Busy (cdr ost))) ((null? ost))
			(print i ":\t" (list-ref (car ost) 5) "\t" (list-ref (car ost) 6)))
	)((timer) (part-check) (proc-check) (args-check #f) ;system command
		(print (apply string-join " " (cddr CLI_ARGS)))
	)((end) (proc-check not "boxy no started") (part-check) (args-check #f) ;comment
		(donelog-append-end DoneLogFile (apply string-join " " (cddr CLI_ARGS)) )
	)((now) (args-check 0) (print (donelog-uncompited (donelog-load DoneLogFile #t)))
	)((check) (args-check 0) 
;unplaned
	)((stun) (part-check) (args-check 0) (donelog-append-head DoneLogFile)
	)((tell) (args-check #f) (donelog-append-tail DoneLogFile (apply string-join " " (cddr CLI_ARGS)))
	)((wait) (part-check) (args-check #f) ;mesg
		(display "Stop C-c, in args or mesg: ")
		(let*((start (param-or-val "t" (clock-seconds) string->number))
				(inpstring (param-or-val "r" (string-chomp (read-line))))
				(stop (param-or-val "l" (clock-seconds) (lambda(v) (+ start (string->number v)))))
				(clistring (if (null? (cddr CLI_ARGS)) inpstring (apply string-join " " (cddr CLI_ARGS)))))
		(if (string=? "" clistring) (begin (print "no mesg") (exit)) (begin
			(donelog-append-head DoneLogFile start)
			(donelog-append-tail DoneLogFile (if (string=? "" inpstring) inpstring clistring) stop))))
;other
	)((stat) (args-check 0) ;pattern match
		;(print (donelog-uncomplite (map donelog-parse-line (donelog-load DoneLogFile))))
		(donelog-pretty-print (donelog-agregate (donelog-load DoneLogFile #t) #t #t))
	)(else (print-help)))

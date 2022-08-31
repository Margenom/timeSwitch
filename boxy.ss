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

;fakes
;(define time (clock-seconds)) (define tm (clock time)) (print (list time tm (clock-format "%d%M" tm)))
;(with-output-to-file-append "/tmp/test.t" (lambda()(print 453 4  5 4 3 36 'sndfl )))

;(map print Busy)
(display (list CLI_ARGS CLI_PARAMS CLI_PATH CLI_EXEC)) (newline)
(define (print-help) (with-input-from-file (BoxyHome "README.txt") (lambda()
	(do ((ln (read-line) (read-line))) ((equal? ln #<eof>)) (print ln)))))

;main
(define (args-check need) (unless (need-args? 2 (or need 0) (if need 0 #f)) (begin (print-help) (exit))))
(case (string->symbol (if (need-args? 1 1 #f) (cadr CLI_ARGS) "help"))
;planed
	((select) (args-check 1) 
		(donelog-append-begin DoneLogFile (list-ref Busy (string->number (list-ref CLI_ARGS 2)))) 
	)((list)  (args-check 0)
		(do ((i 0 (+ i 1)) (ost Busy (cdr ost))) ((null? ost))
			(print i ":\t" (list-ref (car ost) 5) "\t" (list-ref (car ost) 6)))
	)((timer) (args-check #f) ;system command
		(print (apply string-join " " (cddr CLI_ARGS)))
	)((end) (args-check #f) ;comment
;		(let ((time_begin (donelog-part-get-last)
		(donelog-append-end DoneLogFile (apply string-join " " (cddr CLI_ARGS)))
;unplaned
	)((stun)(args-check 0) (donelog-append-head DoneLogFile)
	)((tell)(args-check #f) ;mesg
		(donelog-append-tail DoneLogFile (apply string-join " " (cddr CLI_ARGS)))
	)((wait)(args-check #f) ;mesg
		(donelog-append-head DoneLogFile)
		(read)
		(donelog-append-tail DoneLogFile (apply string-join " " (cddr CLI_ARGS)))
;other
	)((stat) (args-check 0) ;pattern match
		;(print (donelog-uncomplite (map donelog-parse-line (donelog-load DoneLogFile))))
		(donelog-pretty-print (donelog-agregate (donelog-load DoneLogFile) #t #t #t))
	)((check) (args-check 0) 
	)((now) (args-check 0)
	)(else (print-help)))

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

(define (Ago-filter) 
	(define Secago (apply + (map (lambda(mn name) (* mn (param-or-val name 0 string->number)))
		'(1 60 3600 86400 604800 1814400) '("ts" "tm" "th" "td" "tw" "tc"))))
	((if (= 0 Secago) values (lambda(agrs) (dlg-time-filter agrs Secago))) 
		(dlg-agregate (dlg-load DoneLogFile #t) #t #t)))
(define (print-help) (with-input-from-file (BoxyHome "README.txt") (lambda()
	(do ((ln (read-line) (read-line))) ((equal? ln #<eof>)) (print ln)))))

;main
(define (args-check need) (unless (need-args? 2 (or need 0) (if need 0 #f)) (begin (print-help) (exit))))
(define (part-check) (if (dlg-part-check? DoneLogFile) (begin (print "boxy stuned") (exit))))
(define*(proc-check (type values) (mesg "boxy in procesed")) (if (type (dlg-proc-check? DoneLogFile)) (begin (print mesg) (exit))))
(case (string->symbol (if (need-args? 1 1 #f) (cadr CLI_ARGS) "help"))
;planed
	((select) (args-check 1) (part-check) (proc-check)
		(dlg-app-begin DoneLogFile (list-ref Busy (string->number (list-ref CLI_ARGS 2)))) 
	)((list)  (args-check #f) (do ((i 0 (+ i 1)) (ost Busy (cdr ost))) ((null? ost))
			(if (or (null? (cddr CLI_ARGS)) (apply and (map (lambda(r) (list? (rex-match? r (list-ref (car ost) 6)))) (cddr CLI_ARGS))))
				(print i ":\t" (list-ref (car ost) 5) "\t" (list-ref (car ost) 6))))
	)((timer) (part-check) (proc-check) (args-check #f) ;system command
		(print (apply string-join " " (cddr CLI_ARGS)))
	)((end) (proc-check not "boxy no started") (part-check) (args-check #f) ;comment
		(dlg-app-end DoneLogFile (apply string-join " " (cddr CLI_ARGS)) )
	)((now) (args-check 0) (print (dlg-uncompited (dlg-load DoneLogFile #t)))
	)((check) (args-check 0) 
;unplaned
	)((stun) (part-check) (args-check 0) (dlg-app-head DoneLogFile)
	)((tell) (args-check #f) (dlg-app-tail DoneLogFile (if (param-or-val "g" #f) (gui-input-mesg "Tell me what you do") (apply string-join " " (cddr CLI_ARGS))))
	)((next) (args-check #f) 
		(let ((last (dlg-last-done DoneLogFile "\t")))
			(unless last (begin (print "last is no done") (exit))) (let ((beg (caddr last)) (end (clock-seconds))
				(mesg (if (param-or-val "g" #f) (gui-input-mesg "Tell me what you do") (apply string-join " " (cddr CLI_ARGS)))))
			(unless mesg (begin (print "aborting") (exit)))
			(dlg-app-head DoneLogFile beg)
			(dlg-app-tail DoneLogFile mesg end)
			(if (param-or-val "g" #t)  (notify "Last legth" (string-append (number->string (round (/ (- end beg) 60.))) "min")))
			(if (param-or-val "noshow" #t (lambda(v) #f)) (print "Length: " (/(- end beg) 60.) "'"))))
	)((wait) (part-check) (args-check #f) ;mesg
		(unless (param-or-val "g" #f) (display "Stop C-c, in args or mesg: "))
		(let*((start (clock-seconds))
				(inpstring (if (param-or-val "g" #f) (gui-input-mesg "Tell me what you do") (string-chomp (read-line))))
				(stop (clock-seconds))
				(clistring (if (null? (cddr CLI_ARGS)) inpstring (apply string-join " " (cddr CLI_ARGS)))))
		(if (string=? "" clistring) (begin (print "no mesg") (exit)) (begin
			(dlg-app-head DoneLogFile start)
			(dlg-app-tail DoneLogFile (if (string=? "" inpstring) inpstring clistring) stop))))
;other
	)((showall) (args-check 0) (dlg-pretty-print (Ago-filter))
	)((stat) (args-check 0) (dlg-pretty-print (Ago-filter))
	)((parts) (args-check #f) (let ((paterns (cddr CLI_ARGS)))
		(if (null? paterns) (begin (print "No paterns") (exit))) (let*(
				(groups (map cdr (filter list? (dlg-grep paterns (Ago-filter)))))
				(partical (dlg-patical-length groups)))
		(map (lambda(p t) (print p "\t" (round (* 100 t)) "%")) paterns partical)
		(print "Total(all groups) time : " (round (/ (apply + (map (lambda(g) (apply + (map dlgd-gen-length g))) groups)) 3600.)) " ч.")))
		;what sould be here
	)(else (print-help)))

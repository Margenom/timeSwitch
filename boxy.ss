(set! *load-path* (cons CLI_PATH *load-path*))

;libs
(load "rex.ss")

;subs
(load "std.ss")
(load "busy.ss")
(load "donelog.ss")

(define (delay sec) (system (string-append "sleep " (number->string sec))))

;paths
(define (BoxyHome path) (string-append CLI_PATH "/" path))
(define BusyHome (let*(
		(closedir (getenv "MYCLOSEDIR"))
		(busyhome (if (string=? "" closedir) (getenv "HOME") closedir)))
	(lambda(path) (string-append busyhome "/" path))))
(define DoneLogFile (BusyHome ".donelog"))
(define Busy (busy-load (BusyHome "busy.set")))

(define*(Ago-filter (deftime 0))
	(define periods '(("ts" . 1) ("tm" . 60) ("th" . 3600) ("td" . 86400) ("tw" . 604800) ("tc" . 1814400)))
	(let*((test #f)
			(beenor (lambda (str)(set! test #t) (string->number str)))
			(argval (map (lambda(mrk)  (param-or-val mrk 0 beenor)) (map car periods)))
			(Secago (if test (apply + (map * argval (map cdr periods))) deftime)))
		((if (= 0 Secago) values (lambda(agrs) (dlg-time-filter agrs Secago)))
			(dlg-agregate (dlg-load DoneLogFile #t) #t #t))))
(define (print-help) (with-input-from-file (BoxyHome "README.txt") (lambda()
	(do ((ln (read-line) (read-line))) ((equal? ln #<eof>)) (print ln)))))

;main
(define (args-check need) 
	(unless (need-args? 2 (or need 0) (if need 0 #f)) (begin (print-help) (exit))))
(define*(part-check (type values) (mesg "boxy stuned")) 
	(if (type (dlg-part-check? DoneLogFile)) (begin (print mesg) (exit))))
(define*(proc-check (type values) (mesg "boxy in procesed"))
	(if (type (dlgd-proc-check? DoneLogFile)) (begin (print mesg) (exit))))
(define (main) (case (string->symbol (if (need-args? 1 1 #f) (cadr CLI_ARGS) "help"))
;planed
	((select) (args-check 1) (part-check) (proc-check) (let (
			(busy (list-ref Busy (string->number (list-ref CLI_ARGS 2)))))
		(dlg-app-begin DoneLogFile busy )
		(if (param-or-val "m" #f)  (notify "Select busy" (busy-name busy))))
	)((list)  (args-check #f) (map (lambda(b)
		(if (or (null? (cddr CLI_ARGS)) (apply and 
			(map (lambda(r) (list? (rex-match? r (busy-name b)))) cldata))) (busy-print b))) Busy)
	)((timer) (part-check) (proc-check) (need-args? 1 1 -1) (let (
			(busy (list-ref Busy (string->number (list-ref CLI_ARGS 2)))))
		(dlg-app-begin DoneLogFile busy)
		(if (param-or-val "m" #f)  (notify "Select busy" (busy-name busy)))
		(delay (* 60 (busy-length busy)))
		(unless (param-or-val "m" #f)  (notify "Time is out" 
			(string-append (busy-name busy) "\n" (number->string (busy-length busy)) "min")))
		(print (apply string-join " " (cddr CLI_ARGS))))
	)((end) (proc-check not "boxy no started") (part-check) (args-check #f) ;comment
		(dlg-app-end DoneLogFile (apply string-join " " (cddr CLI_ARGS)) )
	)((now) (args-check 0) (map (lambda(b) (if (busy-now? b (clock-seconds)) (busy-print b))) Busy)
	)((check) (args-check 0) (let ((musk (make-list 5 -1)) (time (busy-clock (clock-seconds))))
		(case (string->symbol ((lambda(a) (if (null? a) "undef" (car a))) (cddr CLI_ARGS)))
		((mouth) (map print (filter (lambda(b) 
			(busy-parttime? b (list-patch musk (cons 3 (busy-month time))))) Busy)))
		(else (let ((busyes (filter (lambda(b) (busy-parttime? b 
				(list-patch time '(0 . -1) '(1 . -1)))) Busy)))
			(print-ranges	(map busy-hour busyes) (busy-hour busy-limits) (map busy-id busyes))
			(print "Total length:\t" (apply + (map busy-length busyes)) "' (from 1440')\n")
			(map busy-print busyes)))))
	)((cron) (args-check 0) (map (lambda(b)
		(if (busy-now? b (clock-seconds)) (notify "Task" (busy-name b)))) Busy)
		(delay 60) (main)
;unplaned
	)((proc) (args-check 2) (part-check) (proc-check) (let (
			(busy (list -1 -1 -1 -1 -1 (string->number (list-ref CLI_ARGS 2)) (string-join (cdddr CLI_ARGS)) -1)))
		(dlg-app-begin DoneLogFile busy))
	)((stun) (part-check) (args-check 0) (dlg-app-head DoneLogFile)
	)((tell) (part-check not "boxy unstaned") (args-check #f) (let (
			(mesg (if (param-or-val "g" #f) 
				(gui-input-mesg "Tell me what you do") 
				(apply string-join " " (cddr CLI_ARGS)))))
		(unless mesg (exit))
		(dlg-app-tail DoneLogFile mesg))
	)((next) (args-check #f) (let ((last (dlg-last-done DoneLogFile "\t")))
			(unless last (begin (print "last is no done") (exit))) 
			(let ((beg (caddr last)) (end (clock-seconds)) (mesg (if (param-or-val "g" #f) 
					(gui-input-mesg "Tell me what you do") 
					(apply string-join " " (cddr CLI_ARGS)))))
				(unless mesg (begin (print "aborting") (exit)))
				(dlg-app-head DoneLogFile beg)
				(dlg-app-tail DoneLogFile mesg end)
			(if (param-or-val "m" #f)  (notify "Last legth" 
				(string-append (number->string (round (/ (- end beg) 60.))) "min")))
			(unless (param-or-val "noshow" #f) (print "Length: " (/(- end beg) 60.) "'"))))
	)((wait) (part-check) (args-check #f) ;mesg
		(unless (param-or-val "g" #f) (display "Stop C-c, in args or mesg: "))
		(let*((start (clock-seconds))
				(inpstring (if (param-or-val "g" #f) 
					(gui-input-mesg "Tell me what you do") 
					(string-chomp (read-line))))
				(stop (clock-seconds))
				(clistring (if (null? (cddr CLI_ARGS)) 
					inpstring 
					(apply string-join " " cldata))))
		(if (string=? "" clistring) (begin (print "no mesg") (exit)) (begin
			(dlg-app-head DoneLogFile start)
			(dlg-app-tail DoneLogFile (if (string=? "" inpstring) inpstring clistring) stop))))
;other
	)((showall) (args-check 0) (map dlgd-gen-print (Ago-filter))
	)((stat) (args-check 0) (map dlgd-gen-print (Ago-filter 604800))
	)((parts) (args-check #f) (let ((paterns (cddr CLI_ARGS)))
		(if (null? paterns) (begin (print "No paterns") (exit))) (let*(
				(total (Ago-filter 604800))
				(groups (map cdr (filter list? (dlg-grep paterns total #f))))
				(partical (dlg-patical-length groups)))
			(map (lambda(p t) (print p "\t" (round (* 100 t)) "%")) paterns partical)
			(print "Total(sum all groups) time: " (round (/ (apply + (map (lambda(g) (apply + (map dlgd-gen-length g))) groups)) 3600.)) " ч.")
			(print "Period length: " (round (/ (- (clock-seconds) (dlgd-gen-start (car total))) 3600.)) " ч.")))
		;what sould be here
	)(else (print-help))))
(main)

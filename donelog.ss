(load "std.ss")

;hum utils
(define (secs->mins secs) (round (/ secs 60.)))

; parser
(define*(dlg-parse-line line (partline #t))
	(define trms #f)
	(cond 
		((set! trms (rex-match? "^([\t<])(\\d+)\t(\\d+)\t(.+)$" line)) 
			(map (lambda (f v) (f v)) (list values string->number string->number values) 
				(cdr (rex-list-nums trms))))
		((set! trms (rex-match? "^(>)(\\d+)\t(.*)$" line)) 
			(map (lambda (f v) (f v)) (list values string->number values) (cdr (rex-list-nums trms))))
		((and partline (set! trms (rex-match? "^\t(\\d+)$" line))) 
			(list "p" (string->number (rex-sub trms 1))))
		(else #f)))
;read 
(define*(dlg-load DLfile (allow_partline #t)) 
	(map (lambda(l) (dlg-parse-line l allow_partline)) (read-lines DLfile)))

;;types - parsed file lines (first element is marker)
(define (dlg-type-remove typed) (cdr typed))
(define (dlg-type typed type) (and (string=? type (car typed)) (dlg-type-remove typed)))
(define (dlg-befoisnoagr-check? DLfile type)
	(define lst (reverse (dlg-load DLfile #t)))
	(and (list? lst) (car lst) (string=? (caar lst) type)))
;begin - begin of deal (consist from: start, planed length, description)
(define (dlgt-begin-start beg) (car beg))
(define (dlgt-begin-planed beg) (cadr beg))
(define (dlgt-begin-descr beg) (caddr beg))
(define (dlgt-begin-hum beg) ; pretty time, minutes
	(list (clock-pretty (dlgt-begin-start beg)) (/ (dlgt-begin-planed beg) 60.) (dlgt-begin-descr beg)))
; write
(define*(dlg-app-begin DLfile busy (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() 
		(print "<" now "\t" (* 60 (list-ref busy 5)) "\t" (list-ref busy 6)))))
;end - end of deal (cf: stop, comment)
(define (dlgt-end-stop end) (car end))
(define (dlgt-end-comment end) (cadr end))
(define (dlgt-end-hum end) ; pretty time, minute
	(list (clock-pretty (dlgt-end-start end)) (dlgt-end-comment end)))
; write
(define*(dlg-app-end DLfile comment (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() (print ">" now "\t" comment))))
;part - uncomplited wait or head (cf: start)
(define (dlgt-part-start wait) (car wait))
(define (dlgt-wait-hum part) (list (clock-pretty (dlgt-part-start wait))))
(define (dlg-part-check? DLfile) (dlg-befoisnoagr-check? DLfile "p"))
; write
(define*(dlg-app-head DLfile (now (clock-seconds))) (with-output-to-file-append DLfile (lambda()
	(map display (list "\t" now)))))
;wait - pause in deal (cf: head(start), tail(stop, mesg))
(define (dlgt-wait-start wait) (car wait))
(define (dlgt-wait-end wait) (cadr wait))
(define (dlgt-wait-length wait) (- (cadr wait) (car wait)))
(define (dlgt-wait-mesg wait) (caddr wait))
(define (dlgt-wait-hum wait) ; pretty time, minutes
	(list (clock-pretty (dlgt-wait-start wait)) (secs->mins (dlgt-wait-length wait)) (dlgt-wait-mesg wait)))
; write
(define*(dlg-app-tail DLfile mesg (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() (print "\t" now "\t" mesg))))

; agregate parsed lines into records or part (uncomplited) or free (unplaned and unhead)
(define*(dlg-agregate parsed (allow_free #f) (allow_processed #f))
	(let rec((ost parsed) (out '()) (waits '()) (head #f))
		(if (null? ost) (reverse (if (and allow_processed head) 
				(cons (list 'proc head (reverse waits)) out) out)) ; like processing
			(if (car ost) (let*(
					(mark (string-ref (caar ost) 0))
					(unt (dlg-type-remove (car ost))))
				(case mark
				; wait
					((#\tab) (rec (cdr ost) 
						(if (and allow_free (not head)) (cons (cons 'free unt) out) out) 
						(if (> 0 (dlgt-wait-length unt)) waits (cons unt  waits)) head)
				; part
				) ((#\p) (rec (cdr ost) 
					(if (and allow_free (not head)) (cons (list 'part (dlgt-part-start unt)) out) out)
						(cons (list (dlgt-part-start unt)) waits) head)
				; begin
				) ((#\<) (rec (cdr ost) 
					(if (and allow_processed head) (cons (list 'proc head (reverse waits)) out) out) '() unt)
				; end
				) ((#\>) (rec (cdr ost) (cons (list 'rec head (reverse waits) unt) out) '() #f)
				) (else (rec (cdr ost) out waits head))))
			(rec (cdr ost) out waits head)))))
;;dones - agregated and parsed lines
(define (dlg-befois-check? DLfile label) (equal? (caar (reverse (dlg-agregate (dlg-load DLfile #t) #t #t))) label))
;records - complited deal (cf: begin, (list of waits), end)
(define (dlgd-rec? done) (and (eq? (car done) 'rec) done))
(define (dlgd-rec-begin done) (cadr done))
(define (dlgd-rec-waits done) (caddr done))
(define (dlgd-rec-end done) (cadddr done))
(define (dlgd-rec-length done) 
	(- (dlgt-end-stop (dlgd-rec-end done)) (dlgt-begin-start (dlgd-rec-begin done)) 
		(apply + (map dlgt-wait-length (dlgd-rec-waits done)))))
(define (dlgd-rec-planed-diff done) 
	(- (dlgd-rec-length done) (dlgt-begin-planed (dlgd-rec-begin done))))
(define (dlgd-rec-hum done)
	(define beg (dlgd-rec-begin done))
	(define end (dlgd-rec-end done))
	(define waits (dlgd-rec-waits done))
	(print (clock-pretty (dlgt-begin-start beg)) "\t" (secs->mins (dlgd-rec-planed-diff done)) "'\t" (dlgt-begin-descr beg))
	(print 	(secs->mins (- (dlgt-end-stop end) 
	(let rec((w waits) (ltime (dlgt-begin-start beg)))
		(if (null? w) ltime (begin 
			(print "\t" (secs->mins (- (dlgt-wait-start (car w)) ltime)) "'\t" (secs->mins (dlgt-wait-length (car w))) "'\t" (dlgt-wait-mesg (car w)))
			(rec (cdr w) (dlgt-wait-end (car w))))))
		)) "'\t" (secs->mins (dlgd-rec-length done)) "'\t" (dlgt-end-comment end)))

;proc - deal in process, record without end
(define (dlgd-proc-check? DLfile) (dlg-befois-check? DLfile 'proc))
(define (dlgd-proc-begin done) (cadr done))
(define (dlgd-proc-waits done) (caddr done)) ;may be exists part
(define (dlgd-wait-check? wait) (= 3 (length wait)))
(define (dlgd-part-check? wait) (= 1 (length wait)))
(define*(dlgd-proc-length done (now (clock-seconds))) (- now (dlgt-begin-start (dlgd-proc-begin done)) 
	(apply + (map (lambda(w) (if (dlgd-wait-check? w) (dlgt-wait-length w) 0)) (dlgd-proc-waits done)))))
(define*(dlgd-proc-planed-diff done (now (clock-seconds))) 
	(- (dlgd-proc-length done now) (dlgt-begin-planed (dlgd-proc-begin done))))
(define*(dlgd-proc-hum done (now (clock-seconds)))
	(define beg (dlgd-proc-begin done))
	(define waits (dlgd-proc-waits done))
	(print (clock-pretty (dlgt-begin-start beg)) "\t" (secs->mins (dlgd-proc-planed-diff done now)) "'\t" (dlgt-begin-descr beg))
	(print 	(secs->mins (- now 
	(let rec((w waits) (ltime (dlgt-begin-start beg)))
		(if (null? w) ltime (begin 
			(map display (list "\t" (secs->mins (- (dlgt-wait-start (car w)) ltime)) "'\t"))
			(if (dlgd-part-check? (car w)) (begin (newline) (rec (cdr w) ltime)) (begin
				(print (secs->mins (dlgt-wait-length (car w))) "'\t" (dlgt-wait-mesg (car w)))
				(rec (cdr w) (dlgt-wait-end (car w))))))))
		)) "'\t" (secs->mins (dlgd-proc-length done now)) "'"))
;free - wait un deal 
(define (dlgd-free->wait done) (and (eq? (car done) 'free) (cdr done)))

;spec
(define*(dlg-last-done DLfile (type #f) (full #f)) (define lst (reverse (dlg-load DLfile #t)))
	(and (list? lst) (car lst) (or (not type) (string=? (caar lst) type)) (if full lst (car lst))))
(define (dlg-pretty-print agrd)
	(if (null? agrd) agrd (let ((agr (car agrd))) (case (car agr)
			((free) (map (lambda (v e) (display v)(display e)) 
				(dlgt-wait-hum (dlgd-free->wait agr)) '("\t" "'\t" "\n")))
			((rec) (dlgd-rec-hum agr))
			((proc) (dlgd-proc-hum agr))
			(else (print agr)))
		(dlg-pretty-print (cdr agrd)))))
; any from end
(define (dlg-uncomplite parsed) (let rec((ost (reverse parsed)) (out '()))
	(if (or (null? ost) (and (car ost) (string=? (caar ost) ">"))) out (rec (cdr ost) (cons (car ost) out)))))

(define (test)
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
(define tdl (map dlg-parse-line '(
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
(define argtdl(dlg-agregate tdl))
(map print argtdl )
(print (dlg-record-planed-diff (car argtdl)))
(print (dlg-uncomplite (reverse tdl)))
)

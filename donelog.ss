(load "std.ss")

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
(define (dlg-befoisnoagr-check? DLfile type)
	(define lst (reverse (dlg-load DLfile #t)))
	(and (list? lst) (car lst) (string=? (caar lst) type)):)
;begin - begin of deal (consist from: start, planed length, description)
(define (dlgt-begin-start beg) (car beg))
(define (dlgt-begin-planed beg) (cadr beg))
(define (dlgt-begin-descr beg) (caddr beg))
; write
(define*(dlg-app-begin DLfile busy (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() 
		(print "<" now "\t" (* 60 (list-ref busy 5)) "\t" (list-ref busy 6)))))
;end - end of deal (cf: stop, comment)
(define (dlgt-end-stop end) (car end))
(define (dlgt-end-comment end) (cadr end))
; write
(define*(dlg-app-end DLfile comment (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() (print ">" now "\t" comment))))
;part - uncomplited wait or head (cf: start)
(define (dlgt-part-start wait) (car wait))
(define (dlg-part-check? DLfile) (dlg-befoisnoagr-check? DLfile "p"))
; write
(define*(dlg-app-head DLfile (now (clock-seconds))) (with-output-to-file-append DLfile (lambda()
	(map display (list "\t" now)))))
;wait - pause in deal (cf: head(start), tail(stop, mesg))
(define (dlgt-wait-start wait) (car wait))
(define (dlgt-wait-end wait) (cadr wait))
(define (dlgt-wait-length wait) (- (cadr wait) (car wait)))
(define (dlgt-wait-descr wait) (caddr wait))
(define (dlgt-wait->short wait) (list (dlgt-wait-start wait) (dlgt-wait-length wait) (dlgt-wait-descr wait)))
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
				; waits
					((#\tab) (rec (cdr ost) 
						(if (and allow_free (not head)) (cons (cons 'free (dlgt-wait->short unt)) out) out) 
						(if (> 0 (dlgt-wait-length unt)) waits (cons (dlgt-wait->short unt)  waits)) head)
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
(define (dlgdr-begin-start beg) (car beg))
(define (dlgdr-begin-planed beg) (cadr beg))
(define (dlgdr-begin-descr beg) (caddr beg))
(define (dlgd-rec-waits done) (caddr done))
; rec's wait like wait, but no have marker and end (use length)
(define (dlgdr-wait-start wait) (car wait))
(define (dlgdr-wait-length wait) (cadr wait))
(define (dlgdr-wait-descr wait) (caddr wait))
(define (dlgdr-wait-hum wait)
	(list (clock-pretty (dlgdr-wait-start wait)) (/ (dlgdr-wait-length wait) 60.) (dlgdr-wait-descr wait)))
(define (dlgd-rec-end done) (cadddr done))
(define (dlgdr-end-stop end) (car end))
(define (dlgdr-end-comment end) (cadr end))
(define (dlgd-rec-length done) 
	(- (dlgdr-end-stop (dlgd-rec-end done)) (dlgdr-beg-start (dlgd-rec-begin done)) 
		(apply + (map dlgdr-wait-length (dlgd-rec-waits done)))))
(define (dlgd-rec-planed-diff done) 
	(- (dlgd-rec-length done) (dlgdr-begin-planed (dlgd-rec-begin done))))
;proc - deal in process, record without end
(define (dlgd-proc-check? DLfile) (dlg-befois-check? DLfile 'proc))
(define (dlgd-proc-begin done) (cadr done))
(define (dlgd-proc-waits done) (caddr done)) ;may be exists part
(define (dlgd-wait-check? wait) (= 3 (length wait)))
(define (dlgd-part-check? wait) (= 1 (length wait)))
;(define*(dlgd-proc-length done (now (clock-seconds))) 
;	(- now (dlgdr-beg-start (dlgd-proc-begin done)) 
;		(apply + (map (lambda(w) (if (dlgd-wait-check? w) (dlgdr-wait-length  (dlgd-proc-waits done)))))
(define (dlgd-rec-planed-diff done) 
	(- (dlgd-rec-length done) (dlgdr-begin-planed (dlgd-rec-begin done))))
;free - wait un deal 
(define (dlgd-free->wait done) (and (eq? (car done) 'free) (cdr done)))

;spec
(define*(dlg-last-done DLfile (type #f) (full #f))
	(define lst (reverse (dlg-load DLfile #t)))
	(and (list? lst) (car lst) (or (not type) (string=? (caar lst) type)) (if full lst (car lst))))
(define (dlg-pretty-print agrd)
	(if (null? agrd) agrd (let ((agr (car agrd))) (case (car agr)
;			((free)	(print (apply string-join "\t" (dlgt-wait-hum ((dlgd-free->wait agr))))))
;			((rec) (let*((waits (dlgd-rec-waits agr))(beg (dlgd-rec-begin agr))(end (dlgd-rec-end agr))
;					(time (clock-pretty (dlgt-wait-start wait)))
;					(len (/ (dlgt-wait-length wait) 60.))
;					(descr (dlgt-wait-descr wait)))
;				(print time "\t" len "\t" descr))
			((proc) (print agr))
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

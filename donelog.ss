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

; agregate parsed lines into records or part (uncomplited) or free (unplaned and unhead)
(define*(dlg-agregate parsed (allow_free #f) (allow_processed #f))
	(let rec((ost parsed) (out '()) (waits '()) (head #f))
		(if (null? ost) (reverse (if (and allow_processed head) 
				(cons (list 'proc head (reverse waits)) out) out)) ; like processing
			(if (car ost) (case (string-ref (caar ost) 0)
				((#\tab) (let*((upl_length (- (caddar ost) (cadar ost))) (upl (list (cadar ost) upl_length  (car (cdddar ost)))))
				(rec (cdr ost) (if (and allow_free (not head)) (cons (cons 'free upl) out) out) (if (> 0 upl_length) waits (cons upl  waits)) head))
				; part
				)((#\p) (rec (cdr ost) (if (and allow_free (not head)) (cons (cons 'part (cdar ost)) out) out) (cons (cdar ost) waits) head)
				; begin
				) ((#\<) (rec (cdr ost) (if (and allow_processed head) 
					(cons (list 'proc head (reverse waits)) out)
					out) '() (cdar ost))
				; end
				) ((#\>) (rec (cdr ost) (cons (list 'rec head (reverse waits) (cdar ost)) out) '() #f)
				) (else (rec (cdr ost) out waits head))) 
			(rec (cdr ost) out waits head)))))

; any from end
(define (dlg-uncomplite parsed) (let rec((ost (reverse parsed)) (out '()))
	(if (or (null? ost) (and (car ost) (string=? (caar ost) ">"))) out (rec (cdr ost) (cons (car ost) out)))))

;io 
(define (dlg-pretty-print agrd)
	(if (null? agrd) agrd (let ((agr (car agrd))) (case (car agr)
			((rec) (print agr))
			((part) (print agr))
			((free) (print agr))
			((proc) (map print agr))
			(else (print agr)))
		(dlg-pretty-print (cdr agrd)))))
;load 
(define*(dlg-load DLfile (allow_partline #t)) 
	(map (lambda(l) (dlg-parse-line l allow_partline)) (read-lines DLfile)))
;write 
(define*(dlg-append-begin DLfile busy (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() 
		(print "<" now "\t" (* 60 (list-ref busy 5)) "\t" (list-ref busy 6)))))
(define*(dlg-append-head DLfile (now (clock-seconds))) (with-output-to-file-append DLfile (lambda()
	(map display (list "\t" now)))))
(define*(dlg-append-tail DLfile mesg (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() (print "\t" now "\t" mesg))))
(define*(dlg-append-end DLfile comment (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() (print ">" now "\t" comment))))
;spec
(define*(dlg-last-done DLfile (type #f) (full #f))
	(define lst (reverse (dlg-load DLfile #t)))
	(and (list? lst) (car lst) (or (not type) (string=? (caar lst) type)) (if full lst (car lst))))

;;types - parsed file lines (first element is marker)
(define (dlg-befoisnoagr-check? DLfile type)
	(define lst (reverse (dlg-load DLfile #t)))
	(and (list? lst) (car lst) (string=? (caar lst) type)):)
;begin - begin of deal (consist from: start, planed length, description)
(define (dlg-type-begin-start beg) (cadr beg))
(define (dlg-type-begin-planed beg) (caddr beg))
(define (dlg-type-begin-descr beg) (cadddr beg))
;end - end of deal (cf: stop, comment)
(define (dlg-type-end-stop end) (cadr end))
(define (dlg-type-end-comment end) (caddr end))
;wait - pause in deal (cf: start, stop, mesg)
(define (dlg-type-wait-start wait) (cadr wait))
(define (dlg-type-wait-end wait) (cadr wait))
(define (dlg-type-wait-length wait) (- (caddr wait) (cadr wait)))
(define (dlg-type-wait-descr wait) (cadddr wait))
;part - uncomplited wait (cf: start)
(define (dlg-type-part-start wait) (cadr wait))
(define (dlg-part-check? DLfile) (dlg-befoisnoagr-check? DLfile "p"))
;;dones - agregated and parsed lines
(define (dlg-befois-check? DLfile label) (equal? (caar (reverse (dlg-agregate (dlg-load DLfile #t) #t #t))) label))
;records - complited deal (cf: begin, (list of waits), end)
(define (dlg-record? done) (and (eq? (car done) 'rec) done))
(define (dlg-record-begin done) (cadr done))
(define (dlg-record-waits done) (caddr done))
(define (dlg-record-end done) (cadddr done))
(define (dlg-record-begin-start done) (cadr done))
(define (dlg-record-comment done) ( done))
(define (dlg-record-length done)
	(define rec (or (dlg-record record) record))
	(define rec_begin (caar rec))
	(define rec_end (caaddr rec))
	(define rec_waits_length (apply + (map cadr (cadr rec)))))
(define (dlg-record-planed-diff record) 
	(define rec (or (dlg-record record) record))
	(- (dlg-record-length record) (cadar rec)))
;proc - deal in process, record without end
(define (dlg-proc-check? DLfile) (dlg-befois-check? DLfile 'proc))
;free - wait un deal 
(define (dlg-free->wait done) (and (eq? (car done) 'free) (cdr done)))

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

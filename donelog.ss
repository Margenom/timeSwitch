(load "std.ss")

; parser
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

; (head right) agregate parsed lines into records or part (uncomplited) or free (unplaned and unhead)
(define*(donelog-agregate parsed (allow_free #f) (allow_partline #f))
	(let rec((ost parsed) (out '()) (waits '()) (head '()))
		(if (null? ost) (reverse out) (if (car ost) (case (string-ref (caar ost) 0)
			((#\tab) (let*((upl_length (- (caddar ost) (cadar ost))) (upl (list (cadar ost) upl_length  (cdddar ost))))
				(rec (cdr ost) (if (and allow_free (not head)) (cons (cons 'free upl) out) out) (if (> 0 upl_length) waits (cons upl  waits)) head))
				; begin
			) ((#\<) (rec (cdr ost) out '() (cdar ost))
				; end
			) ((#\>) (rec (cdr ost) (cons (list 'rec head (reverse waits) (cdar ost)) out) '() #f)
				; part
			) ((#\p) (rec (cdr ost) (if allow_partline (cons  (list 'part head (reverse (cons (cdar ost) waits)))out) out) waits head)
			) (else (rec (cdr ost) out waits head))) (rec (cdr ost) out waits head)))))

; (head left) any from end
(define (donelog-uncomplite parsed) (let rec((ost parsed) (out '()))
	(if (or (null? ost) (and (car ost) (string=? (caar ost) ">"))) out (rec (cdr ost) (cons (car ost) out)))))
;records
(define (donelog-record rec) (and (eq? (car rec) 'rec) (cdr rec)))
(define (donelog-record-length record)
	(define rec (or (donelog-record record) record))
	(define rec_begin (caar rec))
	(define rec_end (caaddr rec))
	(define rec_waits_length (apply + (map cadr (cadr rec)))))
(define (donelog-record-planed-diff record) 
	(define rec (or (donelog-record record) record))
	(- (donelog-record-length record) (cadar rec)))

;io 
(define*(donelog-load DLfile (allow_partline #t)) 
	(map (lambda(l) (donelog-parse-line l allow_partline)) (read-lines DLfile)))
(define*(donelog-append-begin DLfile busy (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() 
		(print "<" now "\t" (* 60 (list-ref busy 5)) "\t" (list-ref busy 6)))))
(define*(donelog-append-head DLfile (now (clock-seconds))) (with-output-to-file-append DLfile (lambda()
	(map display (list "\t"now)))))
(define*(donelog-append-teil DLfile mesg (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() (print "\t" now "\t" mesg))))
(define*(donelog-append-end DLfile comment (now (clock-seconds))) 
	(with-output-to-file-append DLfile (lambda() (print ">" now "\t" comment))))

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
(define argtdl(donelog-agregate tdl))
(map print argtdl )
(print (donelog-record-planed-diff (car argtdl)))
(print (donelog-uncomplite (reverse tdl)))
)

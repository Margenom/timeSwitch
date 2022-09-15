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
(define*(dlgd-rec-hum done)
	(define beg (dlgd-rec-begin done))
	(define end (dlgd-rec-end done))
	(define waits (dlgd-rec-waits done))
	(print (clock-pretty (dlgt-begin-start beg)) "\tp: " (secs->mins (dlgd-rec-planed-diff done)) "'\td: " (dlgt-begin-descr beg))
	(print "b: " (secs->mins (- (dlgt-end-stop end)
	(let rec((w waits) (ltime (dlgt-begin-start beg)))
		(if (null? w) ltime (begin
			(print "\tb: " (secs->mins (- (dlgt-wait-start (car w)) ltime)) "'\tl: " (secs->mins (dlgt-wait-length (car w))) "'\tc: " (dlgt-wait-mesg (car w)))
			(rec (cdr w) (dlgt-wait-end (car w))))))
		)) "' \tl: " (secs->mins (dlgd-rec-length done)) "'\tt: " (secs->mins (- (dlgt-end-stop end) (dlgt-begin-start beg))) "'\tm: " (dlgt-end-comment end)))

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
	(print (clock-pretty (dlgt-begin-start beg)) "\tp: " (secs->mins (dlgd-proc-planed-diff done now)) "'\td: " (dlgt-begin-descr beg))
	(print "b: "	(secs->mins (- now
	(let rec((w waits) (ltime (dlgt-begin-start beg)))
		(if (null? w) ltime (begin
			(map display (list "\tb: " (secs->mins (- (dlgt-wait-start (car w)) ltime)) "'\tl: "))
			(if (dlgd-part-check? (car w)) (begin (newline) (rec (cdr w) ltime)) (begin
				(print (secs->mins (dlgt-wait-length (car w))) "'\tc: " (dlgt-wait-mesg (car w)))
				(rec (cdr w) (dlgt-wait-end (car w))))))))
		)) "'\tl: " (secs->mins (dlgd-proc-length done now)) "'t: " (secs->mins (- now (dlgt-begin-start beg)))))
;free - wait un deal
(define (dlgd-free->wait done) (and (eq? (car done) 'free) (cdr done)))
;unification or generaly
(define (dlgd-make-gen-ret lrec lproc lfree lpart) (lambda(done) (case (car done)
	((rec) (lrec done)) ((proc) (lproc done)) ((free) (lfree done)) ((part) (lpart done)) (else #f))))
(define dlgd-gen-start (dlgd-make-gen-ret
	(lambda(done) (dlgt-begin-start (dlgd-rec-begin done)))
	(lambda(done) (dlgt-begin-start (dlgd-proc-begin done)))
	(lambda(done) (dlgt-wait-start (dlgd-free->wait done)))
	(lambda(done) (cadr done))))
(define dlgd-gen-length (dlgd-make-gen-ret
	dlgd-rec-length  dlgd-proc-length (lambda(done) (dlgt-wait-length (dlgd-free->wait done)))
	(lambda(done) (- (clock-seconds) (cadr done)))))
(define dlgd-gen-stop (dlgd-make-gen-ret
	(lambda(done) (dlgt-end-stop (dlgd-rec-end done)))
	(lambda(done) (clock-seconds))
	(lambda(done) (dlgt-wait-stop (dlgd-free->wait done)))
	(lambda(done) (clock-seconds))))
(define dlgd-gen-print (dlgd-make-gen-ret
	(lambda(done) (dlgd-rec-hum done))
	(lambda(done) (dlgd-proc-hum done))
	(lambda(done) (map (lambda (v e) (display v)(display e)) (dlgt-wait-hum (dlgd-free->wait done)) '("\tl: " "'\tc: " "\n")))
	(lambda(done) (print (clock-pretty (dlgd-gen-start done))  
		"'\tl: " (secs->mins (- (dlgd-gen-stop done) (dlgd-gen-start done))) "'"))))

;spec
(define*(dlgd-gen-match? patern done (allow_waits #f) (delim "\n"))
	(define dlgd-gen-context (dlgd-make-gen-ret
		(lambda(d) (string-append (dlgt-begin-descr (dlgd-rec-begin d)) (if allow_waits 
				(apply string-append delim (map (lambda(w) (values (dlgt-wait-mesg w) delim)) (dlgd-rec-waits d))) "|") 
			(dlgt-end-comment (dlgd-rec-end d))))
		(lambda(d) ((lambda(descr) (if allow_waits 
			(apply string-append descr (map (lambda(w) (values delim (dlgt-wait-mesg w))) (dlgd-proc-waits d)))
			descr)) (dlgt-begin-descr (dlgd-proc-begin d))))
		(lambda(d) (dlgt-wait-mesg (dlgd-free->wait d)))))
	(rex-match? patern (dlgd-gen-context done)))

; return named groups ((patern dones...) next...)
(define (dlg-grep paterns dones allow_waits) (map cons paterns (let rec((ost dones))
		(if (null? ost) (make-list (length paterns) '()) 
			(map (lambda(p t) (if (dlgd-gen-match? p (car ost) allow_waits) (cons (car ost) t) t)) 
					 paterns (rec (cdr ost)))))))
; require unnamed groups ((dones...) next...)
(define*(dlg-patical-length groups)
	(define gen-length-in-groups (map (lambda(group) (apply + (map dlgd-gen-length group))) groups))
	(define total-group-length (apply + gen-length-in-groups))
	(map (lambda(gl) (/ gl 1. total-group-length)) gen-length-in-groups))

(define*(dlg-time-filter agrd secondsago (now (clock-seconds))) 
	(filter (lambda(a) (> (dlgd-gen-start a) (- now secondsago))) agrd))

(define*(dlg-last-done DLfile (type #f) (full #f)) (define lst (reverse (dlg-load DLfile #t)))
	(and (list? lst) (car lst) (or (not type) (string=? (caar lst) type)) (if full lst (car lst))))

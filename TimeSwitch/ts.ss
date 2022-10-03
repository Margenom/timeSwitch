(import (sqlite3) (srfi s13 strings))

(define-values (CLI_ARGS CLI_PAMS)
	(let rec ((ost (command-line-arguments)) (args '()) (pams '()))
		(if (null? ost) (apply values (map reverse (list args pams))) (let* (
			(head (car ost))
			(type (char=? (string-ref head 0) #\-)))
		(rec (cdr ost) (if type args (cons head args)) 
			(if type (cons (let* (
				(delim (string-index head #\=))
				(name (string->symbol (substring head 1 (or delim (string-length head)))))
				(val (and delim (substring head delim (string-length head)))))
			(cons name val)) pams) pams))))))

(define (print . X) (let rec((x X)) (or (null? x) (begin (display (car x)) (rec (cdr x))))) (newline))
(define (pam pamname) (define pam-pair (assoc pamname CLI_PAMS)) (and pam-pair (cdr pam-pair)))

(define (clock-seconds) (time-second (current-time)))

(define CONFIG '())
(define (config-append name def-val description)
	(set! CONFIG (cons (list name def-val description) CONFIG)))

(config-append 'database #f "path for your time switch database")
;(config-append '
;(config-append '
;(config-append '

(define COMMANDS '())
(define (commands-append name args-min args-max doc comma) 
	(set! COMMANDS (cons (list name doc args-min args-max comma) COMMANDS)))
(define (commands-get name argc) (define cmd (assoc name COMMANDS))
	(and cmd (> (list-ref cmd 2) argc (list-ref cmd 3)) (list-ref cmd 4)))

(define (help-gen) 
	(print "Configns: ") (map (lambda(k) (print "\t-" (list-ref k 0) "=<" (list-ref k 2) ", def " (list-ref k 1) ">")) CONFIG) 
	(print "System commands:") (map (lambda(k) (print "\t" (list-ref k 1))) COMMANDS) 0)


(commands-append 'stat 0 0 "stat [-ago=<from how ago in sec>]" (lambda(Db)
	(map-row print Db "SELECT * FROM donelog ORDER BY begin DESC WHERE begin > ?;" 
		(- (clock-seconds) (or (pam ago) 604800)))))
(define (!parts Db . paterns) (print paterns))
(commands-append 'parts 1 -1 "parts <patern0> .. <paternN> [-ago=<from how ago in sec>]" !parts)

(define (!next Db . Mesg) (execute Db 
"INSERT donelog(begin, end, comment)
	SELECT end, strftime('%s'), ? 
	FROM donelog
	ORDER BY begin DESC
	WHERE end IS NOT NULL
	LIMIT 1;" (string-join Mesg " ")))
(commands-append 'next 1 -1 "next <namepart0> .. <namepartN> [-g gui message holder]" !next)
(define (!stun Db . _) (execute Db "INSERT donelog(begin) SELECT strftime('%s');"))
(commands-append 'stun 0 0 "stat" !stun)
(define (!tell Db . Mesg) (execute Db "INSERT donelog(end, comment) SELECT strftime('%s'), ?;" (string-join Mesg " ")))
(commands-append 'tell 1 -1 "tell <namepart0> .. <namepartN> [-g gui message holder]" !tell)

(define (!list Db . patterns) (map-row print Db "SELECT * FROM tasks;"))
(commands-append 'list 0 1 "list <pattern> [-now=<utime>]" !list)
(define (!select Db taskid . _) (execute Db "select 45;"))
(commands-append 'select 1 1 "select <task id>" !select)
(define (!end Db . Mesg) (execute Db "select 45;"))
(commands-append 'end 1 -1 "end [-t=<taskid>] <namepart0> .. <namepartN> [-now=<utime>]" !end)

(define (main)
	(define (alert trunk) (unless trunk (begin (print "help") (exit)))) (alert (= (length CLI_ARGS) 0))
	(define cmd (command-get (string->symbol (car CLI_ARGS)) (length (cdr CLI_ARGS)))) (alert cmd)
	(define DataBase (open-database (pam 'database)))
	(apply cmd DataBase (cdr CLI_ARGS)))

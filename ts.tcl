#!/usr/bin/tclsh
# Я понял зачем может пригодиться много задачность в таком деле и вот
# есть у меня план прочесть книгу за месяц и я могу отмечать что я чтото там читал но это всё было как часть
# этой книги допустим что я читал, но этим не исчерпывалась моя деятельность за этот промежуток времени и вот
# имея несколько задач что мы выполняем это своего рода todo но с отщетом не по times а по времени, но вот
# только эта система не поддерживает иерархии, те как их отслеживать и фиксировать и надоли (я думаю нет)
# можно будет добавить конечный задачи а те обстракции на коих зиждиться моя предидущая реализация (и много кастылей)
# чем проше тем гибче будет в конечном итоге система, а значит надо провести рефакторинг и отделить лищьнее
### Command Line Input
set CLI_PARAMS [list]; 
set CLI_ARGS [list]
# arg pair or param is -t -time -trap=no (QEMU stile), but no -trap yes
foreach p $argv { if [regexp -- {^-([^=]+)(?:=(.+))?$} $p all pname pval] { 
	lappend CLI_PARAMS [list $pname $pval] 
} else { lappend CLI_ARGS $p} }

# if count_optional <0 then unlimit optional arguments
proc args_check {require optional {offset 1}} { global CLI_ARGS
	set alen [expr [llength $CLI_ARGS] - $offset]
	return  [expr !(($alen >= $require) && (($alen <= ($optional + $require)) || ($optional < 0)))] 
}
# req_params is list of names required params
proc params_check pamlist { set bpam 1; global CLI_PARAMS
	foreach p $CLI_PARAMS { set pam [lsearch -index 0 $CLI_PARAMS $p]; set bpam [expr $bpam && ($pam != -1)] }
	return $bpam
}

# param or value
proc pamVal {name {orval false} {convert 0} {empty_val true}} {
	global CLI_PARAMS
	set val [lsearch -index 0 -inline $CLI_PARAMS $name]
	if [string eq $val ""] { return $orval } else { set val [lindex $val 1]
		if [string eq $convert 0] { 
			if [string eq $val ""] { return $empty_val} else { return $val}
		} else { return [$convert $val]}
	}
}

### general functions
proc mytime {timesec} { return [clock format $timesec -format [pamVal timeformat]]}
proc mytimescan {timeline} { return [clock scan $timeline -format [pamVal timescan]]}
proc myhelp {} {global ABOUT; puts $ABOUT; exit}

# система конфигурации как в more 
set ABOUT {test}
proc about-include {name about {def ""} {defhum ""}} { global ABOUT; global CLI_PARAMS
	set mval [pamVal $name ""]
	if [string eq $mval ""] { 
		if [string eq $def ""] { set ln "-$name=<$about>"
		} else { if [string eq $defhum ""] {set df $def} else {set df $defhum}
			set ln "\[-$name=<$about, def \"$df\">\]" 
			lappend CLI_PARAMS [list $name $def] }
	} else { set ln "-$name=$mval"}
	set ABOUT "$ABOUT\t$ln\n"
}

about-include "database" "here your database" 

# более гибкая система команд из-за чего всё не скатываеться в мешанину
# Какие каманды действительно нужны и в каком виде?
# управление думаю следует сделать орентируясь не на будет выполнено, а было выполнено как с записями в more
# те создал я значит какойто процес я могу задать ему чем он будет
# next <mesg> - задаёт что с предидущей отметки я допустим гулял
set SQL_next {INSERT donelog(begin, end, comment)
	SELECT end, strftime('%s'), ? 
	FROM donelog
	ORDER BY begin DESC
	WHERE end IS NOT NULL
	LIMIT 1;} 
# last <id> - связывает это с прогулкой (что дажача по здоаовью) (возможно вместо id следует использовать имена)
set SQL_last {INSERT byplan(done, task)
	SELECT begin, ?
	FROM donelog
	ORDER BY begin DESC
	LIMIT 1;} 
# stun - переводит систему в состояние ожидания ответа (ставит отметку начала)
set SQL_stun {INSERT donelog(begin) SELECT strftime('%s');}
# tell <mesg> - выводит систему из того состояния в коем прибывал (дополняет запись)
set SQL_tell {UPDATE donelog SET end = strftime('%s'), comment = ? 
	FROM (SELECT begin AS last 
		FROM donelog
		ORDER BY begin DESC
		WHERE end IS NULL 
		LIMIT 1)
	WHERE begin = last;}
# думаеться мне что следует сделать иерархию временных отрезков (всё переделывать, опять)
#	- задачи - длительность, описание, id - список возможных к выполнению задач
#		- плановое событие - начало, желаемая длительность, описание - запланированный длительный (обычно) процесс 
#		что может прирываться обычными
# 			- обычное событие - начало, конец (длина), сообщение - ветви, потомков неимеют
# task <length> <description> - вопрос а нужна ли мне переодичность что это даёт (окно что можно пропустить, не такли?)
set SQL_task {INSERT tasks(length, descriprion) VALUES(?, ?);}
# хаотичной системе управления временем не нужны периуды, используй календарь
# list [<pattern>] - возврашает таблицу с id по шаблону отфильтрованных задач
set SQL_list {SELECT * FROM tasks WHERE flag = 0;}
# need <length> <description> - создаёт задачу и плановое событие
# muts <id> [<length>] - создаёт плановое событие и связывает с задачей для выполнения
# end <id> <comment> - завершает и подругому никак выполнение планового события задачи id
# hide <id> - скрывает задачу из списка, их нельзя удалять тк дркгие записи с ними связаны что может привести к непонятному поведению
set SQL_hide {UPDATE tasks SET flag = 1 WHERE begin = ?;}
# restore <id> - у спрятанных занисей id сохраняеться навсегда и они могут сново отображаться в списке
set SQL_hide {UPDATE tasks SET flag = 0 WHERE begin = ?;}
# Здась может потребоваться некоторое количество графических интерфейсов и для их реализации предпочтительнее tk
# за сим следует перенести эту систему на tcl так это даст больше преимуществ для её эксплуатации


set MakeDB {
BEGIN TRANSACTION;
-- planed dones
CREATE TABLE IF NOT EXISTS "byplan" (
	"done"	INTEGER,
	"task"	INTEGER,
	FOREIGN KEY("done") REFERENCES "donelog"("begin"),
	FOREIGN KEY("task") REFERENCES "tasks"("id")
);
-- log what you do
CREATE TABLE IF NOT EXISTS "donelog" (
	"begin"	INTEGER NOT NULL, -- utime
	"end"	INTEGER, -- utime 
	"comment"	TEXT,
	PRIMARY KEY("begin")
);
-- plans
CREATE TABLE IF NOT EXISTS "tasks" (
	"id"	INTEGER,
	"description"	TEXT NOT NULL,
	"length"	INTEGER NOT NULL, -- in secs
	"flags"	INTEGER DEFAULT 0, -- use for hide old records and so more
--	"period"	TEXT, -- any that define period (cron or same more usefull)
-- format period(cron): "* * * * *"
--	"pertype" INTEGER DEFAULT 0, -- 0 - cron, 1+ some more
	PRIMARY KEY("id" AUTOINCREMENT)
);
COMMIT;}

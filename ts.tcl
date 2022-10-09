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
# start - переводит систему в состояние ожидания ответа (ставит отметку начала)
set SQL_start {INSERT donelog(begin) SELECT strftime('%s');}
# muts [<length>] - создаёт плановое событие и связывает с задачей для выполнения
set SQL_must {}
# next <mesg> - задаёт что с предидущей отметки я допустим гулял
set SQL_next {INSERT donelog(begin, end, comment)
	SELECT end, strftime('%s'), ? 
	FROM donelog
	ORDER BY begin DESC
	WHERE end IS NOT NULL
	LIMIT 1;} 
# думаеться мне что следует сделать иерархию временных отрезков (всё переделывать, опять)
#	- задачи - длительность, описание, id - список возможных к выполнению задач
#		- плановое событие - начало, желаемая длительность, описание - запланированный длительный (обычно) процесс 
#		что может прирываться обычными
# 			- обычное событие - начало, конец (длина), сообщение - ветви, потомков неимеют
# хаотичной системе управления временем не нужны периуды, используй календарь
# sub <id>- вклинивает задачу как подзадачу выполняемой
# list - список выполняемых задач
set SQL_list {SELECT * FROM donelog WHERE end IS NULL;}
# need <length> <description> - создаёт задачу и плановое событие
# end <id> <comment> - завершает и подругому никак выполнение планового события задачи id
set SQL_end {UPDATE donelog SET end = strftime('%s'), comment = ? 
	FROM (SELECT begin AS last 
		FROM donelog
		ORDER BY begin DESC
		WHERE end IS NULL 
		LIMIT 1)
	WHERE begin = last;}
# Здась может потребоваться некоторое количество графических интерфейсов и для их реализации предпочтительнее tk
# за сим следует перенести эту систему на tcl так это даст больше преимуществ для её эксплуатации
# stat [-l=<cicle length>] <filtr pattern> - выводит записи за системный цикл
set SQL_stat {SELECT * FROM donelog WHERE;}
# parts [-l=<cicle length>] <patern> [.. <paternN>] - выводит процент шаблона от периода и от других шаблонов (с верменем)
set SQL_part {SELECT * FROM donelog WHERE;}


set MakeDB {
BEGIN TRANSACTION;
-- special values for donelogs
CREATE TABLE IF NOT EXISTS "slots" (
	"done"	INTEGER,
	"slot"	INTEGER,
	"value" TEXT,
	FOREIGN KEY("done") REFERENCES "donelog"("begin"),
);
-- log what you do
CREATE TABLE IF NOT EXISTS "donelog" (
	"begin"	INTEGER NOT NULL, -- utime
	"end"	INTEGER, -- utime 
	"mesg"	TEXT,
	PRIMARY KEY("begin")
);
COMMIT;}

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

# конфигурация
set ABOUT ""
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
proc about-command {usage about} { global ABOUT;
	set ABOUT "$ABOUT\t\t$about\n\t$usage\n";
}
proc about-switch {categ} { global ABOUT;
	set ABOUT "$ABOUT$categ\n";
}

# configuration
about-switch "Config"
about-include "database" "location of your database" 
about-include "cicle" "length of your work cicle (in sec)" [expr 3600*24*7] 
about-include timeformat "print time format" "%a %d.%m (%Y) %H:%M {%s}"
proc time-format {timesec} { return [clock format $timesec -format [pamVal timeformat]]}
about-include timescan "user scan time format" "%Y%m%d%H%M"
proc time-format-scan {timeline} { return [clock scan $timeline -format [pamVal timescan]]}
proc show-slot {slot value} {
	switch $slot {
		0 { return "up:$value"}
		1 { return "len:[expr $value/60]'"}
		default {return ""}
	}
}

proc help-gen {} {global ABOUT; puts $ABOUT}
# req_params is list of names required params
proc params-check pamlist { set bpam 1; global CLI_PARAMS
	foreach p $CLI_PARAMS { 
		set pam [lsearch -index 0 $CLI_PARAMS $p] 
		set bpam [expr $bpam && ($pam != -1)] 
	}
	return $bpam
}

set MakeBase {
BEGIN TRANSACTION;
-- special values for donelogs
CREATE TABLE IF NOT EXISTS "slots" (
	"done"	INTEGER,
	"slot"	INTEGER, -- 0: up, 1: length
	"value" TEXT,
	FOREIGN KEY("done") REFERENCES "donelog"("begin")
);
-- log what you do
CREATE TABLE IF NOT EXISTS "donelog" (
	"begin"	INTEGER NOT NULL, -- utime
	"end"	INTEGER, -- utime 
	"mesg"	TEXT,
	PRIMARY KEY("begin")
);
COMMIT;}

about-switch "Commands"
set COMMANDS [list]

# if count_optional <0 then unlimit optional arguments
proc command-collect {name require optional usage body descr} { global COMMANDS
	lappend COMMANDS [list $name $require $optional $body]
	about-command $usage $descr
}

proc command-exec {fail} { global COMMANDS; global CLI_ARGS
	set cmd [lsearch -index 0 $COMMANDS [lindex $CLI_ARGS 0]]
	if {$cmd == -1} $fail 
	set cmd [lindex $COMMANDS $cmd]

	set offset 1
	set require [lindex $cmd 1]
	set optional [lindex $cmd 2]
	set alen [expr [llength $CLI_ARGS] - $offset]
	set args [expr ($alen >= $require) && (($alen <= ($optional + $require)) || ($optional < 0))] 

	# arguments data
	set adata [lrange $CLI_ARGS 1 end]

	# database 
	package require sqlite3
	sqlite db [pamVal database]
	global MakeBase
	db eval $MakeBase
	db function regexp -deterministic {regexp --}

	# execute command
	if {$args} [lindex $cmd 3] $fail
}

# actions
command-collect do 0 1 {do [<length, in mins>]} {
	set sec [clock seconds]
	db eval "INSERT INTO donelog(begin) VALUES ($sec);"
	if [llength $adata] {
		set len [expr 60*[lindex $adata 0]]
		db eval "INSERT INTO slots(done, slot, value) VALUES ($sec, 1, $len);"
	}
} {запись выполнения нового задания}

command-collect sub 1 1 {sub <id> [<length>]} {
	set up [db eval "SELECT begin
	FROM ( SELECT row_number() OVER (ORDER BY begin) AS id, begin 
		FROM donelog 
		WHERE end IS NULL)
	WHERE id = [lindex $adata 0];"]
	if [string eq $up ""] exit 
	set sec [clock seconds]
	db eval "INSERT INTO donelog(begin) VALUES ($sec);"
	db eval "INSERT INTO slots(done, slot, value) VALUES ($sec, 0, $up);"
	if {[llength $adata] > 1} {
		set len [expr 60*[lindex $adata 1]]
		db eval "INSERT INTO slots(done, slot, value) VALUES ($sec, 1, $len);"
	}
} {вклинивает задачу как подзадачу выполняемой}

command-collect end 2 -1 {end <id> <mesg> [.. <mesg parts>]} {
	set sec [clock seconds]
	set id [lindex $adata 0]
	set mesg [lrange $adata 1 end]
	db eval "UPDATE OR IGNORE donelog
	SET mesg = '$mesg', end = $sec
	FROM (SELECT begin AS done, row_number() OVER (ORDER BY begin) AS id 
		FROM donelog)
	WHERE id = $id AND done = begin;"
	puts "Taken [db eval "SELECT (end-begin)/60 FROM donelog 
	WHERE end IS NOT NULL ORDER BY begin DESC LIMIT 1;"]'"
} {завершает задачу}

command-collect app 1 -1 {app <mesg> [.. <mesg parts>]} {
	db eval "INSERT INTO donelog(begin, end, mesg)
	SELECT end, strftime('%s'), '$adata'
	FROM donelog
	WHERE end IS NOT NULL
	ORDER BY begin DESC
	LIMIT 1;"
	puts "Taken [db eval "SELECT (end-begin)/60 FROM donelog 
	WHERE end IS NOT NULL ORDER BY begin DESC LIMIT 1;"]'"
} {отмечает время от конца последней завершенной задачи как повую задачу}

# information
command-collect wil 0 0 {wil} {
	db eval "SELECT row_number() OVER (ORDER BY begin) AS id, begin, slot, value
	FROM donelog LEFT JOIN slots ON done = begin
	WHERE end IS NULL;" { puts "$id: [time-format $begin]: {[show-slot $slot $value]}" }
} {список выполняемых задач}

command-collect ago 0 0 {ago} {
	db eval "SELECT begin, end - begin AS len, mesg, slot, value 
	FROM donelog LEFT JOIN slots ON done = begin
	WHERE end IS NOT NULL AND begin > [expr [clock seconds] - [pamVal cicle]];"  {
		puts "[expr $len/60]' [time-format $begin] {[show-slot $slot $value]}\n\t $mesg"
	}
} {выводит завершенные записи за установренный рабочий цикл}

command-collect stat 1 -1 {stat <pattern> [.. <next pattern>]} {
	set parts [list]
	foreach patern $adata {
		lappend parts "SELECT '$patern' AS patern, SUM(end - begin) AS time, COUNT(*) AS num
		FROM donelog 
		WHERE mesg REGEXP '$patern' AND end is not null 
			AND begin > [expr [clock seconds] - [pamVal cicle]]"
	}
	puts "Patern\t\t\tTime, h\tTime/a\tNumber\tNum/a"
	set parts [join $parts "\nUNION\n"]
	set total [db eval "SELECT SUM(time), SUM(num) FROM ($parts)"]
	set total-time [lindex $total 0]
	set total-num [lindex $total 1]
	db eval "SELECT patern, time, num FROM ($parts)" {
		puts "\t$patern\t\t[expr $time/3600]\t[expr $time/${total-time}]\t$num\t[expr $num/${total-time}]"
	}
	puts "Total time: [expr ${total-time}/3600] h."
	puts "Total count: ${total-num}"
} {выводит процент шаблона от периода и от других шаблонов (с верменем)}

if [params-check database] {help-gen; exit}
command-exec {help-gen; exit}

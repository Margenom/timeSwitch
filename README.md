# Config

	-database=<location of your database>
	[-cicle=<length of your work cicle (in sec), def "604800">]
	[-timeformat=<print time format, def "%a %d.%m (%Y) %H:%M {%s}">]

# Commands

		запись выполнения нового задания
	do [<length, in mins>]
		вклинивает задачу как подзадачу выполняемой
	sub <id> [<length>]
		завершает задачу
	end [-g gui input] [-n notification] <id> <mesg> [.. <mesg parts>]
		отмечает время от конца последней завершенной задачи как повую задачу
	app [-n notification] [-g gui input] [-o=<offset>] <mesg> [.. <mesg parts>]
		список выполняемых задач
	wil
		выводит завершенные записи за установренный рабочий цикл
	ago
		выводит процент шаблона от периода и от других шаблонов (с верменем)
	stat <pattern> [.. <next pattern>]


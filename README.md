# Config

	-database=<location of your database>
	[-cicle=<length of your work cicle (in sec), def "604800">]
	[-timeformat=<print time format, def "%a %d.%m (%Y) %H:%M {%s}">]

# Commands

		create new task
	do [-u=<id>] [-l=<length, in mins>] [<temp name> .. <name parts>]
		end task by id
	end [-g gui input] [-n notification] <id> <mesg> [.. <mesg parts>]
		append task next by last complited task (with offset)
	app [-n notification] [-g gui input] [-o=<offset >=0, def 0>] <mesg> [.. <mesg parts>]
		list tasks in process
	wil
		list complited tasks in current cicle
	ago
		show some statistic by pattern in completed tasks into current cicle
	stat <pattern> [.. <next pattern>]


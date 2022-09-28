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
	"end"	INTEGER NOT NULL, -- utime 
	"comment"	TEXT NOT NULL,
	PRIMARY KEY("begin")
);
-- plans
CREATE TABLE IF NOT EXISTS "tasks" (
	"id"	INTEGER,
	"description"	TEXT NOT NULL,
	"length"	INTEGER NOT NULL, -- in secs
	"flags"	INTEGER DEFAULT 0, -- use for hide old records and so more
	"period"	TEXT, -- any that define period (cron or same more usefull)
-- format period(cron): "cron:* * * * *"
	PRIMARY KEY("id" AUTOINCREMENT)
);
COMMIT;

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <s7.h>

/* subs */
#include "rex.c"
#include "io.c"
#include "gui.c"
#include "time_clock.c"

#define BOXY_SCRIPT "/boxy.ss"

// return allocated memory
char* exe_path(int pid, int no_realpath) {
	char* procdir = NULL;
	asprintf(&procdir, "/proc/%d/exe", pid);

	if (no_realpath) return procdir;
	
	char *realexe = realpath(procdir, NULL);
	free(procdir);
	return realexe;
}

int main(int argc, char *argv[]) {
	int pid = getpid();
	char* load_script = exe_path(pid, 0);
	s7_scheme *sc = s7_init();

// init subs
	rex(sc);
	io(sc);
	gui(sc);
	time_clock(sc);

{ // init globals variables
	s7_pointer params = s7_nil(sc),
		data = s7_nil(sc);
	for (int i=0; i <argc; i++) {
		char *arg = argv[i];
		if (*arg == '-') {
			char *sep = strchr(arg, '=');
			params = s7_cons(sc, s7_cons(sc, 
				s7_make_string_with_length(sc, arg+1, (sep? sep - arg: strlen(arg))-1),
				sep? s7_make_string(sc, sep+1): s7_t(sc)),
				params);
		} else {data = s7_cons(sc, s7_make_string(sc, arg), data);}
	}
	s7_define_variable(sc, "CLI_PARAMS", s7_reverse(sc, params));
	s7_define_variable(sc, "CLI_ARGS", s7_reverse(sc, data));
	s7_define_variable(sc, "CLI_EXEC", s7_make_string(sc, argv[0]));
	char *path = load_script, *sep = strrchr(load_script, '/');
	load_script = strndup(path, sep?sep-path:strlen(path));
	free(path);
	s7_add_to_load_path(sc, load_script); 
	s7_define_variable(sc, "CLI_PATH", s7_make_string(sc, load_script));
}
// end init

	load_script = realloc(load_script, strlen(load_script)+ strlen(BOXY_SCRIPT)+1);
	if (!s7_load(sc, strcat(load_script, BOXY_SCRIPT)))
		s7_repl(sc);
	free(load_script);

	return 0;
}

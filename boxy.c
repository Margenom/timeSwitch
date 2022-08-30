#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <s7.h>

/* subs */
#include "rex.c"
//#include "io.c"
#include "time_clock.c"

s7_pointer with_output_to_file_append(s7_scheme *sc, s7_pointer a) {
	if (!(s7_list_length(sc, a) >1 && s7_is_string(s7_car(a))))
		return s7_f(sc);
	s7_pointer port = s7_open_output_file(sc, s7_string(s7_car(a)), "a"),
		curport = s7_current_output_port(sc);
	s7_set_current_output_port(sc, port);
	
	s7_pointer out = s7_call(sc, s7_cadr(a), s7_nil(sc));
	//s7_flush_output_port(sc, port);
	s7_close_output_port(sc, port);

	s7_set_current_output_port(sc, curport);
	return out;
}

int main(int argc, char *argv[]) {
	s7_scheme *sc = s7_init();
//subs
	rex(sc);
//	io(sc);
	time_clock(sc);

	s7_define_function(sc, "with-output-to-file-append", with_output_to_file_append,  2, 0, 0, "(with-output-to-file-append <file> <writer>): output from writer or #f");

	s7_pointer params = s7_nil(sc),
		data = s7_nil(sc);
	for (int i=0; i <argc; i++) {
		char *arg = argv[i];
		if (*arg == '-') {
			char *sep = strchr(arg, '=');
			params = s7_cons(sc, s7_cons(sc, 
					s7_make_string_with_length(sc, arg+1, (sep? sep - arg: strlen(arg))-1),
					sep? s7_make_string(sc, sep+1): s7_f(sc)),
				params);
		} else {data = s7_cons(sc, s7_make_string(sc, arg), data);}
	}
	s7_define_variable(sc, "CLI_PARAMS", s7_reverse(sc, params));
	s7_define_variable(sc, "CLI_ARGS", s7_reverse(sc, data));
	char *path = realpath(argv[0], NULL), *sep;
	sep = strrchr(path, '/');
	s7_define_variable(sc, "CLI_PATH", s7_make_string_with_length(sc, path, sep?sep-path:strlen(path)));
	s7_define_variable(sc, "CLI_EXEC", s7_make_string(sc, sep? sep+1: path));
	free(path);

	if (!s7_load(sc, "boxy.ss"))
		s7_repl(sc);

	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <s7.h>
#include <time.h>

/* subs */
#include "rex.c"
#include "io.c"

s7_pointer clock_seconds(s7_scheme *sc, s7_pointer a) {return s7_make_integer(sc, time(NULL));}
s7_pointer clock_vec(s7_scheme *sc, s7_pointer a) {
	time_t tnow = time(NULL);
	
	if (s7_list_length(sc, a) >0 && s7_is_integer(s7_car(a)))
		tnow = s7_integer(s7_car(a));

	int localorgm = 1;
	if (s7_list_length(sc, a) >1 && s7_is_boolean(s7_cadr(a)))
		localorgm = s7_boolean(sc, s7_cadr(a));
	struct tm *tgrid = (localorgm?localtime:gmtime)(&tnow);
		
	int itr =0;
	s7_pointer out = s7_make_vector(sc, 9);

	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_sec));
	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_min));
	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_hour));
	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_mday));
	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_mon));
	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_year));
	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_wday));
	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_yday));
	s7_vector_set(sc, out, itr++, s7_make_integer(sc, tgrid->tm_isdst));

	return out;
}
s7_pointer clock_format(s7_scheme *sc, s7_pointer a) {
	if (s7_list_length(sc, a) ==0 || !s7_is_string(s7_car(a))) return s7_f(sc);
	const char *format = s7_string(s7_car(a));
	
	s7_pointer tm;
	if (s7_list_length(sc, a) >1 && s7_is_vector(s7_cadr(a))) { tm = s7_cadr(a); }
	else tm = clock_vec(sc, s7_nil(sc));
		
	int itr =0;
	struct tm tgrid;

	tgrid.tm_sec = s7_integer(s7_vector_ref(sc, tm, itr++));
	tgrid.tm_min = s7_integer(s7_vector_ref(sc, tm, itr++));
	tgrid.tm_hour = s7_integer(s7_vector_ref(sc, tm, itr++));
	tgrid.tm_mday = s7_integer(s7_vector_ref(sc, tm, itr++));
	tgrid.tm_mon = s7_integer(s7_vector_ref(sc, tm, itr++));
	tgrid.tm_year = s7_integer(s7_vector_ref(sc, tm, itr++));
	tgrid.tm_wday = s7_integer(s7_vector_ref(sc, tm, itr++));
	tgrid.tm_yday = s7_integer(s7_vector_ref(sc, tm, itr++));
	tgrid.tm_isdst = s7_integer(s7_vector_ref(sc, tm, itr++));

	char buf[64];
	if (!strftime(buf, 63, format, &tgrid)){ return s7_f(sc); }
	else return s7_make_string(sc, buf);
}


int main(int argc, char *argv[]) {
	s7_scheme *sc = s7_init();
//subs
	rex(sc);
	io(sc);
	s7_define_function(sc, "clock", clock_vec,  0, 2, 0, "(clock [unix time] [local(def) time or UTC]): #(sec min hour mday mon year wday yday isdst)");
	s7_define_function(sc, "clock-format", clock_format,  1, 1, 0, "(clock <strftime format> [time vec from (clock)]): string or #f");
	s7_define_function(sc, "clock-seconds", clock_seconds,  0, 0, 0, "(clock_seconds): utime");

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

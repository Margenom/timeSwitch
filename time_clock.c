#include <time.h>
#include <s7.h>

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

void time_clock(s7_scheme *sc) {
	s7_define_function(sc, "clock", clock_vec,  0, 2, 0, "(clock [unix time] [local(def) time or UTC]): #(sec min hour mday mon year wday yday isdst)");
	s7_define_function(sc, "clock-format", clock_format,  1, 1, 0, "(clock <strftime format> [time vec from (clock)]): string or #f");
	s7_define_function(sc, "clock-seconds", clock_seconds,  0, 0, 0, "(clock_seconds): utime");
}

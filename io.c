#include <stdio.h>
#include <stdlib.h>
#include "s7.h"

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

void io(s7_scheme *sc) {
	s7_define_function(sc, "with-output-to-file-append", with_output_to_file_append,  2, 0, 0, "(with-output-to-file-append <file> <writer>): output from writer or #f");
}

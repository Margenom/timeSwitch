#include <stdio.h>
#include <stdlib.h>
#include "s7.h"

s7_pointer io_read_file(s7_scheme *sc, s7_pointer args) {
	FILE *fp = fopen(s7_string(s7_car(args)), "r");
	if (!fp) return s7_f(sc);

	fseek(fp, 0L, SEEK_END);
	long file_size = ftell(fp);
	fseek(fp, 0L, SEEK_SET);

	char*buf = malloc(file_size);
	if(0>fread(buf, file_size, 1, fp)) {
		free(buf);
		fclose(fp);
		return s7_f(sc);
	}

	s7_pointer out = s7_make_string(sc, buf);
	free(buf);
	fclose(fp);
	return out;
}

s7_pointer io_addto_file(s7_scheme *sc, s7_pointer args) {
	FILE *fp = fopen(s7_string(s7_car(args)), "a");
	if (!fp) return s7_f(sc);

	s7_pointer str = s7_string(s7_cadr(args));
	if(0>fwrite(s7_string(str), s7_string_length(str), 1, fp)) {
		fclose(fp);
		return s7_f(sc);
	}
	fclose(fp);
	return s7_string_length(str);;
}

s7_pointer io_write_file(s7_scheme *sc, s7_pointer args) {
	FILE *fp = fopen(s7_string(s7_car(args)), "w");
	if (!fp) return s7_f(sc);

	s7_pointer str = s7_string(s7_cadr(args));
	if(0>fwrite(s7_string(str), s7_string_length(str), 1, fp)) {
		fclose(fp);
		return s7_f(sc);
	}
	fclose(fp);
	return s7_string_length(str);
}

void io(s7_scheme *sc) {
	s7_define_function(sc, "io-read-file", io_read_file,  1, 0, 0, "(io-read-file file)");
	s7_define_function(sc, "io-write-file", io_write_file, 2, 0, 0, "(io-write-file file cont)");
	s7_define_function(sc, "io-append-file", io_addto_file, 2, 0, 0, "(io-append-file file append)");
}

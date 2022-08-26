#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <s7.h>

/* subs */
#include "rex.c"
#include "io.c"
/*
s7_pointer boxy_log_scan(s7_scheme *sc, s7_pointer args) {
	FILE *fp = fopen(s7_string(s7_car(args)), "r");
	if (!fp) return s7_f(sc);

	fseek(fp, 0l, seek_end);
	long file_size = ftell(fp);
	fseek(fp, 0l, seek_set);

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

s7_pointer boxy_log_store_head(s7_scheme *sc, s7_pointer args) {
	FILE *fp = fopen(s7_string(s7_car(args)), "a");
	if (!fp) return s7_f(sc);
	fprintf(fp, "%d\t", s7_number(s7_car(args)));
	fclose(fp);
	return s7_number(s7_car(args));
}
*/
/*
s7_pointer boxy_log_scan_last(s7_scheme *sc, s7_pointer args) {
	FILE *fp = fopen(s7_string(s7_car(args)), "r");
	if (!fp) return s7_f(sc);

	int ch;
	long line, linebef;
	while((ch = fgetc(fp)) != EOF) {
		if (ch == '\n') {
			linebef = line;
			line = ftell(fp);
		}
	}
	long file_end = ftell(fp);

	s7_pointer out = s7_cons(sc, s7_make_boolean(sc, 
	

	fseek(fp, 0l, seek_set);

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
*/
/*
s7_pointer boxy_log_store_about(s7_scheme *sc, s7_pointer args) {
	FILE *fp = fopen(s7_string(s7_car(args)), "r");
	fseek(fp, 0l, seek_end);
	long file_size = ftell(fp);
	if (file_size == 0) {fclose(fp); return s7_f(sc);}
	fseek(fp, (file_size-19 <0)?0:file_size-19, seek_set);
	
	long start

	char buf[20];
	if(0>(file_size =fread(buf, 19, 1, fp))) {
		free(buf);
		fclose(fp);
		return s7_f(sc);
	}
	if (file_size == 19) {
		char*t = strrchr(buf, '\n')+1;

	s7_pointer out = s7_make_string(sc, buf);

	s
	if (!fp) return s7_f(sc);
	fprintf(fp, "%d\t%s", s7_number(s7_car(args)), s7_string(s7_car(args)));
	fclose(fp);
	return s7_number(s7_car(args));
}
*/


int main(int argc, char *argv[]) {
	s7_scheme *sc = s7_init();
//subs
	rex(sc);
	io(sc);
//	s7_define_function(sc, "io-read-file", io_read_file,  1, 0, 0, "(io-read-file file)");

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

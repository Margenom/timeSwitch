/* based on pcre2demo code example */
#define PCRE2_CODE_UNIT_WIDTH 8

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include <pcre2.h>
#include "s7.h"

/*GFN* general functions */
int rex_is_object(s7_scheme *sc, s7_pointer obj, int obj_type) {
	return s7_is_c_object(obj) && s7_c_object_type(obj) == obj_type;}
int rex_equal(s7_scheme *sc, s7_pointer a, s7_pointer b, int obj_type) {
	return ( a == b)|| 
	( /* rex_is_object(sc, a, obj_type) && *first obj this type*/ 
	  rex_is_object(sc, b, obj_type) && (s7_c_object_value(a) == s7_c_object_value(b)));
}
/*GFN*/
/*VAR* variables and constants */
static int rex_type_code = 0;
static int rex_type_match = 0;
/*VAR*/
/*CODE* pcre code spicific functions */
s7_pointer rex_code_compile(s7_scheme *sc, s7_pointer args) {
	int errornumber;
	PCRE2_SIZE erroroffset;
	s7_pointer pattern = s7_car(args);
	
	pcre2_code *re = pcre2_compile( s7_string(pattern),               /* the pattern */
	  s7_string_length(pattern), /* PCRE2_ZERO_TERMINATED indicates pattern is zero-terminated */
	  PCRE2_UCP| PCRE2_UTF,  /* unicode support */
	  &errornumber,          /* for error number */
	  &erroroffset,          /* for error offset */
	  NULL);                 /* use default compile context */
	
	/* Compilation failed: print the error message and exit. */
	
	if (re == NULL) {
	  PCRE2_UCHAR buffer[256];
	  pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
	  printf("PCRE2 compilation failed at offset %d: %s\n", (int)erroroffset, buffer);
	  return s7_list(sc, 3, s7_f(sc), 
		s7_make_integer(sc, erroroffset), 
		s7_make_string(sc, buffer)); }
	return s7_make_c_object(sc, rex_type_code, re);
}
s7_pointer rex_code_free(s7_scheme *sc, s7_pointer obj) {
	pcre2_code_free(s7_c_object_value(obj));  /*   data and the compiled pattern. */
	return NULL;}
s7_pointer rex_code_is_code(s7_scheme *sc, s7_pointer args) {
	return s7_make_boolean(sc, rex_is_object(sc, s7_car(args), rex_type_code));}
s7_pointer rex_code_is_equal(s7_scheme *sc, s7_pointer objs) {
	s7_pointer a = s7_car (objs),
		   b = s7_cadr (objs);
	return s7_make_boolean(sc, rex_equal(sc, a, b, rex_type_code));
}
void rex_code_type_make(s7_scheme *sc) {
	rex_type_code = s7_make_c_type(sc, "rex code");
	s7_c_type_set_gc_free(sc, rex_type_code, rex_code_free);
	s7_c_type_set_is_equal(sc, rex_type_code, rex_code_is_equal);

	s7_define_function(sc, "rex-compile", rex_code_compile, 1, 0, 0, 
		"(rex-compile pattern) makes compilled code of regular extention"); 
}
/*CODE*/
/*MATCH* match specific */
struct rex_match {
	s7_pointer str, code;
	pcre2_match_data *data;
};

s7_pointer rex_match(s7_scheme *sc, s7_pointer args) {
	PCRE2_SIZE offset = s7_list_length(sc, args)>2? s7_integer(s7_caddr(args)): 0;
	struct rex_match *rm = malloc (sizeof (struct rex_match));
	rm->str = s7_cadr(args);
	rm->code = s7_car(args);

	if (!(rex_is_object(sc, rm->code, rex_type_code) && s7_is_string(rm->str)))
		goto rex_match_err;
	pcre2_code *re = s7_c_object_value(rm->code);
	rm->data = pcre2_match_data_create_from_pattern(re, NULL);

	int rc = pcre2_match( re,                   /* the compiled pattern */
	  s7_string(rm->str),       /* the subject string */
	  s7_string_length(rm->str),/* the length of the subject */
	  offset,                    /* start at offset 0 in the subject */
	  0,                    /* default options */
	  rm->data,           /* block for storing the result */
	  NULL);                /* use default match context */

	/* Matching failed */
	if (rc < 0) {
	  	pcre2_match_data_free(rm->data);   /* Release memory used for the match */
		goto rex_match_err;}	

	return s7_make_c_object(sc, rex_type_match, rm);
	/* Match succeeded. Get a pointer to the output vector, where string offsets are stored. */
rex_match_err:
	free(rm);
	return s7_f(sc);
}
s7_pointer rex_match_free(s7_scheme *sc, s7_pointer obj) {
	pcre2_match_data_free(((struct rex_match*)s7_c_object_value(obj))->data);
	free(s7_c_object_value(obj));
	return NULL;
}
s7_pointer rex_match_mark(s7_scheme *sc, s7_pointer obj) {
	s7_mark(((struct rex_match*)s7_c_object_value(obj))->str);
	s7_mark(((struct rex_match*)s7_c_object_value(obj))->code);
	return NULL;
}
s7_pointer rex_match_is_match(s7_scheme *sc, s7_pointer obj) {
	return s7_make_boolean(sc, rex_is_object(sc, s7_car(obj), rex_type_match));}
s7_pointer rex_match_is_equal(s7_scheme *sc, s7_pointer objs) {
	s7_pointer a = s7_car (objs),
		   b = s7_cadr (objs);
	return s7_make_boolean(sc, rex_equal(sc, a, b, rex_type_match));
}
void rex_match_type_make(s7_scheme *sc) {
	rex_type_match = s7_make_c_type(sc, "rex match");
	s7_c_type_set_gc_free(sc, rex_type_match, rex_match_free);
	s7_c_type_set_gc_mark(sc, rex_type_match, rex_match_mark);
	s7_c_type_set_is_equal(sc, rex_type_match, rex_match_is_equal);

	s7_define_function(sc, "rex-match", rex_match, 2, 1, 0, 
		"(rex-match code string [offset]) match pattern in string"); 
}

s7_pointer rex_match_sub_vector(s7_scheme *sc, s7_pointer args) {
	s7_pointer mch = s7_car(args);
	if (!rex_is_object(sc, mch, rex_type_match))
		return s7_f(sc);

	struct rex_match *rm = s7_c_object_value(mch);
	
	PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(rm->data);
	uint32_t count = pcre2_get_ovector_count(rm->data);
	s7_pointer out = s7_make_vector(sc, count);
	
	for (int i=0; i < count; i++)
		s7_vector_set(sc, out, i, s7_cons(sc, s7_make_integer(sc, ovector[2*i]), 
					s7_make_integer(sc, ovector[2*i+1])));
	return out;
}
s7_pointer rex_match_sub_name_vector(s7_scheme *sc, s7_pointer args) {
	s7_pointer mch = s7_car(args);
	if (!rex_is_object(sc, mch, rex_type_match))
		return s7_f(sc);

	struct rex_match *rm = s7_c_object_value(mch);
	pcre2_code *re = s7_c_object_value(rm->code);

	PCRE2_SPTR tabptr, name_table, name_entry_size;
	PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(rm->data);

	uint32_t namecount;
	pcre2_pattern_info(
	  re,                   /* the compiled pattern */
	  PCRE2_INFO_NAMECOUNT, /* get the number of named substrings */
	  &namecount);          /* where to put the answer */
	
	if (namecount == 0) return s7_nil(sc); else {
	
	  /* Before we can access the substrings, we must extract the table for
	  translating names to numbers, and the size of each entry in the table. */
	
	  pcre2_pattern_info(re,/* the compiled pattern */
	    PCRE2_INFO_NAMETABLE,     /* address of the table */
	    &name_table);             /* where to put the answer */
	
	  pcre2_pattern_info(re,/* the compiled pattern */
	    PCRE2_INFO_NAMEENTRYSIZE, /* size of each entry in the table */
	    &name_entry_size);        /* where to put the answer */
	
	  /* Now we can scan the table and, for each entry, print the number, the name,
	  and the substring itself. In the 8-bit library the number is held in two
	  bytes, most significant first. */
	
	  tabptr = name_table;
	s7_pointer out = s7_make_vector(sc, namecount);
	for (int i = 0; i < namecount; i++) {
		int n = (tabptr[0] << 8) | tabptr[1];
		s7_vector_set(sc, out, i, s7_cons(sc, 
			s7_make_string(sc, tabptr+2), //name_entry_size -3),
			s7_cons(sc, 
				s7_make_integer(sc, ovector[2*n]), 
				s7_make_integer(sc, ovector[2*n+1]))));
	    	tabptr = (PCRE2_SPTR)((long) tabptr + (long)name_entry_size);
		}
		return s7_vector_to_list(sc, out);
	}
}
/*MATCH*/

void rex(s7_scheme *sc) {
	rex_code_type_make(sc);	
	rex_match_type_make(sc);	

	s7_define_function(sc, "rex-match-index", rex_match_sub_vector, 1, 0, 0, 
		"(rex-match-index match) return matched patern vector by index"); 
	s7_define_function(sc, "rex-match-names", rex_match_sub_name_vector, 1, 0, 0, 
		"(rex-match-names match) return matched patern vector with names"); 
}

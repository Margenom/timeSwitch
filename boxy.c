#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "s7.h"

/* subs */
#include "rex.c"
#include "io.c"

int main() {
	s7_scheme *s7 = s7_init();
//subs
	rex(s7);
	io(s7);

	if (!s7_load(s7, "boxy.ss"))
		s7_repl(s7);

	return 0;
}

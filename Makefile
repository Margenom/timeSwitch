# pcre2
Libs=`pkg-config --cflags --libs libpcre2-8`
# s7
Libs+= -lm -ldl -ls7_deb -L/ext/lib
Cflags=-fpic -Wall -g -I/ext/include
S7Home=${PWD}/s7

boxy:boxy.o
	cc $(Libs) -Wl,-export-dynamic boxy.o -o boxy

boxy.o:boxy.c rex.c io.c
	cc -c boxy.c $(Cflags) -o boxy.o

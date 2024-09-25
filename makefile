
CC=gcc
CFLAGS=-std=c89 -pedantic -g

all: mycc

clean:
	rm -f mycc

mycc: mycc.c
	${CC} $< -o $@ ${CFLAGS}	


.PHONY: all clean


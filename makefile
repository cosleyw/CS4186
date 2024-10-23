
CC=gcc
#CFLAGS=-std=c89 -pedantic -g
CFLAGS=-g -O0

all: mycc

clean:
	rm -f mycc

mycc: mycc.c
	${CC} $< -o $@ ${CFLAGS}	

format:
	clang-format -i ./parser.c

run:
	valgrind --leak-check=full --show-leak-kinds=all ./mycc ./test.c


.PHONY: all clean format run


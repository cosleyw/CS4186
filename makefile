
CC=gcc
CFLAGS=-std=c89 -pedantic -g -O0
#CFLAGS=-g -O0 -fsanitize=address
#CFLAGS=-g -O0

all: mycc

clean:
	rm -f mycc

mycc: mycc.c
	${CC} $< -o $@ ${CFLAGS}	

format:
	clang-format -i ./parser.c

run:
	valgrind -s --leak-check=full --show-leak-kinds=all ./mycc ./test.c


.PHONY: all clean format run


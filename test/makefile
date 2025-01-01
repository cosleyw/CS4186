
test.o: test.S
	nasm -f elf64 -o $@ $<

test: test.o  /lib/musl/lib/libc.a
	ld -m elf_x86_64 -N -o $@ $^

all: test

clean:
	rm -f ./test ./test.o

.PHONY: all clean

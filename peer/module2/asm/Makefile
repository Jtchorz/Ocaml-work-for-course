.PHONY: all clean

all:
	gcc -c factorial.c -o fac.o
	nasm -felf64 main-asm.asm -o main-asm.o
	gcc -no-pie main-asm.o fac.o -o main-asm

asm:
	gcc -c factorial.c -o fac.o
	nasm -felf64 main-asm.asm -o main-asm.o
	gcc -no-pie main-asm.o fac.o -o main-asm

test: asm
	-./main-asm e 3 4
	-./main-asm 1 2 3
	-./main-asm m 2 3
	-./main-asm + -13 8
	-./main-asm ! 5 hewwo	

gdb-magic:	asm
	gdb --args main-asm m 2 3


clean:
	rm -f *.o main-c main-asm *~

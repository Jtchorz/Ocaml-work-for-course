.PHONY: all clean
all:
	ocamlbuild -use-menhir cigrid_mod2.native
	mv cigrid_mod2.native cigrid


test: all
	./cigrid --pretty-print tests/test1.c

	
test-token: all
	./cigrid --pretty-print --test tests/test1.c

test2: all
	./cigrid --pretty-print tests/examples.cpp > out.txt
	diff -w tests/examples.txt out.txt

test3: all
	./cigrid --pretty-print --name-analysis tests/test1.c

test2-semantics: all 
	./cigrid --type-check --line-error tests/examples.cpp
test-semantics: all 
	./cigrid --pretty-print --type-check --line-error tests/semTest.cpp

clean-o:
	-rm *.o

asm-generation: clean-o all 
	./cigrid --asm tests/asmtest.cpp > asm.asm

asm: asm-generation
	nasm -felf64 asm.asm -o please.o
	gcc -no-pie please.o -o please
	-./please

asm-test: asm-generation
	gcc -c tests/test1.c -o tests/test.o
	nasm -felf64 asm.asm -o please.o
	gcc -no-pie please.o tests/test.o -o please
	

clean:
	$(RM) -rf _build
	$(RM) -f cigrid
	$(RM) -f *.conflicts
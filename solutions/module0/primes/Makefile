.PHONY: all clean
all:
	ocamlbuild primes.native
	mv primes.native hello

clean:
	$(RM) -rf _build
	$(RM) -f hello

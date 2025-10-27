executable=hello

all:
	dune build && cp _build/default/src/main.exe $(executable)

clean:
	dune clean
	rm -f $(executable)

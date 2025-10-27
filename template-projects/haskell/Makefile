.PHONY: all clean
all:
	stack build --allow-different-user
	cp $(shell stack path --local-install-root --allow-different-user)/bin/hello hello

clean:
	stack clean

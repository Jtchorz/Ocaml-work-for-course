# C Template

This is the C template for the ID2202 course. Like the other templates, building
happens with `make`, running with `./hello`. The project is built using `gcc`,
which you thus must have installed on your machine.

To use this template for an assignment, replace the following two lines in the `Makefile`:
```
EXEC = hello    # Rename to preferred name of executable
SRCS = hello.c  # Replace by your source files (space separated)
```
For example, if you replace them by
```
EXEC = calc
SRCS = calc.c
```
then upon a successful `make`, the executable is run with `./calc`. Remember to ignore the new file in `.gitignore`.

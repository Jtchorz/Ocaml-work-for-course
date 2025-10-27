# Makefile for a project written in C.

CC = gcc
CFLAGS = -Wall -O3
RM = rm -f

EXEC = hello    # Rename to preferred name of executable
SRCS = hello.c  # Replace by your source files (space separated)
OBJS = $(SRCS:.c=.o)

$(EXEC): %: %.o
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $^

.PHONY: clean
clean:
	$(RM) $(OBJS) $(EXEC)

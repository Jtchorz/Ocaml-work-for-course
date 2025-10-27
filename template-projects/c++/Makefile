# Makefile for a project written in C++.

CXX = g++
CXX_FLAGS = -std=c++11 -Wall -O3
RM = rm -f

EXEC = hello      # Rename to preferred name of executable
SRCS = hello.cpp  # Replace by your source files (space separated)
OBJS = $(SRCS:.cpp=.o)

$(EXEC): %: %.o
	$(CXX) $(CXX_FLAGS) -o $@ $^

%.o: %.c
	$(CXX) $(CXX_FLAGS) -c -o $@ $^

.PHONY: clean
clean:
	$(RM) $(OBJS) $(EXEC)

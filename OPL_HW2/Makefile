# Trivial makefile for the calculator scanner/parser.
# Depends on default (built-in) rules for C compilation.

# Note that rule for goal (parse) must be the first one in this file.

all: parse.o scan.o
	g++ -std=c++14 -o parse parse.o scan.o
parse.o: parse.cpp scan.h
	g++ -std=c++14 -c parse.cpp
scan.o: scan.cpp scan.h
	g++ -std=c++14 -c scan.cpp
clean:
	rm -rf *.o parse
test:  
	./parse < primes.cl
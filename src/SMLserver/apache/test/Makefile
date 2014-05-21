.PHONY: clean all

all: server 

server: server.c
	gcc -Wall -pedantic -g server.c -o server

gen: gen.c
	gcc -Wall -pedantic -g gen.c -o gen

clean:
	rm -f server gen

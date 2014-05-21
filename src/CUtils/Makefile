.PHONY: all

all: hashmap_typed_test binaryheap_test

hashmap_typed_test:
	gcc hashmap.c hashmap_typed.c hashmap_typed_test.c
	./a.out

binaryheap_test: 
	gcc -Wall -pedantic binaryheap_test.c
	@echo "output should be: 5 4 1 7 2 3 6"
	./a.out 3 4 5 2 1 6 3 

clean:
	rm -rf *~ a.out *.o

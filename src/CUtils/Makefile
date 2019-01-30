RESFILES=hashmap_typed_test.res binaryheap_test.res
CFLAGS=-Wall -pedantic

.PHONY: all
all: test

hashmap_typed_test.out:
	gcc $(CFLAGS) hashmap.c hashmap_typed.c hashmap_typed_test.c
	./a.out > $@

binaryheap_test.out:
	gcc $(CFLAGS) binaryheap_test.c
# @echo "output should be: 5 4 1 7 2 3 6"
	./a.out 3 4 5 2 1 6 3 > $@

%.res: %.out
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*: OK" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "Test $*: *** ERR: file $< differs from $<.ok ***" > $@ \
             ; else \
                echo "Test $*: *** ERR: file $<.ok does not exist ***" > $@ \
             ; fi \
         ; fi)

.PHONY: test
test: $(RESFILES)
	@cat $(RESFILES)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "-------------------------------------"
	@exit `grep "ERR" $(RESFILES) | wc -l`

clean:
	rm -rf *~ *.o *.res *.out

## Using the dojo library requires installation of dojo in the compilation directory; download 
## dojo-release-1.9.1 and unpack it, which should cause dijit, dojo, and dojox to appear in 
## this directory along-side the SML sources and this Makefile. Further, copy (or link) the 
## directory ../../basis to this directory and copy (or link) also the file ../../prims.js to 
## the present directory. After running 'make run.html', start a web server using, for instance
##
##   python -m SimpleHTTPServer 8000
##
## Now, request "localhost:8000/run.html" in your browser!
##

FILES=dojo_test.mlb dojo_test.sml
wd=$(shell pwd)
JS_PATH_FLAGS=-js_path_compress -js_path_relative_to "../.."
SMLTOJS=SML_LIB=../.. ../../../bin/smltojs -js_dom_mode

.PHONY: all
all:
	@echo "Assumptions:"
	@echo "  - install dojo, dojox, dijit, dgrid in present directory"
	@echo "Relevant targets:"
	@echo "  - make prepare0 : rebuild basis library"
	@echo "  - make prepare  : copy basis library and prims.js to present directory"  
	@echo "  - make run.html : compile example"
	@echo "Afterwards execute:"
	@echo "  - python -m SimpleHTTPServer 8000"
	@echo "Then request (in a browser):"
	@echo "  - http://localhost:8000/run.html"

.PHONY: prepare0
prepare0:
	(cd ../../..; make smltojs_basislibs)

.PHONY: prepare
prepare:
	cp -pa ../../prims.js ../../basis .

run.html: clean prepare Makefile $(FILES)
	$(SMLTOJS) -aopt $(JS_PATH_FLAGS) dojo_test.mlb

formlets_test.html: clean prepare Makefile $(FILES) formlets_test.mlb formlets_test.sml
	$(SMLTOJS) -aopt $(JS_PATH_FLAGS) -o formlets_test formlets_test.mlb

clean:
	rm -rf *~ MLB run.html prims.js basis

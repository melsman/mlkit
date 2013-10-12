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
SMLTOJS=SML_LIB=../.. ../../../bin/smltojs

run.html: Makefile $(FILES)
	$(SMLTOJS) -aopt $(JS_PATH_FLAGS) dojo_test.mlb

clean:
	rm -rf *~ MLB run.html  

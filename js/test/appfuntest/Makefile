## 
## 
## Establish links to directories js/basis, js/codemirror, js/doc, dojo-release-1.9.1/dojo, 
## dojo-release-1.9.1/dijit and to the files js/prims.js, js/appfun/dropbox.png and 
## js/appfun/nodropbox.png. Start web-server in this directory:
##  python -m SimpleHTTPServer 8000
##
SRC_ROOT=$(shell cd ../../..; pwd)
SML_LIB=$(SRC_ROOT)/js
SMLTOJS=SML_LIB=$(SML_LIB) $(SRC_ROOT)/bin/smltojs
CODEMIRRORFLAGS=-jslibs "$(SRC_ROOT)/js/codemirror/dist/js/codemirror.js"
#JS_PATH_FLAGS=-js_path_compress -js_path_prefix "js/test/appfuntest" -js_path_relative_to $(SRC_ROOT)
JS_PATH_FLAGS=-js_path_compress -js_path_relative_to $(SRC_ROOT)/js

all: index.html

index.html: Makefile appfuntest.sml appfuntest.mlb ../../appfun/AppFun2.sml ../../basis/dojo.sig ../../basis/dojo.sml
	$(SMLTOJS) -aopt $(JS_PATH_FLAGS) $(CODEMIRRORFLAGS) -o index appfuntest.mlb

deploy:
	cp index.html $(SRC_ROOT)/appfuntest.html

echo:
	echo SRC_ROOT is $(SRC_ROOT)

clean:
	rm -rf index.html *~ MLB 

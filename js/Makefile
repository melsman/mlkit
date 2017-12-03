HOST=37.139.21.24
TDIR=smlserver.org/www/smltojs

.PHONY: publish
publish: doc
	$(MAKE) -C test all
	ssh $(HOST) rm -rf $(TDIR)/basis $(TDIR)/test
	ssh $(HOST) mkdir -p $(TDIR)/basis/MLB/Js
	scp -r basis/MLB/Js/*.js $(HOST):$(TDIR)/basis/MLB/Js/
	ssh $(HOST) mkdir -p $(TDIR)/test/MLB/Js
	scp -r test/MLB/Js/*.js $(HOST):$(TDIR)/test/MLB/Js/
	scp -r test/*.html $(HOST):$(TDIR)/test/
	scp -r test/*.sml $(HOST):$(TDIR)/test/
	scp -r test/*.sig $(HOST):$(TDIR)/test/
	scp -r test/*.png $(HOST):$(TDIR)/test/
	scp prims.js $(HOST):$(TDIR)/
	$(MAKE) publishweb

publishweb: smltojs.man.html
	scp slides_diku_2007-11-27.pdf slides_lunch_2007-09-17.pdf base.css index.html smltojs_logo_color.png smltojs.man.html $(HOST):$(TDIR)/

smltojs.man.html: Makefile
	man2html -r ../man/man1/smltojs.1 > smltojs.man.html
#	rman -f HTML -r '' ../man/man1/smltojs.1 > smltojs.man.html

THEBASISFILES = js/basis/MLB/Js/*.js

CODEMIRRORFILES=js/codemirror/codemirror.js js/codemirror/codemirror.css js/codemirror/sml.js

favicon.ico: smltojs_logo_no_text.png
	convert smltojs_logo_no_text.png \
            \( -clone 0 -resize 16x16 \) \( -clone 0 -resize 32x32 \) \
            \( -clone 0 -resize 48x48 \) \( -clone 0 -resize 64x64 \) \
            favicon.ico

SRC_ROOT=$(shell cd ..; pwd)
SML_LIB=$(SRC_ROOT)/js
SMLTOJS=SML_LIB=$(SML_LIB) $(SRC_ROOT)/bin/smltojs
CODEMIRRORFLAGS=-jslibs "../../js/codemirror/codemirror.js"
JS_PATH_FLAGS=-js_path_compress -js_path_prefix "src/Compiler" -js_path_relative_to $(SRC_ROOT)
JSDOCFILES=js/doc/*.{html,css,js}
DOJO=/Users/mael/dojo-release-1.9.11
# Before running "make smltojsonline", make sure to run "make smltojs; make smltojs_basislibs"
# in the parent directory; otherwise, certain js-files will be missing.
# For trying smltojsonline on http://localhost:8000, first start a web-server in the smltojsonline directory:
#   python -m SimpleHTTPServer 8000

smltojsonline: Makefile doc favicon.ico
	rm -rf smltojsonline
	(cd ../src/Compiler; $(SMLTOJS) -aopt $(JS_PATH_FLAGS) $(CODEMIRRORFLAGS) -o index smltojs0.mlb)
	mkdir -p smltojsonline
	cp -p ../src/Compiler/index.html smltojsonline/
	(cd ..; tar c `sed 's/.*src="\(.*\)".*/\1/g' < js/smltojsonline/index.html | grep '\.js'` $(THEBASISFILES) $(JSDOCFILES) $(CODEMIRRORFILES) | tar xv -C js/smltojsonline/)
	cp -p appfun/dropbox.png appfun/nodropbox.png appfun/appfunstyle.css smltojsonline/
	cp -pa $(DOJO)/dojo $(DOJO)/dijit smltojsonline/
	cp -pa test/appfuntest/smltojs_logo_transparent_small.png smltojsonline/
	cp favicon.ico smltojsonline/
	mkdir -p smltojsonline/otests
	cp -p otests/content smltojsonline/otests/
	(cd otests; for f in `ls *.sml`; do cp -p $${f} ../smltojsonline/otests/$${f}_; done)

# Before running "make smltojs_web", make sure to run "make smltojs; make smltojs_basislibs"
# in the parent directory; otherwise, certain js-files are missing and will not be
# part of the packaged tar-file.
smltojs_web: Makefile doc
	(cd ../src/Compiler; $(SMLTOJS) -aopt $(JS_PATH_FLAGS) $(CODEMIRRORFLAGS) -o index smltojs0.mlb)
	(cd ..; cp src/Compiler/index.html .)
	(cd ..; tar czvf js/smltojs_web.tgz `sed 's/.*src="\(.*\)".*/\1/g' < index.html | grep '\.js'` index.html js/appfun/style.css $(THEBASISFILES) js/smltojs_logo_color160.png $(JSDOCFILES) $(CODEMIRRORFILES))

codemirror: Makefile
	(cd codemirror/sml; $(SMLTOJS) -c -aopt codemirror.mlb)

TMPTARGETDIR=/Users/mael/tmp2
.PHONY: buildanduntar
buildanduntar: smltojs_web
	cp smltojs_web.tgz $(TMPTARGETDIR) && cd $(TMPTARGETDIR) && tar xzf smltojs_web.tgz

.PHONY: doc
doc:
	(cd doc; make)

.PHONY: clean
clean:
	$(MAKE) -C doc clean
	$(MAKE) -C test clean
	$(MAKE) -C basis clean
	$(MAKE) -C codemirror clean
	rm -f *~ smltojs.man.html

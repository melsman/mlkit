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
SMLTOJS_LIB=$(SRC_ROOT)/js
SMLTOJS=SML_LIB=$(SMLTOJS_LIB) $(SRC_ROOT)/bin/smltojs
CODEMIRRORFLAGS=-jslibs "../../js/codemirror/codemirror.js"
JS_PATH_FLAGS=-js_path_compress -js_path_prefix "src/Compiler" -js_path_relative_to $(SRC_ROOT)
JSDOCFILES=js/doc/*.{html,css,js} js/doc/images/*.png
DOJO=/Users/mael/dojo-release-1.9.1

IDE_BASISLIBS=Initial General Option List ListPair Vector VectorSlice	\
   Array ArraySlice Array2 ByteTable ByteSlice StringCvt String2	\
   Substring Text Bool IntInfRep Word32 Word8 Word31 Pack32Little	\
   Pack32Big Byte Int32 Int31 Math Real IntInf Time Random Path Date	\
   Timer TextIO JsCore Js Html Rwp XMLrpcClient dojo formlets utest
IDE_BASISLIBS_PATHS=$(IDE_BASISLIBS:%=js/basis/MLB/Js/%.sml.o.eb.js)
IDE_EXTRACODE=Array2-sml ArraySlice-sml-code1 ArraySlice-sml-code3	\
   VectorSlice-sml-code1 VectorSlice-sml-code3 ByteTable-sml-code11	\
   ByteTable-sml-code14 ByteTable-sml-code16 ByteTable-sml-code17	\
   ByteTable-sml-code20 ByteTable-sml-code22 ByteTable-sml-code3	\
   ByteTable-sml-code5 ByteTable-sml-code6 ByteTable-sml-code9		\
   ByteSlice-sml-code1 ByteSlice-sml-code10 ByteSlice-sml-code12	\
   ByteSlice-sml-code3 ByteSlice-sml-code4 ByteSlice-sml-code6		\
   ByteSlice-sml-code7 ByteSlice-sml-code9 Bool-sml Char-sml Byte-sml	\
   StrBase-sml Math-sml Html-sml Rwp-sml Parsercomb-sml			\
   XMLrpcClient-sml-code1 XMLrpcClient-sml-code2			\
   XMLrpcClient-sml-code3 utest-sml Random-sml
IDE_EXTRACODE_PATHS=$(IDE_EXTRACODE:%=js/basis/MLB/Js/%.js)

#
# We bundle javascript files together in two bundles, one for code to
# be used for starting the app (bundle.js) and one for holding basis
# environment bindings and basis library code that the compiler is not
# using (bundle_extra.js).  The bundle_extra.js file needs to be kept
# in sync with the mentioned basislibs files in SmlToJsCompApp.sml.
#
# Before running "make smltojsonline", make sure to run "make smltojs;
# make smltojs_basislibs" in the parent directory; otherwise, certain
# js-files will be missing.  For trying smltojsonline on
# http://localhost:8000, first start a web-server in the smltojsonline
# directory:
#
#   python -m SimpleHTTPServer 8000
# or
#   python3 -m http.server 8000
#

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
	(cd otests; cp -p *.png ../smltojsonline/otests)
	(cd smltojsonline; cat index.html | grep 'script type' | sed -e 's/^.*src=\"\([^"]*\)".*$$/\1/' | xargs cat > bundle.js)
	(cd smltojsonline; mv index.html index-split.html)
	(cd smltojsonline; echo '<script type="text/javascript" src="bundle.js"></script>' > index.html)
	(cd smltojsonline; cat $(IDE_BASISLIBS_PATHS) $(IDE_EXTRACODE_PATHS) > bundle_extra.js)

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

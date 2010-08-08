HOST=tiger.itu.dk
TDIR=smlserver.org/www/smltojs
publish:
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

publishweb: 
	scp slides_diku_2007-11-27.pdf slides_lunch_2007-09-17.pdf base.css index.html smltojs_logo_color.png $(HOST):$(TDIR)/

smltojs.man.html: Makefile
	man2html -r ../man/man1/smltojs.1 > smltojs.man.html
#	rman -f HTML -r '' ../man/man1/smltojs.1 > smltojs.man.html

BASISFILES=Initial General Option List ListPair \
 Vector VectorSlice Array ArraySlice Array2 ByteTable ByteSlice \
 StringCvt String2 Substring Text Bool IntInfRep \
 Word32 Word8 Word31 Pack32Little Pack32Big Byte \
 Int32 Int31 Math Real IntInf \
 Time Random Path Date Timer TextIO \
 JsCore Js Html Rwp

THEBASISFILES = $(BASISFILES:%=js/basis/MLB/Js/%.sml.o.eb.js)

smltojs_web: Makefile doc
	(cd ../src/Compiler; SML_LIB=../../js ../../bin/smltojs -aopt -js_path_compress -js_path_prefix "src/Compiler" -o index smltojs0.mlb)
	(cd ..; cp src/Compiler/index.html .)
	(cd ..; tar czvf js/smltojs_web.tgz `sed 's/.*src="\(.*\)".*/\1/g' < index.html | grep '\.js'` index.html $(THEBASISFILES) js/smltojs_logo_color160.png js/doc/*.html)

.PHONY: doc
doc:
	(cd doc; make)

.PHONY: clean
clean:
	$(MAKE) -C test clean
	$(MAKE) -C basis clean
	rm -f *~ smltojs.man.html
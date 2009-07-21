HOST=ssh.itu.dk
TDIR=public_html/smltojs
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
	scp slides_diku_2007-11-27.pdf slides_lunch_2007-09-17.pdf base.css index.php smltojs_logo_color.png $(HOST):$(TDIR)/

smltojs.man.html: Makefile
	man2html -r ../man/man1/smltojs.1 > smltojs.man.html
#	rman -f HTML -r '' ../man/man1/smltojs.1 > smltojs.man.html

smltojs_web: Makefile
	(cd ../src/Compiler; SML_LIB=../../js ../../bin/smltojs -js_path_compress -js_path_prefix "src/Compiler" smltojs0.mlb)
	(cd ..; cp src/Compiler/run.html .)
	(cd ..; tar czvf js/smltojs_web.tgz `sed 's/.*src="\(.*\)".*/\1/g' < run.html | grep '\.js'` run.html)

clean:
	$(MAKE) -C test clean
	$(MAKE) -C basis clean
	rm -f *~ smltojs.man.html
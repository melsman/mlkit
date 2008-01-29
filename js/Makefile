HOST=ssh.itu.dk
TDIR=public_html/smltojs
publish:
	$(MAKE) -C test all
	ssh $(HOST) rm -rf $(TDIR)/basis $(TDIR)/test
	ssh $(HOST) mkdir -p $(TDIR)/basis/MLB/Js
	scp -r basis/MLB/Js/*.js $(HOST):$(TDIR)/basis/MLB/Js/
	ssh $(HOST) mkdir -p $(TDIR)/test/MLB/Js
	scp -r test/MLB/Js/*.js $(HOST):$(TDIR)/test/MLB/Js/
	scp -r test/*.{html,sml,sig} $(HOST):$(TDIR)/test/
	scp prims.js $(HOST):$(TDIR)/
	$(MAKE) publishweb

publishweb: smltojs.man.html
	scp smltojs.man.html slides_diku_2007-11-27.pdf slides_lunch_2007-09-17.pdf base.css index.php smltojs_logo_color.png $(HOST):$(TDIR)/

smltojs.man.html: Makefile
	man2html -r ../man/man1/smltojs.1 > smltojs.man.html
#	rman -f HTML -r '' ../man/man1/smltojs.1 > smltojs.man.html

clean:
	$(MAKE) -C test clean
	rm -f *~ smltojs.man.html
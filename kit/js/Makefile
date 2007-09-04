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

clean:
	$(MAKE) -C test clean
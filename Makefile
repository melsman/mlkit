SHELL=/bin/sh

KITVERSION=3.4.0
ARCH-OS=x86-linux
INSTDIR=/usr/local/mlkit
INSTDIR_KAM=/usr/local/mlkit-kam-$(KITVERSION)

# Some commands
MKDIR=mkdir -p
INSTALL=cp -p

mlkit:
	cd src; $(MAKE)

mlkit_kam:
	cd src; $(MAKE) mlkit_kam

mlkit_web:
	cd src; $(MAKE) mlkit_web

mlkit_hppa:
	cd src; $(MAKE) mlkit_hppa

clean:
	rm -rf *~ bin PM run
	cd basislib; rm -rf PM run *~
	cd kitlib; rm -rf PM run *~
	cd ml-yacc-lib; rm -rf PM *~
	cd kitdemo; rm -rf PM run *~ */PM */*~
	cd test; rm -rf PM run *~ *.out *.log */PM */*~
	cd test_dev; rm -rf PM run *~ *.out *.log
	cd smlserver_demo; rm -rf *~
	cd smlserver_demo/sml; rm -rf *~ PM 
	cd smlserver_demo/www; rm -rf *~ PM
	cd src; $(MAKE) clean

tgz_export:
	cd ..; rm -rf mlkit-$(KITVERSION) mlkit-$(KITVERSION).tgz
	cd ..; cvs -d linux.it.edu:/cvsroot -q export -D now -d mlkit-$(KITVERSION) mlkit/kit
	cd ..; tar czf mlkit-$(KITVERSION).tgz mlkit-$(KITVERSION)
	cd ..; rm -rf mlkit-$(KITVERSION)

tgz:
	cd ..; rm -rf mlkit-$(KITVERSION) mlkit-$(KITVERSION).tgz
	cd ..; cp -d -f -p -R kit mlkit-$(KITVERSION)
	cd ../mlkit-$(KITVERSION); $(MAKE) clean
	cd ../mlkit-$(KITVERSION); rm -rf CVS */CVS */*/CVS */*/*/CVS */*/*/*/CVS */*/*/*/*/CVS */*/*/*/*/*/CVS
	cd ..; tar czf mlkit-$(KITVERSION).tgz mlkit-$(KITVERSION)
	cd ..; rm -rf mlkit-$(KITVERSION)

install:
	rm -rf $(INSTDIR)
	$(MKDIR) $(INSTDIR)
	$(MKDIR) $(INSTDIR)/bin
	$(MKDIR) $(INSTDIR)/doc
	$(INSTALL) bin/runtimeSystem.o $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGC.o $(INSTDIR)/bin
	$(INSTALL) bin/rp2ps $(INSTDIR)/bin
	$(INSTALL) bin/mlkit.$(ARCH-OS) $(INSTDIR)/bin
	$(INSTALL) copyright $(INSTDIR)
	$(INSTALL) README $(INSTDIR)
	$(INSTALL) -R kitdemo $(INSTDIR)/kitdemo 
	$(INSTALL) -R ml-yacc-lib $(INSTDIR)/ml-yacc-lib
	$(INSTALL) -R basislib $(INSTDIR)/basislib
	$(INSTALL) doc/manual/mlkit.pdf $(INSTDIR)/doc
	chown -R root.root $(INSTDIR)
	chmod -R ug+rw $(INSTDIR)
	chmod -R o+r $(INSTDIR)
#
# The following is also done in the %post section in the rpm file, 
# because the --prefix option to rpm can change the installation 
# directory! 
#
	echo '#!/bin/sh' > $(INSTDIR)/bin/mlkit
	echo -e '$(INSTDIR)/bin/mlkit.$(ARCH-OS) $(INSTDIR) $$*' >> $(INSTDIR)/bin/mlkit
	chmod a+x $(INSTDIR)/bin/mlkit

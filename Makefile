SHELL=/bin/sh

KITVERSION=3.4.0
ARCH-OS=x86-linux
INSTDIR=/usr/local/mlkit
INSTDIR_KAM=/usr/local/mlkit_kam
INSTDIR_WEB=/usr/local/smlserver

# Some commands
MKDIR=mkdir -p
INSTALL=cp -p

mlkit:
	$(MKDIR) bin
	cd src; $(MAKE)

mlkit_kam:
	$(MKDIR) bin
	cd src; $(MAKE) mlkit_kam

smlserver:
	$(MKDIR) bin
	cd src; $(MAKE) smlserver

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

tgz_smlserver:
	cd ..; rm -rf smlserver-$(KITVERSION) smlserver-$(KITVERSION).tgz
	cd ..; cp -d -f -p -R kit smlserver-$(KITVERSION)
	cd ../smlserver-$(KITVERSION); $(MAKE) clean
	cd ../smlserver-$(KITVERSION); rm -rf CVS */CVS */*/CVS */*/*/CVS */*/*/*/CVS */*/*/*/*/CVS */*/*/*/*/*/CVS
	cd ..; tar czf smlserver-$(KITVERSION).tgz smlserver-$(KITVERSION)
	cd ..; rm -rf smlserver-$(KITVERSION)

install:
	rm -rf $(INSTDIR)
	$(MKDIR) $(INSTDIR)
	$(MKDIR) $(INSTDIR)/bin
	$(MKDIR) $(INSTDIR)/doc
	$(INSTALL) bin/runtimeSystem.o $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGC.o $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGCProf.o $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemProf.o $(INSTDIR)/bin
	$(INSTALL) bin/rp2ps $(INSTDIR)/bin
	$(INSTALL) bin/mlkit.$(ARCH-OS) $(INSTDIR)/bin
	$(INSTALL) copyright $(INSTDIR)
	$(INSTALL) README $(INSTDIR)
	$(INSTALL) -R kitdemo $(INSTDIR)/kitdemo 
	$(INSTALL) -R ml-yacc-lib $(INSTDIR)/ml-yacc-lib
	$(INSTALL) -R basislib $(INSTDIR)/basislib
	$(INSTALL) doc/manual/mlkit.pdf $(INSTDIR)/doc
	chown -R `whoami`.`whoami` $(INSTDIR)
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

# The following is necessary if you want to either run kittester
# or bootstrap the Kit.
bootstrap:
	$(INSTALL) -a test $(INSTDIR)/test
	$(INSTALL) -a src $(INSTDIR)/src
	cd $(INSTDIR)/src; make clean
	$(INSTALL) bin/kittester.$(ARCH-OS) $(INSTDIR)/bin
	echo -e 'sml @SMLload=$(INSTDIR)/bin/kittester.$(ARCH-OS) $$*' >> $(INSTDIR)/bin/kittester
	chmod a+x $(INSTDIR)/bin/kittester
	chown -R `whoami`.`whoami` $(INSTDIR)
	chmod -R ug+rw $(INSTDIR)
	chmod -R o+r $(INSTDIR)
	echo -e '# Some commands' >! $(INSTDIR)/Makefile
	echo -e 'ARCH-OS=x86-linux' >> $(INSTDIR)/Makefile
	echo -e 'MKDIR=mkdir -p' >> $(INSTDIR)/Makefile
	echo -e 'INSTALL=cp -p' >> $(INSTDIR)/Makefile
	echo -e '' >> $(INSTDIR)/Makefile
	echo -e 'bootstrap:' >> $(INSTDIR)/Makefile
	echo -e '	cd src; ../bin/mlkit -gc sources.pm' >> $(INSTDIR)/Makefile
	echo -e '	$(MKDIR) $(INSTDIR2)' >> $(INSTDIR)/Makefile
	echo -e '	$(MKDIR) $(INSTDIR2)/bin' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) bin/runtimeSystem.o $(INSTDIR2)/bin' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) bin/runtimeSystemGC.o $(INSTDIR2)/bin' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) bin/runtimeSystemGCProf.o $(INSTDIR2)/bin' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) bin/runtimeSystemProf.o $(INSTDIR2)/bin' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) bin/rp2ps $(INSTDIR2)/bin' >> $(INSTDIR2)/Makefile
	echo -e '	$(INSTALL) src/run $(INSTDIR2)/bin/mlkit' >> $(INSTDIR2)/Makefile
	echo -e '	$(INSTALL) bin/kittester.$(ARCH-OS) $(INSTDIR2)/bin' >> $(INSTDIR)/Makefile
	echo -e "	echo -e 'sml @SMLload=$(INSTDIR2)/bin/kittester.$(ARCH-OS) $$*' >> $(INSTDIR2)/bin/kittester" >> $(INSTDIR)/Makefile
	echo -e '	chmod a+x $(INSTDIR2)/bin/kittester' >> $(INSTDIR)/Makefile

	echo -e '	$(INSTALL) copyright $(INSTDIR2)' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) README $(INSTDIR2)' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) -R kitdemo $(INSTDIR2)/kitdemo ' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) -a test $(INSTDIR2)/test' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) -a src $(INSTDIR2)/src' >> $(INSTDIR)/Makefile
	echo -e '	cd $(INSTDIR2)/src; make clean' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) -R ml-yacc-lib $(INSTDIR2)/ml-yacc-lib' >> $(INSTDIR)/Makefile
	echo -e '	$(INSTALL) -R basislib $(INSTDIR2)/basislib' >> $(INSTDIR)/Makefile
	echo -e '	chown -R `whoami`.`whoami` $(INSTDIR2)' >> $(INSTDIR)/Makefile
	echo -e '	chmod -R ug+rw $(INSTDIR2)' >> $(INSTDIR)/Makefile
	echo -e '	chmod -R o+r $(INSTDIR2)' >> $(INSTDIR)/Makefile
	echo -e '	chmod a+x $(INSTDIR2)/bin/mlkit' >> $(INSTDIR)/Makefile
	echo -e '' >> $(INSTDIR)/Makefile
	echo -e 'all_test:' >> $(INSTDIR)/Makefile
	echo -e '	cd test; ../bin/kittester ../bin/mlkit all.tst' >> $(INSTDIR)/Makefile
	echo -e '' >> $(INSTDIR)/Makefile
	echo -e 'all: all_test bootstrap' >> $(INSTDIR)/Makefile

install_smlserver:
	rm -rf $(INSTDIR_WEB)
	$(MKDIR) $(INSTDIR_WEB)
	$(MKDIR) $(INSTDIR_WEB)/bin
	$(MKDIR) $(INSTDIR_WEB)/doc
	$(INSTALL) bin/mlkit_web.$(ARCH-OS) $(INSTDIR_WEB)/bin
	$(INSTALL) src/SMLserver/nssml.so $(INSTDIR_WEB)/bin
	$(INSTALL) copyright $(INSTDIR_WEB)
	$(INSTALL) README $(INSTDIR_WEB)
	$(INSTALL) README_SMLSERVER $(INSTDIR_WEB)
	$(INSTALL) -R smlserver_demo $(INSTDIR_WEB)/smlserver_demo 
	$(INSTALL) -R basislib $(INSTDIR_WEB)/basislib
	$(INSTALL) doc/manual/mlkit.pdf $(INSTDIR_WEB)/doc
	chown -R `whoami`.`whoami` $(INSTDIR)
	chmod -R ug+rw $(INSTDIR_WEB)
	chmod -R o+r $(INSTDIR_WEB)
#
# The following is also done in the %post section in the rpm file, 
# because the --prefix option to rpm can change the installation 
# directory! 
#
	echo '#!/bin/sh' > $(INSTDIR_WEB)/bin/mlkit_web
	echo -e '$(INSTDIR_WEB)/bin/mlkit_web.$(ARCH-OS) $(INSTDIR_WEB) $$*' >> $(INSTDIR_WEB)/bin/mlkit_web
	chmod a+x $(INSTDIR_WEB)/bin/mlkit_web


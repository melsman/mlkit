SHELL=/bin/sh

KITVERSION=4.1.4
ARCH-OS=x86-linux
#ARCH-OS=x86-bsd
INSTDIR=/usr/share/mlkit
INSTDIR_KAM=/usr/share/mlkit_kam
INSTDIR_BARRY=/usr/share/barry
INSTDIR_SMLSERVER=/usr/share/smlserver
RPMDIR=/usr/src/rpm
#RPMDIR=/usr/src/redhat

# Some commands
MKDIR=mkdir -p
INSTALL=cp -p

CLEAN=rm -rf PM CM *~ .\#*

.PHONY: smlserver
mlkit:
	$(MKDIR) bin
	cd src; $(MAKE)

mlkit_kam:
	$(MKDIR) bin
	cd src; $(MAKE) mlkit_kam

smlserver:
	$(MKDIR) bin
	cd src; $(MAKE) smlserver

barry:
	$(MKDIR) bin
	cd src; $(MAKE) barry

clean:
	$(CLEAN) bin run
	cd basislib; $(MAKE) clean
	cd doc/manual; $(MAKE) clean
	cd kitlib; $(CLEAN) run
	cd ml-yacc-lib; $(CLEAN)
	cd kitdemo; $(CLEAN) run */PM */*~
	cd test; $(MAKE) clean
	cd test_dev; $(CLEAN) run *.out *.log
	cd src; $(MAKE) clean
	cd smlserver_demo; $(CLEAN) sources.pm nsd.mael.tcl
	cd smlserver_demo/lib; $(CLEAN)
	cd smlserver_demo/demo; $(CLEAN) 
	cd smlserver_demo/demo/rating; $(CLEAN) 
	cd smlserver_demo/demo/employee; $(CLEAN) 
	cd smlserver_demo/demo/link; $(CLEAN) 
	cd smlserver_demo/demo_lib; $(CLEAN) 
	cd smlserver_demo/demo_lib/orasql; $(CLEAN) 
	cd smlserver_demo/demo_lib/pgsql; $(CLEAN) 
	cd smlserver_demo/scs_lib; $(CLEAN) 
	cd smlserver_demo/scs_lib/pgsql; $(CLEAN)
	cd smlserver_demo/log; rm -f server.log access* nspid*
	cd smlserver_demo/www; $(CLEAN)
	cd smlserver_demo/www/demo; $(CLEAN)
	cd smlserver_demo/www/demo/rating; $(CLEAN)
	cd smlserver_demo/www/demo/link; $(CLEAN)
	cd smlserver_demo/www/demo/employee; $(CLEAN)
	cd smlserver; $(CLEAN)
	cd smlserver/xt; $(CLEAN)
	cd smlserver/xt/demolib; $(CLEAN)
	cd smlserver/xt/libxt; $(CLEAN)
	cd smlserver/xt/www; $(CLEAN)

tgz_export:
	cd ..; rm -rf mlkit-$(KITVERSION) mlkit-$(KITVERSION).tgz
	cd ..; cvs -d linux.it.edu:/cvsroot -q export -D now -d mlkit-$(KITVERSION) mlkit/kit
	cd ..; tar czf mlkit-$(KITVERSION).tgz mlkit-$(KITVERSION)
	cd ..; rm -rf mlkit-$(KITVERSION)

tgz:
	cd ..; rm -rf mlkit-$(KITVERSION) mlkit-$(KITVERSION).tgz
	cd ..; cp -d -f -p -R kit mlkit-$(KITVERSION)
	cd ../mlkit-$(KITVERSION); $(MAKE) clean
	cd ../mlkit-$(KITVERSION); rm -rf test_dev
	cd ../mlkit-$(KITVERSION); rm -rf CVS */CVS */*/CVS */*/*/CVS */*/*/*/CVS */*/*/*/*/CVS */*/*/*/*/*/CVS
	cd ../mlkit-$(KITVERSION); rm -rf .cvsignore */.cvsignore */*/.cvsignore \
           */*/*/.cvsignore */*/*/*/.cvsignore */*/*/*/*/.cvsignore \
           */*/*/*/*/*/.cvsignore
	cd ..; tar czf mlkit-$(KITVERSION).tgz mlkit-$(KITVERSION)
	cd ..; rm -rf mlkit-$(KITVERSION)

tgz_smlserver:
	cd ..; rm -rf smlserver-$(KITVERSION) smlserver-$(KITVERSION).tgz
	cd ..; cp -d -f -p -R kit smlserver-$(KITVERSION)
	cd ../smlserver-$(KITVERSION); $(MAKE) clean
	cd ../smlserver-$(KITVERSION); rm -rf test_dev
	cd ../smlserver-$(KITVERSION); rm -rf CVS */CVS */*/CVS */*/*/CVS */*/*/*/CVS */*/*/*/*/CVS */*/*/*/*/*/CVS
	cd ../smlserver-$(KITVERSION); rm -rf .cvsignore */.cvsignore */*/.cvsignore \
           */*/*/.cvsignore */*/*/*/.cvsignore */*/*/*/*/.cvsignore \
           */*/*/*/*/*/.cvsignore
	cd ..; tar czf smlserver-$(KITVERSION).tgz smlserver-$(KITVERSION)
	cd ..; rm -rf smlserver-$(KITVERSION)

rpm_smlserver:
	# assume that ``make tgz_smlserver'' has been run 
	# as a user other than root!
	cp -f ../smlserver-$(KITVERSION).tgz $(RPMDIR)/SOURCES/
	cp -f smlserver.spec $(RPMDIR)/SPECS/smlserver-$(KITVERSION).spec
	(cd $(RPMDIR)/SPECS; rpm -ba smlserver-$(KITVERSION).spec)

rpm:
	# assume that ``make tgz'' has been run 
	# as a user other than root!
	cp -f ../mlkit-$(KITVERSION).tgz $(RPMDIR)/SOURCES/
	cp -f mlkit.spec $(RPMDIR)/SPECS/mlkit-$(KITVERSION).spec
	(cd $(RPMDIR)/SPECS; rpm -ba mlkit-$(KITVERSION).spec)

install:
	rm -rf $(INSTDIR)
	$(MKDIR) $(INSTDIR)
	$(MKDIR) $(INSTDIR)/bin
	$(MKDIR) $(INSTDIR)/doc
	$(INSTALL) bin/runtimeSystem.a $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemProf.a $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGC.a $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGCProf.a $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGenGC.a $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGenGCProf.a $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGCTP.a $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemGCTPProf.a $(INSTDIR)/bin
	$(INSTALL) bin/runtimeSystemTag.a $(INSTDIR)/bin
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
	rm -f /usr/bin/mlkit /usr/bin/rp2ps
	cp -f -p $(INSTDIR)/bin/mlkit /usr/bin/mlkit
	cp -f -p $(INSTDIR)/bin/rp2ps /usr/bin/rp2ps

# The following is necessary if you want to either run kittester
# or bootstrap the Kit.
bootstrap:
	$(INSTALL) -a test $(INSTDIR)/test
	cd $(INSTDIR)/test; ln -sf README testlink
	cd $(INSTDIR)/test; ln -sf testcycl testcycl
	cd $(INSTDIR)/test; ln -sf exists.not testbadl
	cd $(INSTDIR)/test; echo -e 'hardlinkA' >> hardlinkA
	cd $(INSTDIR)/test; ln -f hardlinkA hardlinkB
	$(INSTALL) -a src $(INSTDIR)/src
	cd $(INSTDIR)/src; $(MAKE) clean
	$(INSTALL) bin/kittester.$(ARCH-OS) $(INSTDIR)/bin
	echo -e 'sml @SMLload=$(INSTDIR)/bin/kittester.$(ARCH-OS) $$*' >> $(INSTDIR)/bin/kittester
	chmod a+x $(INSTDIR)/bin/kittester
	chown -R `whoami`.`whoami` $(INSTDIR)
	chmod -R ug+rw $(INSTDIR)
	chmod -R o+r $(INSTDIR)
	$(INSTALL) test/Makefile_bootstrap $(INSTDIR)/Makefile

install_kam:
	rm -rf $(INSTDIR_KAM)
	$(MKDIR) $(INSTDIR_KAM)
	$(MKDIR) $(INSTDIR_KAM)/bin
	$(MKDIR) $(INSTDIR_KAM)/doc
	$(INSTALL) bin/mlkit_kam.$(ARCH-OS) $(INSTDIR_KAM)/bin
	$(INSTALL) bin/kam $(INSTDIR_KAM)/bin
	$(INSTALL) copyright $(INSTDIR_KAM)
	$(INSTALL) README $(INSTDIR_KAM)
	$(INSTALL) -R kitdemo $(INSTDIR_KAM)/kitdemo 
	$(INSTALL) -R ml-yacc-lib $(INSTDIR_KAM)/ml-yacc-lib
	$(INSTALL) -R basislib $(INSTDIR_KAM)/basislib
	$(INSTALL) doc/manual/mlkit.pdf $(INSTDIR_KAM)/doc
	chown -R `whoami`.`whoami` $(INSTDIR_KAM)
	chmod -R ug+rw $(INSTDIR_KAM)
	chmod -R o+r $(INSTDIR_KAM)

	echo '#!/bin/sh' > $(INSTDIR_KAM)/bin/mlkit_kam
	echo -e '$(INSTDIR_KAM)/bin/mlkit_kam.$(ARCH-OS) $(INSTDIR_KAM) $$*' >> $(INSTDIR_KAM)/bin/mlkit_kam
	chmod a+x $(INSTDIR_KAM)/bin/mlkit_kam
	rm -f /usr/bin/mlkit_kam
	cp -f -p $(INSTDIR_KAM)/bin/mlkit_kam /usr/bin/mlkit_kam

install_barry:
	rm -rf $(INSTDIR_BARRY)
	$(MKDIR) $(INSTDIR_BARRY)
	$(MKDIR) $(INSTDIR_BARRY)/bin
	$(MKDIR) $(INSTDIR_BARRY)/doc
	$(INSTALL) bin/barry.$(ARCH-OS) $(INSTDIR_BARRY)/bin
	$(INSTALL) copyright $(INSTDIR_BARRY)
	$(INSTALL) README $(INSTDIR_BARRY)
	$(INSTALL) README_BARRY $(INSTDIR_BARRY)
	$(INSTALL) -R kitdemo $(INSTDIR_BARRY)/kitdemo 
	$(INSTALL) -R ml-yacc-lib $(INSTDIR_BARRY)/ml-yacc-lib
	$(INSTALL) -R basislib $(INSTDIR_BARRY)/basislib
	$(INSTALL) doc/manual/mlkit.pdf $(INSTDIR_BARRY)/doc
	chown -R `whoami`.`whoami` $(INSTDIR_BARRY)
	chmod -R ug+rw $(INSTDIR_BARRY)
	chmod -R o+r $(INSTDIR_BARRY)

	echo '#!/bin/sh' > $(INSTDIR_BARRY)/bin/barry
	echo -e '$(INSTDIR_BARRY)/bin/barry.$(ARCH-OS) $(INSTDIR_BARRY) $$*' >> $(INSTDIR_BARRY)/bin/barry
	chmod a+x $(INSTDIR_BARRY)/bin/barry
	rm -f /usr/bin/barry
	cp -f -p $(INSTDIR_BARRY)/bin/barry /usr/bin/barry

install_smlserver:
	rm -rf $(INSTDIR_SMLSERVER)
	$(MKDIR) $(INSTDIR_SMLSERVER)
	$(MKDIR) $(INSTDIR_SMLSERVER)/bin
	$(MKDIR) $(INSTDIR_SMLSERVER)/doc
	$(INSTALL) bin/smlserverc.$(ARCH-OS) $(INSTDIR_SMLSERVER)/bin
	$(INSTALL) src/SMLserver/nssml.so $(INSTDIR_SMLSERVER)/bin
	$(INSTALL) copyright $(INSTDIR_SMLSERVER)
	$(INSTALL) README $(INSTDIR_SMLSERVER)
	$(INSTALL) README_SMLSERVER $(INSTDIR_SMLSERVER)
	$(INSTALL) NEWS_SMLSERVER $(INSTDIR_SMLSERVER)
	$(INSTALL) -R smlserver/xt $(INSTDIR_SMLSERVER)/xt
	$(INSTALL) -R smlserver_demo $(INSTDIR_SMLSERVER)/smlserver_demo 
	$(INSTALL) -R basislib $(INSTDIR_SMLSERVER)/basislib
#	$(INSTALL) doc/manual/mlkit.pdf $(INSTDIR_SMLSERVER)/doc
	$(INSTALL) doc/smlserver.pdf $(INSTDIR_SMLSERVER)/doc
	chown -R `whoami`.`whoami` $(INSTDIR_SMLSERVER)
	chmod -R ug+rw $(INSTDIR_SMLSERVER)
	chmod -R o+r $(INSTDIR_SMLSERVER)
#
# The following is also done in the %post section in the rpm file, 
# because the --prefix option to rpm can change the installation 
# directory! 
#
	echo '#!/bin/sh' > $(INSTDIR_SMLSERVER)/bin/smlserverc
	echo -e '$(INSTDIR_SMLSERVER)/bin/smlserverc.$(ARCH-OS) $(INSTDIR_SMLSERVER) $$*' >> $(INSTDIR_SMLSERVER)/bin/smlserverc
	chmod a+x $(INSTDIR_SMLSERVER)/bin/smlserverc
	rm -f /usr/bin/smlserverc
	cp -f -p $(INSTDIR_SMLSERVER)/bin/smlserverc /usr/bin/smlserverc

SMLSERVER_HOST=hug.it.edu
SMLSERVER_HOSTDIR=$(SMLSERVER_HOST):/web/smlserver/www/dist

dist_smlserver:
	scp NEWS_SMLSERVER $(SMLSERVER_HOSTDIR)/NEWS_SMLSERVER-$(KITVERSION).txt
	scp README_SMLSERVER $(SMLSERVER_HOSTDIR)/README_SMLSERVER-$(KITVERSION).txt
	scp doc/smlserver.pdf $(SMLSERVER_HOSTDIR)/smlserver-$(KITVERSION).pdf
	scp ../smlserver-$(KITVERSION).tgz $(SMLSERVER_HOSTDIR)/
	scp $(RPMDIR)/RPMS/i386/smlserver-$(KITVERSION)-1.i386.rpm $(SMLSERVER_HOSTDIR)/
	scp $(RPMDIR)/SRPMS/smlserver-$(KITVERSION)-1.src.rpm $(SMLSERVER_HOSTDIR)/


MLKIT_HOST=ssh.it.edu
MLKIT_HOSTDIR=$(MLKIT_HOST):/import/www/research/mlkit/dist
TESTDATE=2002-08-25

dist_mlkit:
	scp NEWS $(MLKIT_HOSTDIR)/NEWS-$(KITVERSION).txt
	scp README $(MLKIT_HOSTDIR)/README-$(KITVERSION).txt
	scp INSTALL $(MLKIT_HOSTDIR)/INSTALL-$(KITVERSION).txt
	scp doc/manual/mlkit.pdf $(MLKIT_HOSTDIR)/mlkit-$(KITVERSION).pdf
	scp test/test_report-native-$(TESTDATE).dvi $(MLKIT_HOSTDIR)/test_report-native-$(KITVERSION).dvi
	scp test/test_report-kam-$(TESTDATE).dvi $(MLKIT_HOSTDIR)/test_report-kam-$(KITVERSION).dvi
	scp ../mlkit-$(KITVERSION).tgz $(MLKIT_HOSTDIR)/
	scp $(RPMDIR)/RPMS/i386/mlkit-$(KITVERSION)-1.i386.rpm $(MLKIT_HOSTDIR)/
	scp $(RPMDIR)/SRPMS/mlkit-$(KITVERSION)-1.src.rpm $(MLKIT_HOSTDIR)/

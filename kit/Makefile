mlkit:
	cd src; $(MAKE)

mlkit_kam:
	cd src; $(MAKE) mlkit_kam

mlkit_web:
	cd src; $(MAKE) mlkit_web

mlkit_hppa:
	cd src; $(MAKE) mlkit_hppa

clean:
	rm -rf *~ bin
	cd basislib; rm -rf PM run *~
	cd kitlib; rm -rf PM run *~
	cd ml-yacc-lib; rm -rf PM *~
	cd kitdemo; rm -rf PM run *~
	cd test; rm -rf PM run *~ *.out *.log
	cd test_dev; rm -rf PM run *~ *.out *.log
	cd smlserver_demo; rm -rf *~
	cd smlserver_demo/sml; rm -rf *~ PM 
	cd smlserver_demo/www; rm -rf *~ PM
	cd src; $(MAKE) clean

tgz: clean
	cd ..; mv kit mlkit-3.2; tar cf mlkit-3.2.tar mlkit-3.2; mv mlkit-3.2 kit; gzip mlkit-3.2.tar; mv mlkit-3.2.tar.gz mlkit-3.2.tgz

install:
	cd src; $(MAKE) install

install_kam:
	cd src; $(MAKE) install_kam

install_web:
	cd src; $(MAKE) install_web

install_hppa:
	cd src; $(MAKE) install_hppa

all:
	(cd src; $(MAKE))

clean:
	rm -rf *~
	(cd basislib; rm -rf PM run *~)
	(cd kitdemo; rm -rf PM run *~)
	(cd test; rm -rf PM run *~ *.out *.log)
	(cd bin; rm -rf kit.* kittester.* mlkit.heap2exec rp2ps *~)
	(cd src; $(MAKE) clean)

tgz: clean
	(cd ..; mv kit mlkit-3.2; tar cf mlkit-3.2.tar mlkit-3.2; mv mlkit-3.2 kit; gzip mlkit-3.2.tar; mv mlkit-3.2.tar.gz mlkit-3.2.tgz)

install:
	(cd src; $(MAKE) install)

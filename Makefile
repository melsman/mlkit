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
	(cd ..; tar cf mlkit-3.1.tar mlkit-3.1; gzip mlkit-3.1.tar; mv mlkit-3.1.tar.gz mlkit-3.1.tgz)

install:
	(cd src; $(MAKE) install)

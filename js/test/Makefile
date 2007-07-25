SML_LIB=..

test1:
	rm -rf MLB ; SML_LIB=.. ../../bin/smltojs test1.sml

testdyn1:
	SML_LIB=$(SML_LIB) ../../bin/smltojs testdyn1.sml

clean:
	rm -rf MLB
	(cd ../basis; rm -rf MLB)
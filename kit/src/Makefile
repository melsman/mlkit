#
# Build the ML Kit including tools, runtime systems, etc.
#

all:
	cd Tools/GenOpcodes; $(MAKE)
	../bin/kitgen_opcodes `pwd`
	cd RuntimeWithGC; $(MAKE)
	cd SMLserver; $(MAKE)
	cd Tools/Tester; $(MAKE)
	cd Tools/Rp2ps; $(MAKE)
	echo 'use "build.sml";' | sml
#	cd heap2exec; $(MAKE)
#	echo 'use "x86-linux.sml";' | sml
#	heap2exec/heap2exec heap2exec/run.x86-linux ../bin/kit.x86-linux ../bin/mlkit.heap2exec

INSTDIR = /usr/local/mlkit

KITDEMO_FILES = `(cd ../kitdemo; ls -w 10000 *.sml *.pm *.c *.script scanfiles)`

MLYACCLIB_FILES = join.sml ml-yacc-lib.pm parser2.sml base.sig          \
lrtable.sml parser1.sml stream.sml

BASISLIB_FILES = `(cd ../basislib; ls -w 10000 *.sml *.pm *.c *.script)`

NOPROF_OFILES = `(cd ../basislib/PM/NoProf; ls -w 10000 *.o *.bdy *.date)`

install:
	rm -rf $(INSTDIR)
	mkdir $(INSTDIR)
	mkdir $(INSTDIR)/runtime
	mkdir $(INSTDIR)/doc
	mkdir $(INSTDIR)/bin
	mkdir $(INSTDIR)/kitdemo
	mkdir $(INSTDIR)/kitdemo/utils
	mkdir $(INSTDIR)/ml-yacc-lib
	mkdir $(INSTDIR)/basislib
	mkdir $(INSTDIR)/basislib/PM
	mkdir $(INSTDIR)/basislib/PM/NoProf

	(cd RuntimeWithGC; cp -p runtimeSystem.o runtimeSystemGC.o $(INSTDIR)/runtime)
	(cd ..; cp -p copyright readme.x86-linux $(INSTDIR))
	(cd ../kitdemo; cp -p $(KITDEMO_FILES) $(INSTDIR)/kitdemo) 
	(cd ../kitdemo/utils; cp -p ListUtils.sml utils.pm $(INSTDIR)/kitdemo/utils) 
	(cd ../ml-yacc-lib; cp -p $(MLYACCLIB_FILES) $(INSTDIR)/ml-yacc-lib)        
	(cd ../bin; cp -p mlkit.heap2exec $(INSTDIR)/bin)
	(cd ../basislib; cp -p $(BASISLIB_FILES) $(INSTDIR)/basislib)
	(cd ../basislib/PM/NoProf; cp -p $(NOPROF_OFILES) $(INSTDIR)/basislib/PM/NoProf)
	cp -p ../doc/manual/manual.ps $(INSTDIR)/doc

# the following is done in the %post section in the rpm file! But why not here???
#	echo '#!/bin/sh' > /usr/local/mlkit/bin/mlkit
#	echo -e '/usr/local/mlkit/bin/mlkit.heap2exec /usr/local/mlkit \44*' >> /usr/local/mlkit/bin/mlkit
#	chmod a+x /usr/local/mlkit/bin/mlkit

#
# Cleaning up the source directories
#
clean:
	rm -rf run PM CM *~
	(cd Common; rm -rf PM CM *~)
	(cd Common/EfficientElab; rm -rf PM CM *~)
	(cd Compiler; rm -rf PM CM *~)
	(cd Parsing; rm -rf PM CM *~)
	(cd Manager; rm -rf PM CM *~)
	(cd Edlib; rm -rf PM CM *~)
	(cd heap2exec; $(MAKE) clean)
	(cd Runtime; $(MAKE) clean)
	(cd RuntimeWithGC; $(MAKE) clean)
	(cd Tools/Tester; rm -rf PM CM *~)
	(cd Tools/Rp2ps; $(MAKE) clean)
	(cd Compiler; rm -rf PM CM *~)
	(cd Compiler/Backend; rm -rf PM CM *~)
	(cd Compiler/Backend/Dummy; rm -rf PM CM *~)
	(cd Compiler/Backend/HpPaRisc; rm -rf PM CM *~)
	(cd Compiler/Backend/X86; rm -rf PM CM *~)
	(cd Compiler/Regions; rm -rf PM CM *~)
	(cd Compiler/C; rm -rf PM CM *~)
	(cd Compiler/Hppa; rm -rf PM CM *~)
	(cd Compiler/Kam; rm -rf PM CM *~)
	(cd Compiler/Cfg; rm -rf PM CM *~)
	(cd Compiler/Lambda; rm -rf PM CM *~)

#
# Support for measuring the code blowup resulting from
# compiling functors in the ML Kit; not used by install!
#

FILE = /home/mael/kit/src/bdys.txt
LINES = /home/mael/kit/src/lines.mael.sml


bdys:
	(cd Common/PM/NoProf; wc -l *.bdy > $(FILE))
	(cd Common/EfficientElab/PM/NoProf; wc -l *.bdy >> $(FILE)) 
	(cd Parsing/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Lambda/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Regions/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Kam/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Cfg/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/C/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Backend/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Backend/HpPaRisc/PM/NoProf; wc -l *.bdy >> $(FILE))	
	(cd Manager/PM/NoProf; wc -l *.bdy >> $(FILE))	

lines:
	(cd Common; cat *.sml > $(LINES))
	(cd Common/EfficientElab; cat *.sml >> $(LINES)) 
	(cd Parsing; cat *.sml >> $(LINES))	
	(cd Compiler; cat *.sml >> $(LINES))	
	(cd Compiler/Lambda; cat *.sml >> $(LINES))	
	(cd Compiler/Regions; cat *.sml >> $(LINES))	
	(cd Compiler/Kam; cat *.sml >> $(LINES))	
	(cd Compiler/Cfg; cat *.sml >> $(LINES))	
	(cd Compiler/C; cat *.sml >> $(LINES))	
	(cd Compiler/Backend; cat *.sml >> $(LINES))	
	(cd Compiler/Backend/HpPaRisc; cat *.sml >> $(LINES))	
	(cd Manager; cat *.sml >> $(LINES))
	wc -l $(LINES)
	rm -f $(LINES)

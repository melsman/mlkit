#
# Build the ML Kit including tools, runtime systems, etc.
#

BINDIR=../bin
RUNDIR=RuntimeWithGC
INSTDIR=/usr/local/mlkit
ARCH-OS=x86-linux
HEAP2EXEC=heap2exec/heap2exec heap2exec/run.$(ARCH-OS)

GENOPCODES_TARGETS=RuntimeWithGC/Prims.c RuntimeWithGC/PrimsNsSml.c \
 RuntimeWithGC/KamInsts.h Compiler/Backend/KAM/BuiltInCFunctionsKAM.sml \
 Compiler/Backend/KAM/OPCODES_KAM.sml Compiler/Backend/KAM/OpcodesKAM.sml

GENOPCODES_SOURCES=Compiler/Backend/KAM/BuiltInCFunctions.spec \
 Compiler/Backend/KAM/BuiltInCFunctionsNsSml.spec

mlkit: basics
	mkdir -p $(BINDIR)
	cd $(RUNDIR); $(MAKE) runtimeSystem.o
	cd $(RUNDIR); $(MAKE) runtimeSystemGC.o
#	cd $(RUNDIR); $(MAKE) runtimeSystemProf.o
	echo 'use "build.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/mlkit.$(ARCH-OS)
	rm -f $(BINDIR)/mlkit
	cd ..; echo '#!/bin/sh' > bin/mlkit
	cd ..; echo `pwd`/bin/mlkit.$(ARCH-OS) `pwd` \$$* >> bin/mlkit
	chmod a+x $(BINDIR)/mlkit

mlkit_kam: basics
	cd $(RUNDIR); $(MAKE) kam
	echo 'use "build_kam.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/mlkit_kam.$(ARCH-OS)
	rm -f $(BINDIR)/mlkit_kam
	cd ..; echo '#!/bin/sh' > bin/mlkit_kam
	cd ..; echo `pwd`/bin/mlkit_kam.$(ARCH-OS) `pwd` \$$* >> bin/mlkit_kam
	chmod a+x $(BINDIR)/mlkit_kam

mlkit_web: basics smlserver
	cd $(RUNDIR); $(MAKE) runtimeSystemKamNsSml.o
	echo 'use "build_web.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/mlkit_web.$(ARCH-OS)
	rm -f $(BINDIR)/mlkit_web
	cd ..; echo '#!/bin/sh' > bin/mlkit_web
	cd ..; echo `pwd`/bin/mlkit_web.$(ARCH-OS) `pwd` \$$* >> bin/mlkit_web
	chmod a+x $(BINDIR)/mlkit_web

basics: genopcodes rp2ps tester heap2exec

genopcodes: $(GENOPCODES_TARGETS)

$(GENOPCODES_TARGETS): $(GENOPCODES_SOURCES)
	cd Tools/GenOpcodes; $(MAKE)
	../bin/kitgen_opcodes `pwd`

smlserver:
	cd SMLserver; $(MAKE)

rp2ps:
	cd Tools/Rp2ps; $(MAKE)

tester:
	cd Tools/Tester; $(MAKE)

heap2exec: heap2exec/heap2exec
	cd heap2exec; $(MAKE)

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
	mkdir $(INSTDIR)/basislib/PM/GC

	(cd RuntimeWithGC; cp -p runtimeSystem.o runtimeSystemGC.o $(INSTDIR)/runtime)
	(cd ..; cp -p copyright readme.x86-linux $(INSTDIR))
	(cd ../kitdemo; cp -p $(KITDEMO_FILES) $(INSTDIR)/kitdemo) 
	(cd ../kitdemo/utils; cp -p ListUtils.sml utils.pm $(INSTDIR)/kitdemo/utils) 
	(cd ../ml-yacc-lib; cp -p $(MLYACCLIB_FILES) $(INSTDIR)/ml-yacc-lib)        
	(cd ../bin; cp -p mlkit.heap2exec $(INSTDIR)/bin)
	(cd ../basislib; cp -p $(BASISLIB_FILES) $(INSTDIR)/basislib)
	(cd ../basislib/PM/NoProf; cp -p $(NOPROF_OFILES) $(INSTDIR)/basislib/PM/NoProf)
	cp -p ../doc/manual/manual.ps $(INSTDIR)/doc

# the following is done in the %post section in the rpm file! 
# But why not here? Because the installdir is not known yet!
#
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

# -----------------------------------------------------------
# Build the ML Kit including tools, runtime systems, etc.
# -----------------------------------------------------------

SHELL=/bin/sh

# Some commands
MKDIR=mkdir -p
INSTALL=cp -p

BINDIR=../bin

ARCH-OS=x86-linux
HEAP2EXEC=heap2exec/heap2exec heap2exec/run.$(ARCH-OS)

GENOPCODES_TARGETS=RuntimeWithGC/Prims.c RuntimeWithGC/PrimsNsSml.c \
 RuntimeWithGC/KamInsts.h Compiler/Backend/KAM/BuiltInCFunctionsKAM.sml \
 Compiler/Backend/KAM/OPCODES_KAM.sml Compiler/Backend/KAM/OpcodesKAM.sml

GENOPCODES_SOURCES=Compiler/Backend/KAM/BuiltInCFunctions.spec \
 Compiler/Backend/KAM/BuiltInCFunctionsNsSml.spec

mlkit: basics
	cd RuntimeWithGC; $(MAKE) runtimeSystem.o
	cd RuntimeWithGC; $(MAKE) runtimeSystemGC.o
	cd RuntimeWithGC; $(MAKE) runtimeSystemProf.o
	echo 'use "build.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/mlkit.$(ARCH-OS)
	rm -f $(BINDIR)/mlkit
	cd ..; echo '#!/bin/sh' > bin/mlkit
	cd ..; echo `pwd`/bin/mlkit.$(ARCH-OS) `pwd` \$$* >> bin/mlkit
	chmod a+x $(BINDIR)/mlkit

mlkit_kam: basics
	cd RuntimeWithGC; $(MAKE) kam
	echo 'use "build_kam.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/mlkit_kam.$(ARCH-OS)
	rm -f $(BINDIR)/mlkit_kam
	cd ..; echo '#!/bin/sh' > bin/mlkit_kam
	cd ..; echo `pwd`/bin/mlkit_kam.$(ARCH-OS) `pwd` \$$* >> bin/mlkit_kam
	chmod a+x $(BINDIR)/mlkit_kam

smlserver: basics
	cd SMLserver; $(MAKE)
	cd RuntimeWithGC; $(MAKE) runtimeSystemKamNsSml.o
	echo 'use "build_web.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/mlkit_web.$(ARCH-OS)
	rm -f $(BINDIR)/mlkit_web
	cd ..; echo '#!/bin/sh' > bin/mlkit_web
	cd ..; echo `pwd`/bin/mlkit_web.$(ARCH-OS) `pwd` \$$* >> bin/mlkit_web
	chmod a+x $(BINDIR)/mlkit_web

basics: genopcodes rp2ps tester heap2exec

genopcodes: $(GENOPCODES_TARGETS)

$(GENOPCODES_TARGETS): $(GENOPCODES_SOURCES)
	mkdir -p $(BINDIR)
	cd Tools/GenOpcodes; $(MAKE)
	$(BINDIR)/kitgen_opcodes `pwd`

rp2ps:
	cd Tools/Rp2ps; $(MAKE)

tester:
	cd Tools/Tester; $(MAKE)

heap2exec: heap2exec/heap2exec
	cd heap2exec; $(MAKE)

clean:
	rm -rf run PM CM *~
	cd Common; rm -rf PM CM *~
	cd Common/EfficientElab; rm -rf PM CM *~
	cd Compiler; rm -rf PM CM *~
	cd Parsing; rm -rf PM CM *~
	cd Manager; rm -rf PM CM *~
	cd Edlib; rm -rf PM CM *~
	cd heap2exec; $(MAKE) clean
	cd Runtime; $(MAKE) clean
	cd RuntimeWithGC; $(MAKE) clean
	cd SMLserver; $(MAKE) clean
	cd Tools/Tester; $(MAKE) clean
	cd Tools/Rp2ps; $(MAKE) clean
	cd Tools/GenOpcodes; $(MAKE) clean
	cd Compiler; rm -rf PM CM *~
	cd Compiler/Backend; rm -rf PM CM *~
	cd Compiler/Backend/Dummy; rm -rf PM CM *~
	cd Compiler/Backend/HpPaRisc; rm -rf PM CM *~
	cd Compiler/Backend/X86; rm -rf PM CM *~
	cd Compiler/Regions; rm -rf PM CM *~
	cd Compiler/C; rm -rf PM CM *~
	cd Compiler/Hppa; rm -rf PM CM *~
	cd Compiler/Kam; rm -rf PM CM *~
	cd Compiler/Cfg; rm -rf PM CM *~
	cd Compiler/Lambda; rm -rf PM CM *~


# ----------------------------------------------------------
# Support for measuring the code blowup resulting from
# compiling functors in the ML Kit; not used by install!
# ----------------------------------------------------------

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

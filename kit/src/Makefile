# -----------------------------------------------------------
# Build the ML Kit including tools, runtime systems, etc.
# -----------------------------------------------------------

SHELL=/bin/sh

# Some commands
MKDIR=mkdir -p
INSTALL=cp -p
CLEAN=rm -rf MLB PM CM *~ .\#* a.out
CLEAN_MLB=rm -rf MLB

BINDIR=../bin

ARCH-OS=x86-linux
#ARCH-OS=x86-bsd
HEAP2EXEC=heap2exec/heap2exec heap2exec/run.$(ARCH-OS)

GENOPCODES_TARGETS=Runtime/Prims.c Runtime/PrimsNsSml.c \
 Runtime/KamInsts.h Runtime/KamInsts.c Runtime/jumptbl.h \
 Compiler/Backend/KAM/BuiltInCFunctionsKAM.sml \
 Compiler/Backend/KAM/OPCODES_KAM.sml Compiler/Backend/KAM/OpcodesKAM.sml

GENOPCODES_SOURCES=Compiler/Backend/KAM/BuiltInCFunctions.spec \
 Compiler/Backend/KAM/BuiltInCFunctionsNsSml.spec \
 Compiler/Backend/KAM/KamInsts.spec

# Whether request profiling is enabled 
#REQUEST_PROFILING=
REQUEST_PROFILING=true

mlkit: basics rp2ps tester mlbmake kitbench
	cd Runtime; $(MAKE) runtimeSystem.a
	cd Runtime; $(MAKE) runtimeSystemProf.a
	cd Runtime; $(MAKE) runtimeSystemGC.a
	cd Runtime; $(MAKE) runtimeSystemGCProf.a
	cd Runtime; $(MAKE) runtimeSystemGenGC.a
	cd Runtime; $(MAKE) runtimeSystemGenGCProf.a
	cd Runtime; $(MAKE) runtimeSystemGCTP.a
	cd Runtime; $(MAKE) runtimeSystemGCTPProf.a
	cd Runtime; $(MAKE) runtimeSystemTag.a
	echo 'use "build.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/mlkit.$(ARCH-OS)
	rm -f $(BINDIR)/mlkit
	cd ..; echo '#!/bin/sh' > bin/mlkit
	cd ..; echo `pwd`/bin/mlkit.$(ARCH-OS) `pwd` \$$* >> bin/mlkit
	chmod a+x $(BINDIR)/mlkit

mlkit_kam: basics tester kitbench
	cd Runtime; $(MAKE) kam
	echo 'use "build_kam.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/mlkit_kam.$(ARCH-OS)
	rm -f $(BINDIR)/mlkit_kam
	cd ..; echo '#!/bin/sh' > bin/mlkit_kam
	cd ..; echo `pwd`/bin/mlkit_kam.$(ARCH-OS) `pwd` \$$* >> bin/mlkit_kam
	chmod a+x $(BINDIR)/mlkit_kam

smlserver: basics
	cd Runtime; $(MAKE) runtimeSystemKamNsSml.o
	cd Runtime; $(MAKE) runtimeSystemKamApSml.o
	cd SMLserver; $(MAKE) REQUEST_PROFILING=$(REQUEST_PROFILING)
	echo 'use "build_web.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/smlserverc.$(ARCH-OS)
	rm -f $(BINDIR)/smlserverc
	cd ..; echo '#!/bin/sh' > bin/smlserverc
	cd ..; echo `pwd`/bin/smlserverc.$(ARCH-OS) `pwd` \$$* >> bin/smlserverc
	chmod a+x $(BINDIR)/smlserverc

barry: basics
	echo 'use "build_barry.sml";' | sml
	$(HEAP2EXEC) $(BINDIR)/kit.$(ARCH-OS) $(BINDIR)/barry.$(ARCH-OS)
	rm -f $(BINDIR)/barry
	cd ..; echo '#!/bin/sh' > bin/barry
	cd ..; echo `pwd`/bin/barry.$(ARCH-OS) `pwd` \$$* >> bin/barry
	chmod a+x $(BINDIR)/barry

basics: genopcodes heap2exec

genopcodes: $(GENOPCODES_TARGETS)

$(GENOPCODES_TARGETS): $(GENOPCODES_SOURCES)
	mkdir -p $(BINDIR)
	cd Tools/GenOpcodes; $(MAKE)
# kitgen_opcodes assumes it's run from the src-directory
	$(BINDIR)/kitgen_opcodes

rp2ps:
	cd Tools/Rp2ps; $(MAKE)

tester:
	cd Tools/Tester; $(MAKE)

mlbmake:
	cd Tools/MlbMake; $(MAKE)

kitbench:
	cd Tools/Benchmark; $(MAKE)

heap2exec: heap2exec/heap2exec
	cd heap2exec; $(MAKE)

clean:
	$(CLEAN) run $(GENOPCODES_TARGETS)
	cd Pickle; $(CLEAN)
	cd Common; $(CLEAN)
	cd Common/EfficientElab; $(CLEAN)
	cd Compiler; $(CLEAN)
	cd Parsing; $(CLEAN)
	cd Manager; $(CLEAN)
	cd Edlib; $(CLEAN)
	cd heap2exec; $(MAKE) clean
	cd Runtime; $(MAKE) clean
	cd Tools/Tester; $(MAKE) clean
	cd Tools/Rp2ps; $(MAKE) clean
	cd Tools/MlbMake; $(MAKE) clean
	cd Tools/GenOpcodes; $(MAKE) clean
	cd Tools/Benchmark; $(MAKE) clean
	cd Compiler; $(CLEAN)
	cd Compiler/Backend; $(CLEAN)
	cd Compiler/Backend/Dummy; $(CLEAN)
	cd Compiler/Backend/HpPaRisc; $(CLEAN)
	cd Compiler/Backend/X86; $(CLEAN)
	cd Compiler/Backend/KAM; $(CLEAN)
	cd Compiler/Backend/Barry; $(CLEAN)
	cd Compiler/Regions; $(CLEAN)
	cd Compiler/C; $(CLEAN)
	cd Compiler/Hppa; $(CLEAN)
	cd Compiler/Kam; $(CLEAN)
	cd Compiler/Cfg; $(CLEAN)
	cd Compiler/Lambda; $(CLEAN)
	cd SMLserver; $(CLEAN) nssml.so *.o

clean_mlb:
	cd ../basis; $(CLEAN_MLB)
	$(CLEAN_MLB)
	cd Pickle; $(CLEAN_MLB)
	cd Common; $(CLEAN_MLB)
	cd Common/EfficientElab; $(CLEAN_MLB)
	cd Compiler; $(CLEAN_MLB)
	cd Compiler/Lambda; $(CLEAN_MLB)
	cd Compiler/Regions; $(CLEAN_MLB)
	cd Compiler/Backend; $(CLEAN_MLB)
	cd Compiler/Backend/Dummy; $(CLEAN_MLB)
	cd Compiler/Backend/X86; $(CLEAN_MLB)
	cd Compiler/Backend/KAM; $(CLEAN_MLB)
	cd Compiler/Backend/Barry; $(CLEAN_MLB)
	cd Parsing; $(CLEAN_MLB)
	cd Manager; $(CLEAN_MLB)
	cd Edlib; $(CLEAN_MLB)
	cd Tools/MlbMake; $(CLEAN_MLB)

grm:
	cd Parsing; ml-lex Topdec.lex; ml-yacc Topdec.grm

# ----------------------------------------------------------
# Support for measuring the code blowup resulting from
# compiling functors in the ML Kit; not used by install!
# ----------------------------------------------------------

FILE = /home/mael/kit/src/bdys.txt
LINES = /home/mael/kit/src/lines.mael.sml


bdys:
	(cd Common/PM/RI; wc -l *.bdy > $(FILE))
	(cd Common/EfficientElab/PM/RI; wc -l *.bdy >> $(FILE)) 
	(cd Parsing/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Compiler/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Lambda/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Regions/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Kam/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Cfg/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Compiler/C/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Backend/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Compiler/Backend/HpPaRisc/PM/RI; wc -l *.bdy >> $(FILE))	
	(cd Manager/PM/RI; wc -l *.bdy >> $(FILE))	

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

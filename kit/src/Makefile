#
# Build the ML Kit for the x86 
#



all:
	(cd RuntimeWithGC; $(MAKE))
	(cd heap2exec; $(MAKE))
	(echo 'use "x86-linux.sml";' | sml)
	heap2exec/heap2exec heap2exec/run.x86-linux ../bin/kit.x86-linux ../bin/mlkit.heap2exec

INSTDIR = /usr/local/mlkit

KITDEMO_FILES = SET.sml Set.pm Set.sml SetApp.sml apply.sml ccalls.sml	\
ccalls.sml.mads compose.pm compose.sml elimpair.sml escape.sml		\
escape1.sml escape2.sml ex.script exceptions.sml fold1.sml fold2.sml	\
fold3.sml fromto.sml helloworld.sml kit.script lambda.sml lambda1.sml	\
lambda2.sml lambda3.sml lambda4.sml length.sml lib.sml life.sml		\
map.sml minilist.pm minilist.sml msort.pm msort.sml msortreset1.pm	\
msortreset1.sml msortreset2.pm msortreset2.sml msortrun.sml my_lib.c	\
my_lib.pm my_lib.sml onetwothree.sml projection.sml refs1.sml		\
refs2.sml refs3.sml runmsort.sml scan.pm scan.sml scan_rev1.pm		\
scan_rev1.sml scan_rev2.pm scan_rev2.sml scanfiles select_second.c	\
select_second.pm select_second.sml set.pm sma1.sml tail.sml		\
takeanddrop.sml test_my_lib.sml testdyn1.sml testdyn2.sml trees.pm	\
trees.sml tststrcmp.sml upto.sml vpprob.pm vpprob.sml

MLYACCLIB_FILES = join.sml ml-yacc-lib.pm parser2.sml base.sig          \
lrtable.sml parser1.sml stream.sml

BASISLIB_FILES = ARRAY.sml Array.sml BIN_IO.sml BOOL.sml BYTE.sml	\
BinIO.sml Bool.sml Byte.sml ByteArray.sml ByteVector.sml CHAR.sml	\
COMMAND_LINE.sml Char.sml CommandLine.sml DATE.sml Date.sml		\
FileSys.sml GENERAL.sml General.sml INTEGER.sml IO.sml Initial.sml	\
Int.sml LIST.sml LIST_PAIR.sml LIST_SORT.sml List.sml ListPair.sml	\
ListSort.sml MATH.sml MONO_ARRAY.sml MONO_VECTOR.sml Math.sml		\
OPTION.sml OS.sml OS_FILE_SYS.sml OS_PATH.sml OS_PROCESS.sml		\
Option.sml Path.sml Process.sml RANDOM.sml REAL.sml Random.sml		\
Real.sml SML90.sml STRING.sml STRING_CVT.sml STR_BASE.sml		\
SUBSTRING.sml StrBase.sml String.sml StringCvt.sml Substring.sml	\
TEXT_IO.sml TIME.sml TIMER.sml TextIO.sml Time.sml Timer.sml		\
VECTOR.sml Vector.sml WORD.sml Word.sml Word8.sml basislib.pm		\
kit.script wordtables.sml

NOPROF_OFILES = basislib-BinIO.sml.o basislib-Bool.sml.o		\
basislib-Byte.sml.o basislib-Char.sml.o basislib-CommandLine.sml.o	\
basislib-Date.sml.o basislib-FileSys.sml.o basislib-General.sml.o	\
basislib-Initial.sml.o basislib-Int.sml.o basislib-List.sml.o		\
basislib-ListPair.sml.o basislib-ListSort.sml.o basislib-Math.sml.o	\
basislib-Option.sml.o basislib-Path.sml.o basislib-Process.sml.o	\
basislib-Random.sml.o basislib-Real.sml.o basislib-StrBase.sml.o	\
basislib-String.sml.o basislib-StringCvt.sml.o				\
basislib-Substring.sml.o basislib-TextIO.sml.o basislib-Time.sml.o	\
basislib-Timer.sml.o basislib-Vector.sml.o basislib-Word.sml.o		\
basislib-Word8.sml.o basislib-code1.o basislib-code10.o			\
basislib-code13.o basislib-code16.o basislib-code19.o			\
basislib-code24.o basislib-code25.o basislib-code26.o basislib-code4.o	\
basislib-code7.o basislib.ByteArray.bdy basislib.ByteVector.bdy		\
basislib.table.bdy basislib.pm.date

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

# the following is done in the %post section in the rpm file!
#	echo '#!/bin/sh' > /usr/local/mlkit/bin/mlkit
#	echo -e '/usr/local/mlkit/bin/mlkit.heap2exec /usr/local/mlkit \44*' >> /usr/local/mlkit/bin/mlkit
#	chmod a+x /usr/local/mlkit/bin/mlkit

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

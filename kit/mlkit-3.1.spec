# ---------------------------------------------------------------------
# RPM Spec File for the ML Kit Version 3.1
# ---------------------------------------------------------------------
#
# Here is a short description of how to build an RPM File for the ML
# Kit. First, check that you can build the Kit with the shell command
#
#   %> make
#
# from within the ML Kit root directory. You must also be able to
# install the Kit in /usr/local/mlkit with the command (you need to be
# root to do this)
# 
#   %> make install
#
# The install section in the Makefile copies all files that the end
# user needs to run the kit into the /usr/local/mlkit directory. Here
# you need to be careful that the files that are copied are also
# mentioned in the %files section of this spec file.
#
# Before you can actually build the rpm file, you need to construct a
# gzipped tar file containing the ML Kit sources. Execute the command
#
#   %> make tgz
#
# from within the ML Kit root directory. As root, copy the 
# specification file mlkit-3.1.spec to /usr/src/redhat/SPECS/ and 
# copy the file mlkit-3.1.tgz to /usr/src/redhat/SOURCES/. You are 
# now ready for the build:
#
#   $> cd /usr/src/redhat/SPECS
#   $> rpm -ba mlkit-3.1.spec
# 
# ---------------------------------------------------------------------

Summary: A Standard ML compiler
Name: mlkit
Version: 3.1
Release: 1
Copyright: GPL
Group: Development/Languages
Source: http://www.diku.dk/research-groups/topps/activities/kit3/mlkit-3.1.tgz
URL: http://www.diku.dk/research-groups/topps/activities/mlkit.html
Vendor: University of Copenhagen
Packager: Martin Elsman (mael@cs.berkeley.edu)
Prefix: /usr/local/mlkit

%description 
The ML Kit with Regions (henceforth refered to as the Kit) is a
compiler for the programming language Standard ML. The Kit covers all
of Standard ML, as defined in the 1997 edition of the Definition of
Standard ML and supports most of the Standard ML Basis Library. The
compiler generates efficient native code for the HP PA-RISC and X86
architectures. The largest program compiled with the Kit is the Kit
itself (around 80.000 lines of SML, plus the Basis Library).

%prep
%setup

%build
make

%install
make install

%files

#
# Documentation, Etc.
#
%doc /usr/local/mlkit/doc/manual.ps
%doc /usr/local/mlkit/readme.x86-linux
/usr/local/mlkit/copyright

#
# ML Kit Executable
#
/usr/local/mlkit/bin/mlkit.heap2exec

#
# Runtime Systems
#
/usr/local/mlkit/runtime/runtimeSystem.o 
/usr/local/mlkit/runtime/runtimeSystemGC.o

#
# The Demonstration Directory
#
/usr/local/mlkit/kitdemo/SET.sml 
/usr/local/mlkit/kitdemo/Set.pm 
/usr/local/mlkit/kitdemo/Set.sml 
/usr/local/mlkit/kitdemo/SetApp.sml 
/usr/local/mlkit/kitdemo/apply.sml 
/usr/local/mlkit/kitdemo/ccalls.sml
/usr/local/mlkit/kitdemo/compose.pm 
/usr/local/mlkit/kitdemo/compose.sml 
/usr/local/mlkit/kitdemo/elimpair.sml 
/usr/local/mlkit/kitdemo/escape.sml
/usr/local/mlkit/kitdemo/escape1.sml 
/usr/local/mlkit/kitdemo/escape2.sml 
/usr/local/mlkit/kitdemo/ex.script 
/usr/local/mlkit/kitdemo/exceptions.sml 
/usr/local/mlkit/kitdemo/fold1.sml 
/usr/local/mlkit/kitdemo/fold2.sml
/usr/local/mlkit/kitdemo/fold3.sml 
/usr/local/mlkit/kitdemo/fromto.sml 
/usr/local/mlkit/kitdemo/helloworld.sml 
/usr/local/mlkit/kitdemo/kit.script 
/usr/local/mlkit/kitdemo/lambda.sml 
/usr/local/mlkit/kitdemo/lambda1.sml
/usr/local/mlkit/kitdemo/lambda2.sml 
/usr/local/mlkit/kitdemo/lambda3.sml 
/usr/local/mlkit/kitdemo/lambda4.sml 
/usr/local/mlkit/kitdemo/length.sml 
/usr/local/mlkit/kitdemo/lib.sml 
/usr/local/mlkit/kitdemo/life.sml
/usr/local/mlkit/kitdemo/map.sml 
/usr/local/mlkit/kitdemo/minilist.pm 
/usr/local/mlkit/kitdemo/minilist.sml 
/usr/local/mlkit/kitdemo/msort.pm 
/usr/local/mlkit/kitdemo/msort.sml 
/usr/local/mlkit/kitdemo/msortreset1.pm
/usr/local/mlkit/kitdemo/msortreset1.sml 
/usr/local/mlkit/kitdemo/msortreset2.pm 
/usr/local/mlkit/kitdemo/msortreset2.sml 
/usr/local/mlkit/kitdemo/msortrun.sml 
/usr/local/mlkit/kitdemo/my_lib.c
/usr/local/mlkit/kitdemo/my_lib.pm 
/usr/local/mlkit/kitdemo/my_lib.sml 
/usr/local/mlkit/kitdemo/onetwothree.sml 
/usr/local/mlkit/kitdemo/projection.sml 
/usr/local/mlkit/kitdemo/refs1.sml
/usr/local/mlkit/kitdemo/refs2.sml 
/usr/local/mlkit/kitdemo/refs3.sml 
/usr/local/mlkit/kitdemo/runmsort.sml 
/usr/local/mlkit/kitdemo/scan.pm 
/usr/local/mlkit/kitdemo/scan.sml 
/usr/local/mlkit/kitdemo/scan_rev1.pm
/usr/local/mlkit/kitdemo/scan_rev1.sml 
/usr/local/mlkit/kitdemo/scan_rev2.pm 
/usr/local/mlkit/kitdemo/scan_rev2.sml 
/usr/local/mlkit/kitdemo/scanfiles 
/usr/local/mlkit/kitdemo/select_second.c
/usr/local/mlkit/kitdemo/select_second.pm 
/usr/local/mlkit/kitdemo/select_second.sml 
/usr/local/mlkit/kitdemo/set.pm 
/usr/local/mlkit/kitdemo/sma1.sml 
/usr/local/mlkit/kitdemo/tail.sml
/usr/local/mlkit/kitdemo/takeanddrop.sml 
/usr/local/mlkit/kitdemo/test_my_lib.sml 
/usr/local/mlkit/kitdemo/testdyn1.sml 
/usr/local/mlkit/kitdemo/testdyn2.sml 
/usr/local/mlkit/kitdemo/trees.pm
/usr/local/mlkit/kitdemo/trees.sml 
/usr/local/mlkit/kitdemo/tststrcmp.sml 
/usr/local/mlkit/kitdemo/upto.sml 
/usr/local/mlkit/kitdemo/vpprob.pm 
/usr/local/mlkit/kitdemo/vpprob.sml
/usr/local/mlkit/kitdemo/utils/ListUtils.sml 
/usr/local/mlkit/kitdemo/utils/utils.pm

/usr/local/mlkit/ml-yacc-lib/join.sml 
/usr/local/mlkit/ml-yacc-lib/ml-yacc-lib.pm 
/usr/local/mlkit/ml-yacc-lib/parser2.sml 
/usr/local/mlkit/ml-yacc-lib/base.sig
/usr/local/mlkit/ml-yacc-lib/lrtable.sml 
/usr/local/mlkit/ml-yacc-lib/parser1.sml 
/usr/local/mlkit/ml-yacc-lib/stream.sml

#
# Basis Library Source Files; the user might want to look at them
#
/usr/local/mlkit/basislib/ARRAY.sml 
/usr/local/mlkit/basislib/Array.sml 
/usr/local/mlkit/basislib/BIN_IO.sml 
/usr/local/mlkit/basislib/BOOL.sml 
/usr/local/mlkit/basislib/BYTE.sml
/usr/local/mlkit/basislib/BinIO.sml 
/usr/local/mlkit/basislib/Bool.sml 
/usr/local/mlkit/basislib/Byte.sml 
/usr/local/mlkit/basislib/ByteArray.sml 
/usr/local/mlkit/basislib/ByteVector.sml 
/usr/local/mlkit/basislib/CHAR.sml
/usr/local/mlkit/basislib/COMMAND_LINE.sml 
/usr/local/mlkit/basislib/Char.sml 
/usr/local/mlkit/basislib/CommandLine.sml 
/usr/local/mlkit/basislib/DATE.sml 
/usr/local/mlkit/basislib/Date.sml
/usr/local/mlkit/basislib/FileSys.sml 
/usr/local/mlkit/basislib/GENERAL.sml 
/usr/local/mlkit/basislib/General.sml 
/usr/local/mlkit/basislib/INTEGER.sml 
/usr/local/mlkit/basislib/IO.sml 
/usr/local/mlkit/basislib/Initial.sml
/usr/local/mlkit/basislib/Int.sml 
/usr/local/mlkit/basislib/LIST.sml 
/usr/local/mlkit/basislib/LIST_PAIR.sml 
/usr/local/mlkit/basislib/LIST_SORT.sml 
/usr/local/mlkit/basislib/List.sml 
/usr/local/mlkit/basislib/ListPair.sml
/usr/local/mlkit/basislib/ListSort.sml 
/usr/local/mlkit/basislib/MATH.sml 
/usr/local/mlkit/basislib/MONO_ARRAY.sml 
/usr/local/mlkit/basislib/MONO_VECTOR.sml 
/usr/local/mlkit/basislib/Math.sml
/usr/local/mlkit/basislib/OPTION.sml 
/usr/local/mlkit/basislib/OS.sml 
/usr/local/mlkit/basislib/OS_FILE_SYS.sml 
/usr/local/mlkit/basislib/OS_PATH.sml 
/usr/local/mlkit/basislib/OS_PROCESS.sml
/usr/local/mlkit/basislib/Option.sml 
/usr/local/mlkit/basislib/Path.sml 
/usr/local/mlkit/basislib/Process.sml 
/usr/local/mlkit/basislib/RANDOM.sml 
/usr/local/mlkit/basislib/REAL.sml 
/usr/local/mlkit/basislib/Random.sml
/usr/local/mlkit/basislib/Real.sml 
/usr/local/mlkit/basislib/SML90.sml 
/usr/local/mlkit/basislib/STRING.sml 
/usr/local/mlkit/basislib/STRING_CVT.sml 
/usr/local/mlkit/basislib/STR_BASE.sml
/usr/local/mlkit/basislib/SUBSTRING.sml 
/usr/local/mlkit/basislib/StrBase.sml 
/usr/local/mlkit/basislib/String.sml 
/usr/local/mlkit/basislib/StringCvt.sml 
/usr/local/mlkit/basislib/Substring.sml
/usr/local/mlkit/basislib/TEXT_IO.sml 
/usr/local/mlkit/basislib/TIME.sml 
/usr/local/mlkit/basislib/TIMER.sml 
/usr/local/mlkit/basislib/TextIO.sml 
/usr/local/mlkit/basislib/Time.sml 
/usr/local/mlkit/basislib/Timer.sml
/usr/local/mlkit/basislib/VECTOR.sml 
/usr/local/mlkit/basislib/Vector.sml 
/usr/local/mlkit/basislib/WORD.sml 
/usr/local/mlkit/basislib/Word.sml 
/usr/local/mlkit/basislib/Word8.sml 
/usr/local/mlkit/basislib/basislib.pm
/usr/local/mlkit/basislib/kit.script 
/usr/local/mlkit/basislib/wordtables.sml

#
# Basis Library Object Files
#
/usr/local/mlkit/basislib/PM/NoProf/basislib-BinIO.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Bool.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-Byte.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Char.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-CommandLine.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-Date.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-FileSys.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-General.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-Initial.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Int.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-List.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-ListPair.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-ListSort.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Math.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-Option.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Path.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Process.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-Random.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Real.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-StrBase.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-String.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-StringCvt.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-Substring.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-TextIO.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Time.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-Timer.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Vector.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-Word.sml.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-Word8.sml.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-code1.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-code10.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-code13.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-code16.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-code19.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-code24.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-code25.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-code26.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib-code4.o
/usr/local/mlkit/basislib/PM/NoProf/basislib-code7.o 
/usr/local/mlkit/basislib/PM/NoProf/basislib.ByteArray.bdy 
/usr/local/mlkit/basislib/PM/NoProf/basislib.ByteVector.bdy
/usr/local/mlkit/basislib/PM/NoProf/basislib.table.bdy
/usr/local/mlkit/basislib/PM/NoProf/basislib.pm.date

%post
echo '#!/bin/sh' > ${RPM_INSTALL_PREFIX}/bin/mlkit
echo -e ''${RPM_INSTALL_PREFIX}'/bin/mlkit.heap2exec '${RPM_INSTALL_PREFIX}' \44*' >> ${RPM_INSTALL_PREFIX}/bin/mlkit
chmod a+x ${RPM_INSTALL_PREFIX}/bin/mlkit

# ---------------------------------------------------------------------
# RPM Spec File for the ML Kit Version 3.4.0
# ---------------------------------------------------------------------
#
# Here is a short description of how to build an RPM File for the ML
# Kit. First, check that you can build the Kit with the shell command
#
#   % make
#
# from within the ML Kit root directory. You must also be able to
# install the Kit in /usr/local/mlkit with the command (you need to be
# root to do this)
# 
#   # make install
#
# The install section in the Makefile copies all files that the end
# user needs to run the kit into the /usr/local/mlkit directory. Here
# you need to be careful that the files that are copied are also
# mentioned in the %files section of this spec file.
#
# Before you can actually build the rpm file, you need to construct a
# gzipped tar file containing the ML Kit sources, unless you already
# have such a gzipped tar file. To create the gzipped tar file, execute 
# the command
#
#   % make tgz
#
# from within the ML Kit root directory. As root, copy the 
# specification file mlkit.spec to /usr/src/redhat/SPECS/mlkit-x.y.z.spec 
# and copy the file mlkit-x.y.z.tgz to /usr/src/redhat/SOURCES/. You are 
# now ready for the build:
#
#   # cd /usr/src/redhat/SPECS
#   # rpm -ba mlkit-x.y.z.spec
# 
# ---------------------------------------------------------------------

Summary: A Standard ML compiler
Name: mlkit
Version: 3.4.0
Release: 1
Copyright: GPL
Group: Development/Languages
Source: http://www.it.edu/research/mlkit/download/mlkit-3.4.0.tgz
URL: http://www.it.edu/research/mlkit
Vendor: IT University of Copenhagen
Packager: Martin Elsman (mael@dina.kvl.dk)
Prefix: /usr/local/mlkit
ExclusiveOS: Linux
ExcludeArch: sparc alpha

%description 
The ML Kit (henceforth refered to as the Kit) is a compiler for the
programming language Standard ML. The Kit covers all of Standard ML,
as defined in the 1997 edition of the Definition of Standard ML and
supports most of the Standard ML Basis Library. The Kit uses a region
based memory management scheme in which allocation and de-allocation
primitives are added to the program at compile time. The Kit also
makes it possible to combine the region based memory management scheme
with traditional reference tracing garbage collection. The Kit
generates efficient native code for the x86 architecture; there is
also a version of the Kit that generates portable bytecode, which can
be interpreted by an abstract machine. The largest program compiled
with the Kit is the Kit itself (around 80.000 lines of SML, plus the
Basis Library).

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
%doc /usr/local/mlkit/doc/mlkit.pdf
%doc /usr/local/mlkit/README
/usr/local/mlkit/copyright

# ML Kit Executables
/usr/local/mlkit/bin/mlkit.x86-linux
/usr/local/mlkit/bin/rp2ps

# Runtime Systems
/usr/local/mlkit/bin/runtimeSystem.o 
/usr/local/mlkit/bin/runtimeSystemGC.o

# The Demonstration Directory
/usr/local/mlkit/kitdemo

# Libraries
/usr/local/mlkit/ml-yacc-lib
/usr/local/mlkit/basislib

%post
echo '#!/bin/sh' > ${RPM_INSTALL_PREFIX}/bin/mlkit
echo -e '${RPM_INSTALL_PREFIX}/bin/mlkit.x86-linux ${RPM_INSTALL_PREFIX} $$*' >> ${RPM_INSTALL_PREFIX}/bin/mlkit
chmod a+x ${RPM_INSTALL_PREFIX}/bin/mlkit
ln -sf ${RPM_INSTALL_PREFIX}/bin/mlkit /usr/bin/mlkit
ln -sf ${RPM_INSTALL_PREFIX}/bin/rp2ps /usr/bin/rp2ps
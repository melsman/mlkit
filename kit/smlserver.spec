# ---------------------------------------------------------------------
# RPM Spec File for the ML Kit SMLserver Version 3.9.2
# ---------------------------------------------------------------------
#
# Here is a short description of how to build an RPM File for the ML
# Kit SMLserver. First, check that you can build the Kit with the shell 
# command
#
#   % make smlserver
#
# from within the ML Kit root directory. You must also be able to
# install the Kit in /usr/share/smlserver with the command (you need to 
# be root to do this)
# 
#   # make install_smlserver
#
# The install section in the Makefile copies all files that the end
# user needs to use SMLserver into the /usr/share/smlserver directory. 
# Here you need to be careful that the files that are copied are also
# mentioned in the %files section of this spec file.
#
# Before you can actually build the rpm file, you need to construct a
# gzipped tar file containing the ML Kit sources, unless you already
# have such a gzipped tar file. To create the gzipped tar file, execute 
# the command
#
#   % make tgz_smlserver
#
# from within the ML Kit root directory. As root, copy this 
# specification file smlserver.spec to /usr/src/redhat/SPECS/smlserver-x.y.z.spec 
# and copy the file smlserver-x.y.z.tgz to /usr/src/redhat/SOURCES/. 
# You are now ready for the build:
#
#   # cd /usr/src/redhat/SPECS
#   # rpm -ba smlserver-x.y.z.spec
# 
# ---------------------------------------------------------------------

Summary: SML Support for AOLserver
Name: smlserver
Version: 3.9.2
Release: 1
Copyright: GPL
Group: Development/Languages
Source: http://www.it.edu/research/mlkit/download/smlserver-3.9.2.tgz
URL: http://www.it.edu/research/mlkit
Vendor: IT University of Copenhagen
Packager: Martin Elsman (mael@dina.kvl.dk)
Prefix: /usr/share/smlserver
ExclusiveOS: Linux
ExcludeArch: sparc alpha
Requires: aolserver

%description 
SMLserver is an SML module for AOLserver, an Open Source webserver
provided by America Online. SMLserver supports webserver
interpretation of bytecode files compiled with the ML Kit. Both ML
Server Pages [1] (i.e., msp-files) and plain sml-files, using an SML
interface to the AOLserver API, are supported. SMLserver uses a
load-once-execute-many scheme for executing sml-files and
msp-files. Together with database pooling, the SMLserver
load-once-execute-many scheme makes it possible to create database
backed web sites in SML that are capable of answering a high number of
requests compared to systems using a traditional CGI based
approach. The ML Kit (henceforth refered to as the Kit) is a compiler
for the programming language Standard ML. The Kit covers all of
Standard ML, as defined in the 1997 edition of the Definition of
Standard ML and supports most of the Standard ML Basis Library.

[1] A system for running ML Server Pages using the Apache Web-server
CGI support was first implemented by Peter Sestoft. For information
about this work, consult http://www.dina.kvl.dk/~sestoft/msp

%prep
%setup

%build
make smlserver

%install
rm -rf $RPM_BUILD_ROOT
make install_smlserver

%clean
rm -rf $RPM_BUILD_ROOT

%files

# Documentation, Etc.
%doc /usr/share/smlserver/doc/mlkit.pdf
%doc /usr/share/smlserver/README
%doc /usr/share/smlserver/README_SMLSERVER
%doc /usr/share/smlserver/NEWS_SMLSERVER
/usr/share/smlserver/copyright

# ML Kit Executables
/usr/share/smlserver/bin/smlserverc.x86-linux
/usr/share/smlserver/bin/smlserverc
/usr/bin/smlserverc

# Module for AOLserver (includes runtime system)
/usr/share/smlserver/bin/nssml.so

# The Demonstration Directory
/usr/share/smlserver/smlserver_demo

# Libraries
/usr/share/smlserver/basislib

%post
echo '#!/bin/sh' > ${RPM_INSTALL_PREFIX}/bin/smlserverc
echo ''${RPM_INSTALL_PREFIX}/bin/smlserverc.x86-linux ${RPM_INSTALL_PREFIX}' $*' >> ${RPM_INSTALL_PREFIX}/bin/smlserverc
chmod a+x ${RPM_INSTALL_PREFIX}/bin/smlserverc
ln -sf ${RPM_INSTALL_PREFIX}/bin/smlserverc /usr/bin/smlserverc

%preun
rm -f ${RPM_INSTALL_PREFIX}/bin/smlserverc


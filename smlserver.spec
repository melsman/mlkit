# ---------------------------------------------------------------------
# RPM Spec File for the ML Kit SMLserver Version 4.1.1
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

Summary: Standard ML Support for AOLserver
Name: smlserver
Version: 4.1.1
Release: 1
Copyright: GPL
Group: Development/Languages
Source: http://www.smlserver.org/dist/smlserver-4.1.1.tgz
URL: http://www.smlserver.org
Vendor: IT University of Copenhagen
Packager: Martin Elsman (mael@dina.kvl.dk)
Prefix: /usr/share/smlserver
ExclusiveOS: Linux
ExcludeArch: sparc alpha
Requires: aolserver

%description 
SMLserver is a webserver plugin for AOLserver, an Open Source
multi-threaded webserver provided by America Online and used for large
scale, dynamic web sites. SMLserver allows efficient threaded
execution of Standard ML programs and inherits many of the great
features of AOLserver, including the possibility of accessing a
variety of different relational database management systems (RDBMSs),
such as Oracle and Postgresql. The main features of SMLserver are:

Standard ML '97: With SMLserver, you can write dynamic
  web-applications using your favorite programming language, without
  sacrificing efficiency and scalability of your
  web-service. SMLserver is based on the ML Kit, which covers all of
  Standard ML '97, as defined in the Definition of Standard ML. In
  particular, SMLserver supports the use of Standard ML Modules and
  most of the Standard ML Basis Library.

Web-Server API: SMLserver provides an SML interface to the AOLserver
  API, thereby giving the SML programmer access to the following
  AOLserver features:

    * Easy access to form data and request header information,
      including cookies

    * Access to Relational Database Management Systems (RDBMSs)
      through an efficient generic interface that supports database
      pooling (i.e, reuse of database connections). Supported RDBMSs
      include Oracle, Postgresql, and MySQL

    * Support for sending email 

    * Extracting data from foreign web-sites 

    * Caching possibilities for improved efficiency

Efficient Multi-threaded Execution: SMLserver compiles Standard ML
  programs into bytecode files, which are loaded only once but may be
  executed many times upon client requests. A multi-threaded execution
  model makes it possible for SMLserver to serve multiple requests
  simultaneously.  

Documentation: A tutorial on writing dynamic web-applications using
  SMLserver is available from the SMLserver home page

                    http://www.smlserver.org
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
%doc /usr/share/smlserver/doc/smlserver.pdf
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


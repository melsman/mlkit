#
# $Header$
#
# nsopenssl --
#
#      SSLv2, SSLv3, TLSv1 module using OpenSSL.
#

ifdef INST
NSHOME ?= $(INST)
else
NSHOME ?= ../aolserver
endif

MOD      =  nsopenssl.so
OBJS     =  nsopenssl.o config.o init.o ssl.o thread.o tclcmds.o
HDRS     =  nsopenssl.h tclcmds.h config.h thread.h
TCLMOD   =  https.tcl

ifdef BSAFE
    MODLIBS  =  -L$(OPENSSL)/lib -L$(BSAFE)/lib -lssl -lcrypto \
                -lBSAFEglue -lcrypto -lbsafe -lBSAFEglue
else
    MODLIBS  =  -L$(OPENSSL)/lib -lssl -lcrypto 
endif

CFLAGS   +=  -I$(OPENSSL)/include

include  $(NSHOME)/include/Makefile.module

nsopenssl.h: check-env

.PHONY: check-env
check-env:
	@if [ "$(OPENSSL)" = "" ]; then \
	    echo "** "; \
	    echo "** OPENSSL variable not set."; \
	    echo "** nsopenssl.so will not be built."; \
	    echo "** Usage: make OPENSSL=/path/to/openssl"; \
	    echo "** Usage: make install OPENSSL=/path/to/openssl INST=/path/to/aolserver"; \
	    echo "** "; \
	    exit 1; \
	fi

# This overrides the install directive in $(NSHOME)/include/Makefile.module
install: all
	$(RM) $(INSTBIN)/$(MOD)
	$(CP) $(MOD) $(INSTBIN)
	$(MKDIR) $(INSTTCL)
	$(CP) $(TCLMOD) $(INSTTCL)

#################################################################
# NOTE!!! Solaris users *might* need the following, 
# but you'll need to modify it to point to where 
# your libgcc.a lives. Replace the MODLIBS above with
# this:
#
#   MODLIBS  =  -L$(OPENSSL)/lib -lssl -lcrypto \
#   -L/usr/local/products/gcc-2.95/lib/gcc-lib/sparc-sun-solaris2.5.1/2.95 -lgcc

#################################################################
# For development purposes, put the GCCOPT above somewhere
# to turn off 'no-unused' so gcc will report unused funcs
# and variables.
#
#   GCCOPT       =   $(GCCOPTIMIZE) -fPIC -Wall


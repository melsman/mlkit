$Header$


SSLv2, SSLv3, TLSv1 Module
--------------------------

This software is now production-quality. I have load-tested it under
RedHat 6.x and Debian 2.2 Linux. 

This module *REQUIRES* OpenSSL 0.9.6 or higher.
This module also *REQUIRES* that you use nsd8x, not nsd76


Feature Highlights
------------------

 * Open Source software (AOLserver Public License or GPL)
 * Useable for both commercial and non-commercial use
 * 128-bit strong cryptography world-wide
 * Support for SSLv2, SSLv3 and TLSv1 protocols
 * Support for both RSA and Diffie-Hellman ciphers
 * Support for client certificate verification
 * Clean, reviewable ANSI C source code


Compiling the code
------------------

To compile this code, just type:

gmake OPENSSL=/usr/local/ssl

or:

export OPENSSL=/usr/local/ssl
gmake
gmake install INST=/usr/local/aolserver

**** NOTE: Solaris Users:

You may have to add an extra library search path and the gcc library
for the linker to work. Specifically, you may need to change this:

MODLIBS  =  -L$(OPENSSL) -lssl -lcrypto \

to this:

MODLIBS  =  -L$(OPENSSL) -lssl -lcrypto \
 -L/usr/local/lib/gcc-lib/sparc-sun-solaris2.6/2.8.1 -lgcc

Adjust for which version of Solaris and GCC you're using.
Much thanks to Ron Patterson, ron.patterson@corp.usa.net, for
this information.


See nsd.tcl for a sample configuration that uses SSL on port 8443.

To test the server, put the sample configuration from nsd.tcl into
your server's nsd.tcl, copy the sample *.pem files to
$INST/servers/server1/modules/nsopenssl, and start your server.  Visit
https://hostname:8443/.

The default key and certificate for the non-existent 'SnakeOil'
company are included for testing purposes. Do not use these on a real
server -- they are for testing only.


Development Environment
-----------------------

The code was developed under Debian 2.2 with OpenSSL 0.9.6. It will
probably run without too many problems on different flavors of UNIX.

You can see debug output by putting the server itself in debug
mode. There isn't a separate debug option for nsopenssl.

OpenSSL must be compiled as position-independent, but it does not
build that way in the configuration that comes from the OpenSSL
distribution.  The OpenSSL 0.9.5a release doesn't appear to have an
option for this so you'll have to include it in your compile step.

gmake CC="gcc -fPIC"

In addition, some operating systems (Solaris x86) may not support
position-independent code that has inline assembler.  The
configuration that seems to work on these platforms is:

./config no-asm
Then, followed by the same gmake step as before:
gmake CC="gcc -fPIC"


Configuration Options
---------------------

ns_section "ns/server/${servername}/module/nsopenssl"
ns_param port                     $httpsport
ns_param hostname                 $hostname
ns_param CertFile                 certfile.pem
ns_param KeyFile                  keyfile.pem
ns_param Protocol                 All
#ns_param Protocol                 SSLv2
#ns_param Protocol                 SSLv3
#ns_param Protocol                 TLSv1
#ns_param CipherSuite              "ALL:!ADH:RC4+RSA:+HIGH:+MEDIUM:+LOW:+SSLv2:+EXP"
#ns_param SessionCache		   false
#ns_param SessionCacheSize         512
#ns_param SessionCacheTimeout      300
ns_param ClientVerify             true
ns_param CADir                    ca
ns_param CAFile                   ca.pem 
ns_param Trace                    false
ns_param RandomFile               /some/file
ns_param SeedBytes                1024

# NOT IMPLEMENTED YET:
#ns_param VerifyDepth            3
#ns_param CRLDir                 crl
#ns_param CRLFile                crl.pem

ns_section "ns/server/${servername}/modules"
ns_param nsopenssl    ${bindir}/nsopenssl.${ext}


Configuration Notes
-------------------

Session caching enabled by default.

RandomFile isn't necessary, but if you want to use your own random
bits, you can set this. On Linux, it won't matter: OpenSSL will use
/dev/urandom to transparently seed the PRNG.

SeedBytes is optional; it tells how many bytes to seed the PRNG with, but
only if the PRNG actually needs seeding. It defaults to 1024 bytes. If
seeding the PRNG fails, bump this number up.

WARNING!!! If the client sends an invalid certificate, the connection
is still accepted. Use 'ns_openssl clientcert valid' in your Tcl code or ADP
page to determine if you received a client certificate and if it was
valid.

NOTE: Your key.pem file must *not* be protected by a passphrase or the server
won't start.


Tcl Interface Commands
----------------------

ns_openssl info
  - returns a Tcl list containing the SSL libary name, SSL library version,
    Crypto library name, Crypto library version.

ns_openssl protocol
  - returns the protocol used by the current connection as a string: 
    SSLv2, SSLv3, TLSv1 or UNKNOWN

ns_openssl cipher name
  - returns the name of the cipher being used by the current connection
    (e.g. RSA_MD5)

ns_openssl cipher strength
  - returns the strength of the cipher being used by the current connection
    as a number (e.g. 40, 56, 128 etc.)

ns_openssl clientcert exists
  - returns 0 if no client certificate exists, or a 1 if a client
    certificate does exist.

ns_openssl clientcert valid
  - returns 1 if client certificate was obtained *and* it is valid; 0 otherwise.

ns_openssl clientcert version
  - returns a Tcl string containing the certificate's version number, e.g. "3".

ns_openssl clientcert serial
  - returns a Tcl string containing the certificate's serial number, e.g. "27C6".

ns_openssl clientcert subject
  - returns a Tcl string containing the certificate's subject name,
    e.g. "/C=US/O=U.S. Government/OU=DoD/OU=PKI/OU=USAF/CN=Goodwin.Scott.S.0300074002"

ns_openssl clientcert issuer
  - returns a Tcl string containing the certificate's issuer name,
    e.g. "/C=US/O=U.S. Government/OU=DoD/OU=PKI/CN=Med CA-2"

ns_openssl clientcert notbefore
  - returns a Tcl string containing the certificate's valid start date,
    e.g. "Aug 28 20:00:38 2000 GMT"

ns_openssl clientcert notafter
  - returns a Tcl string containing the certificate's valid end date,
    e.g. "Aug 28 20:00:38 2002 GMT"

ns_openssl clientcert signature_algorithm
  - returns a Tcl string containing the algorithm used for the signature,
    e.g. "sha1WithRSAEncryption"

ns_openssl clientcert key_algorithm
  - returns a Tcl string containing the algorithm used for the key,
    e.g. "rsaEncryption"

ns_openssl clientcert pem
  - returns a Tcl string containing the client's PEM-formatted certificate,
    which should have "--- BEGIN CERTIFICATE ---" and
    "--- END CERTIFICATE ---" lines in it.


Open Issues
-----------

See the TODO file.


Copyright Notices
-----------------

The nsopenssl module was originally written and Copyrighted by Stefan
Arentz. Parts of it are also copyrighted by Scott S. Goodwin. It is
distributed under the AOLserver Public License. See the file
license.txt for more information.

This product includes software developed by the OpenSSL Project for
use in the OpenSSL Toolkit. (http://www.openssl.org/)

This product includes cryptographic software written by Eric Young
(eay@cryptsoft.com).


Related Links
-------------

  http://www.aolserver.com  AOLserver homepage
  http://www.openssl.org    OpenSSL toolkit homepage
  http://www.modssl.org     OpenSSL module for Apache
  http://www.thawte.com     For getting test certificates
  http://scottg.net         Information on AOLserver and this module

$Header$

SSLv2, SSLv3, TLSv1 Module
--------------------------

This module requires OpenSSL 0.9.6 or higher. This module also
requires that you use nsd8x, not nsd76.


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

to something like this:

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
probably run on different flavors of UNIX, though minor modifications
may be necessary.

You can see debug output by putting the server itself in debug
mode. There isn't a separate debug option for nsopenssl, but you can
turn on Trace to see the handshaking in action as clients connect.

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

For versions prior to 2.x:

ns_section "ns/server/${servername}/module/nsopenssl"
ns_param port                     $httpsport
ns_param hostname                 $hostname
ns_param address                  192.168.0.2  # will use global address param if this isn't set here
ns_param CertFile                 certfile.pem
ns_param KeyFile                  keyfile.pem
ns_param Protocols                All
#ns_param Protocols                "SSLv2,SSLv3, TLSv1, all"
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


For 2.x and above:


ns_section "ns/server/${servername}/module/nsopenssl"

# NSD-driven connections:
ns_param ServerPort                      $httpsport
ns_param ServerHostname                  $hostname
ns_param ServerAddress                   $address
ns_param ServerCertFile                  certfile.pem
ns_param ServerKeyFile                   keyfile.pem
ns_param ServerProtocols                 "SSLv2, SSLv3, TLSv1"
ns_param ServerCipherSuite               "ALL:!ADH:RC4+RSA:+HIGH:+MEDIUM:+LOW:+SSLv2:+EXP"
ns_param ServerSessionCache              false
ns_param ServerSessionCacheID            1
ns_param ServerSessionCacheSize          512
ns_param ServerSessionCacheTimeout       300
ns_param ServerPeerVerify                true
ns_param ServerPeerVerifyDepth           3
ns_param ServerCADir                     ca
ns_param ServerCAFile                    ca.pem
ns_param ServerTrace                     false

# For listening and accepting SSL connections via Tcl/C API:
ns_param SockServerCertFile              certfile.pem
ns_param SockServerKeyFile               keyfile.pem
ns_param SockServerProtocols             "SSLv2, SSLv3, TLSv1"
ns_param SockServerCipherSuite           "ALL:!ADH:RC4+RSA:+HIGH:+MEDIUM:+LOW:+SSLv2:+EXP"
ns_param SockServerSessionCache          false
ns_param SockServerSessionCacheID        2
ns_param SockServerSessionCacheSize      512
ns_param SockServerSessionCacheTimeout   300
ns_param SockServerPeerVerify            true
ns_param SockServerPeerVerifyDepth       3
ns_param SockServerCADir                 internal_ca
ns_param SockServerCAFile                internal_ca.pem
ns_param SockServerTrace                 false

# Outgoing SSL connections
ns_param SockClientCertFile              clientcertfile.pem
ns_param SockClientKeyFile               clientkeyfile.pem
ns_param SockClientProtocols             "SSLv2, SSLv3, TLSv1"
ns_param SockClientCipherSuite           "ALL:!ADH:RC4+RSA:+HIGH:+MEDIUM:+LOW:+SSLv2:+EXP"
ns_param SockClientSessionCache          false
ns_param SockClientSessionCacheID        3
ns_param SockClientSessionCacheSize      512
ns_param SockClientSessionCacheTimeout   300
ns_param SockClientPeerVerify            true
ns_param SockServerPeerVerifyDepth       3
ns_param SockClientCADir                 ca
ns_param SockClientCAFile                ca.pem
ns_param SockClientTrace                 false

# OpenSSL library support:
ns_param RandomFile                      /some/file
ns_param SeedBytes                       1024


And to load the module:

ns_section "ns/server/${servername}/modules"
ns_param nsopenssl    ${bindir}/nsopenssl.${ext}


Configuration Notes
-------------------

Session caching is turned on by default.

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


New Commands in 2.0:

ns_openssl_sockopen hostname port
  - opens an SSL connection to the host on the specified port.

ns_openssl_socklisten 
  - listens for SSL connections from clients

ns_openssl_sockaccept 
  - accepts SSL connections from clients

ns_openssl_sockcallback 
  - run a script when the SSL socket is in a certain state

ns_openssl_socklistencallback 
  - listen for SSL connections and run a script when a client connects

ns_httpsget 
  - grab a page from an SSL server

ns_httpsopen 
  - open an SSL connection to a server

ns_openssl_geturl 
  - grab a page from an SSL server


To use the last three commands, you'll need to have https.tcl installed in
your server's module/tcl directory.


Copyright Notices
-----------------

The nsopenssl module was originally written and Copyrighted by Stefan
Arentz. Parts of it are also copyrighted by Scott S. Goodwin. It is
distributed under the AOLserver Public License. See the file
license.txt for more information.

This product includes software developed by the OpenSSL Project for
use in the OpenSSL Toolkit. (http://www.openssl.org/)

This product links to cryptographic software (OpenSSL) originally
written by Eric Young (eay@cryptsoft.com). There is no cryptographic
software within the source code of this module.


Related Links
-------------

  http://scottg.net         Information on AOLserver and this module
  http://www.opennsd.org    OpenNSD site
  http://www.aolserver.com  AOLserver homepage
  http://www.openssl.org    OpenSSL toolkit homepage
  http://www.modssl.org     OpenSSL module for Apache
  http://www.thawte.com     For getting test certificates


Developers
----------

If you make any mods to nsopenssl and commit them, please be sure to
tag after your commit. This allows anyone to go and get a named (i.e.
tagged) snapshot of the code.

You tag by doing:

cvs -q tag nsopenssl-2_2_beta_1

Make sure you know what the latest tag is and increment accordingly.



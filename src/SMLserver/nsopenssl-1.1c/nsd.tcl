#
# nsd.tcl --
#
#      Sample nsopenssl configuration.
#
# $Header$
#

ns_section "ns/server/${servername}/module/nsopenssl"   
ns_param port                     $httpsport
ns_param hostname                 $hostname
ns_param CertFile                 certfile.pem
ns_param KeyFile                  keyfile.pem
ns_param Protocol                 All
#ns_param Protocol                 SSLv2
#ns_param Protocol                 SSLv3
#ns_param Protocol                 TLSv1
#ns_param CipherSuite              "ALL:!ADH:RC4+RSA:+HIGH:+MEDIUM:+LOW:+SSLv2:+
EXP"
#ns_param SessionCache        true
#ns_param SessionCacheSize         512
#ns_param SessionCacheTimeout      300
ns_param ClientVerify             true
ns_param CADir                    ca
ns_param CAFile                   ca.pem
ns_param Trace                    false
ns_param RandomFile               /some/file

ns_section "ns/server/${servername}/modules"
ns_param nsopenssl    ${bindir}/nsopenssl${ext}


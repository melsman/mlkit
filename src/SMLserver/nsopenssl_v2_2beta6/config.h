/*
 * The contents of this file are subject to the AOLserver Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://aolserver.lcs.mit.edu/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is AOLserver Code and related documentation
 * distributed by AOL.
 *
 * The Initial Developer of the Original Code is America Online,
 * Inc. Portions created by AOL are Copyright (C) 1999 America Online,
 * Inc. All Rights Reserved.
 *
 * Copyright (C) 2000-2002 Scott S. Goodwin
 * Copyright (C) 2000 Rob Mayoff
 * Copyright (C) 1999 Stefan Arentz.
 *
 * Alternatively, the contents of this file may be used under the terms
 * of the GNU General Public License (the "GPL"), in which case the
 * provisions of GPL are applicable instead of those above.  If you wish
 * to allow use of your version of this file only under the terms of the
 * GPL and not to allow others to use your version of this file under the
 * License, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the GPL.
 * If you do not delete the provisions above, a recipient may use your
 * version of this file under either the License or the GPL.
 */

/* @(#) $Header$ */

#define DEFAULT_PORT                          443
#define DEFAULT_PROTOCOL                      "https"
#define DEFAULT_NAME                          "nsopenssl"

/*
 * Used to determine whether NSD is handling the
 * underlying socket or not.
 */

#define CONNTYPE_SSL_NSD                       0
#define CONNTYPE_SSL_SOCK                      1

/*
 * Used to determine if we're the client or the server
 * for the connection.
 */

#define ROLE_SSL_CLIENT                        0
#define ROLE_SSL_SERVER                        1

#define STR_SOCK_CLIENT                        "sockclient"
#define STR_SOCK_SERVER                        "sockserver"
#define STR_NSD_SERVER                         "nsdserver"

#define CONFIG_SERVER_TRACE                    "ServerTrace"
#define DEFAULT_SERVER_TRACE                   NS_FALSE

#define CONFIG_SOCKSERVER_TRACE                "SockServerTrace"
#define DEFAULT_SOCKSERVER_TRACE               NS_FALSE

#define CONFIG_SOCKCLIENT_TRACE                "SockClientTrace"
#define DEFAULT_SOCKCLIENT_TRACE               NS_FALSE

#define CONFIG_SERVER_CIPHERSUITE              "ServerCipherSuite"
#define DEFAULT_SERVER_CIPHERSUITE             SSL_DEFAULT_CIPHER_LIST

#define CONFIG_SOCKSERVER_CIPHERSUITE          "SockServerCipherSuite"
#define DEFAULT_SOCKSERVER_CIPHERSUITE         SSL_DEFAULT_CIPHER_LIST

#define CONFIG_SOCKCLIENT_CIPHERSUITE          "SockClientCipherSuite"
#define DEFAULT_SOCKCLIENT_CIPHERSUITE         SSL_DEFAULT_CIPHER_LIST

#define	CONFIG_SERVER_PROTOCOLS                "ServerProtocols"
#define DEFAULT_SERVER_PROTOCOLS               "All"

#define	CONFIG_SOCKSERVER_PROTOCOLS            "SockServerProtocols"
#define DEFAULT_SOCKSERVER_PROTOCOLS           "All"

#define	CONFIG_SOCKCLIENT_PROTOCOLS            "SockClientProtocols"
#define DEFAULT_SOCKCLIENT_PROTOCOLS           "All"

#define CONFIG_SERVER_CERTFILE                 "ServerCertFile"
#define DEFAULT_SERVER_CERTFILE                "certificate.pem"

#define CONFIG_SOCKSERVER_CERTFILE             "SockServerCertFile"
#define DEFAULT_SOCKSERVER_CERTFILE            "certificate.pem"

#define CONFIG_SOCKCLIENT_CERTFILE             "SockClientCertFile"
#define DEFAULT_SOCKCLIENT_CERTFILE            NULL

#define CONFIG_SERVER_KEYFILE                  "ServerKeyFile"
#define DEFAULT_SERVER_KEYFILE                 "key.pem"

#define CONFIG_SOCKSERVER_KEYFILE              "SockServerKeyFile"
#define DEFAULT_SOCKSERVER_KEYFILE             "key.pem"

#define CONFIG_SOCKCLIENT_KEYFILE              "SockClientKeyFile"
#define DEFAULT_SOCKCLIENT_KEYFILE             NULL

#define CONFIG_SERVER_CAFILE                   "ServerCAFile"
#define DEFAULT_SERVER_CAFILE                  "ca.pem"

#define CONFIG_SOCKSERVER_CAFILE               "SockServerCAFile"
#define DEFAULT_SOCKSERVER_CAFILE              "ca.pem"

#define CONFIG_SOCKCLIENT_CAFILE               "SockClientCAFile"
#define DEFAULT_SOCKCLIENT_CAFILE              "ca.pem"

#define CONFIG_SERVER_CADIR                    "ServerCADir"
#define DEFAULT_SERVER_CADIR                   "ca"

#define CONFIG_SOCKSERVER_CADIR                "SockServerCADir"
#define DEFAULT_SOCKSERVER_CADIR               "ca"

#define CONFIG_SOCKCLIENT_CADIR                "SockClientCADir"
#define DEFAULT_SOCKCLIENT_CADIR               "ca"

#define CONFIG_SERVER_SESSIONCACHE             "ServerSessionCache"
#define DEFAULT_SERVER_SESSIONCACHE            NS_TRUE

#define CONFIG_SOCKSERVER_SESSIONCACHE         "SockServerSessionCache"
#define DEFAULT_SOCKSERVER_SESSIONCACHE        NS_TRUE

#define CONFIG_SOCKCLIENT_SESSIONCACHE         "SockClientSessionCache"
#define DEFAULT_SOCKCLIENT_SESSIONCACHE        NS_TRUE

#define CONFIG_SERVER_SESSIONCACHEID           "ServerSessionCacheId"
#define DEFAULT_SERVER_SESSIONCACHEID          1

#define CONFIG_SOCKSERVER_SESSIONCACHEID       "SockServerSessionCacheId"
#define DEFAULT_SOCKSERVER_SESSIONCACHEID      2

#define CONFIG_SOCKCLIENT_SESSIONCACHEID       "SockClientSessionCacheId"
#define DEFAULT_SOCKCLIENT_SESSIONCACHEID      3

#define CONFIG_SERVER_SESSIONCACHESIZE         "ServerSessionCacheSize"
#define DEFAULT_SERVER_SESSIONCACHESIZE        128

#define CONFIG_SOCKSERVER_SESSIONCACHESIZE     "SockServerSessionCacheSize"
#define DEFAULT_SOCKSERVER_SESSIONCACHESIZE    128

#define CONFIG_SOCKCLIENT_SESSIONCACHESIZE     "SockClientSessionCacheSize"
#define DEFAULT_SOCKCLIENT_SESSIONCACHESIZE    128

#define CONFIG_SERVER_SESSIONTIMEOUT           "ServerSessionTimeout"
#define DEFAULT_SERVER_SESSIONTIMEOUT          300

#define CONFIG_SOCKSERVER_SESSIONTIMEOUT       "SockServerSessionTimeout"
#define DEFAULT_SOCKSERVER_SESSIONTIMEOUT      300

#define CONFIG_SOCKCLIENT_SESSIONTIMEOUT       "SockClientSessionTimeout"
#define DEFAULT_SOCKCLIENT_SESSIONTIMEOUT      300

#define CONFIG_SERVER_SOCKTIMEOUT              "ServerSockTimeout"
#define DEFAULT_SERVER_SOCKTIMEOUT             30

#define CONFIG_SOCKSERVER_SOCKTIMEOUT          "SockServerSockTimeout"
#define DEFAULT_SOCKSERVER_SOCKTIMEOUT         30

#define CONFIG_SOCKCLIENT_SOCKTIMEOUT          "SockClientSockTimeout"
#define DEFAULT_SOCKCLIENT_SOCKTIMEOUT         30

#define CONFIG_SERVER_BUFFERSIZE               "ServerBufferSize"
#define DEFAULT_SERVER_BUFFERSIZE              16384

#define CONFIG_SOCKSERVER_BUFFERSIZE           "SockServerBufferSize"
#define DEFAULT_SOCKSERVER_BUFFERSIZE          16384

#define CONFIG_SOCKCLIENT_BUFFERSIZE           "SockClientBufferSize"
#define DEFAULT_SOCKCLIENT_BUFFERSIZE          16384

#define CONFIG_SERVER_PEERVERIFY               "ServerPeerVerify"
#define DEFAULT_SERVER_PEERVERIFY              NS_FALSE

#define CONFIG_SOCKSERVER_PEERVERIFY           "SockServerPeerVerify"
#define DEFAULT_SOCKSERVER_PEERVERIFY          NS_FALSE

#define CONFIG_SOCKCLIENT_PEERVERIFY           "SockClientPeerVerify"
#define DEFAULT_SOCKCLIENT_PEERVERIFY          NS_TRUE

#define CONFIG_SERVER_VERIFYDEPTH              "ServerPeerVerifyDepth"
#define DEFAULT_SERVER_VERIFYDEPTH             10

#define CONFIG_SOCKSERVER_VERIFYDEPTH          "SockServerPeerVerifyDepth"
#define DEFAULT_SOCKSERVER_VERIFYDEPTH         10

#define CONFIG_SOCKCLIENT_VERIFYDEPTH          "SockClientPeerVerifyDepth"
#define DEFAULT_SOCKCLIENT_VERIFYDEPTH         10

#define CONFIG_MODULE_DIR           "ModuleDir"

#define CONFIG_RANDOMFILE           "RandomFile"

/*
 * If PRNG fails to seed, increase this number in the
 * nsd.tcl file.
 */

#define CONFIG_SEEDBYTES            "SeedBytes"
#define DEFAULT_SEEDBYTES           1024

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
 * Copyright (C) 1999 Stefan Arentz.
 * Copyright (C) 2000 Rob Mayoff
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

#define DEFAULT_PORT                443
#define DEFAULT_PROTOCOL            "https"
#define DEFAULT_NAME                "nsopenssl"

#define CONFIG_TRACE                "Trace"
#define DEFAULT_TRACE               NS_FALSE

#define CONFIG_CIPHERSUITE          "CipherSuite"
#define DEFAULT_CIPHERSUITE         SSL_DEFAULT_CIPHER_LIST

#define	CONFIG_PROTOCOLS            "Protocols"

#define CONFIG_CERTFILE             "CertFile"
#define DEFAULT_CERTFILE            "certificate.pem"

#define CONFIG_KEYFILE              "KeyFile"
#define DEFAULT_KEYFILE             "key.pem"

#define CONFIG_CAFILE               "CAFile"
#define DEFAULT_CAFILE              "ca.pem"

#define CONFIG_CADIR                "CADir"
#define DEFAULT_CADIR               "ca"

#define CONFIG_SESSIONCACHE         "SessionCache"
#define DEFAULT_SESSIONCACHE        NS_TRUE

#define CONFIG_SESSIONCACHESIZE     "SessionCacheSize"
#define DEFAULT_SESSIONCACHESIZE    128

#define CONFIG_SESSIONTIMEOUT       "SessionTimeout"
#define DEFAULT_SESSIONTIMEOUT      300

#define CONFIG_SOCKTIMEOUT          "SockTimeout"
#define DEFAULT_SOCKTIMEOUT         30

#define CONFIG_BUFFERSIZE           "BufferSize"
#define DEFAULT_BUFFERSIZE          16384

#define CONFIG_CLIENTVERIFY         "ClientVerify"
#define DEFAULT_CLIENTVERIFY        NS_FALSE

#define CONFIG_RANDOMFILE           "RandomFile"

/* If PRNG fails to seed, up this number in your nsd.tcl */
#define CONFIG_SEEDBYTES            "SeedBytes"
#define DEFAULT_SEEDBYTES           1024

char *ConfigStringDefault(char *module, char *path, char *name,
    char *def);
int ConfigBoolDefault(char *module, char *path, char *name,
    int def);
int ConfigIntDefault(char *module, char *path, char *name,
    int def);
char *ConfigPathDefault(char *module, char *path, char *name,
    char *dir, char *def);


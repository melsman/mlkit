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

static const char *RCSID =
    "@(#) $Header$, compiled: "
    __DATE__ " " __TIME__;

#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifndef WIN32
#include <dirent.h>
#endif
#include "nsopenssl.h"
#include "config.h"
#include "thread.h"

static int InitializeOpenSSL (void);
static int CheckModuleDir (NsOpenSSLDriver * sdPtr);
static int MakeDriverSSLContext (NsOpenSSLDriver * sdPtr);

static int MakeSockServerSSLContext (NsOpenSSLDriver * sdPtr);
static int MakeSockClientSSLContext (NsOpenSSLDriver * sdPtr);

static int SetProtocols (char *module, SSL_CTX * context, char *protocols);
static int SetCipherSuite (char *module, SSL_CTX * context,
			   char *cipherSuite);
static int LoadCertificate (char *module, SSL_CTX * context, char *certFile);
static int LoadKey (char *module, SSL_CTX * context, char *keyFile);
static int CheckKey (char *module, SSL_CTX * context);
static int LoadCACerts (char *module, SSL_CTX * context, char *caFile,
			char *caDir);
static int InitLocation (NsOpenSSLDriver * sdPtr);
static int InitSessionCache (char *module, SSL_CTX * context,
			     int cacheEnabled, int cacheId, int cacheTimeout,
			     int cacheSize);
static int PeerVerifyCallback (int preverify_ok, X509_STORE_CTX * x509_ctx);

/*
 * For generating temporary RSA keys. Temp RSA keys are REQUIRED if
 * you want 40-bit encryption to work in old export browsers.
 */

static int AddEntropyFromRandomFile (NsOpenSSLDriver * sdPtr, long maxbytes);
static int PRNGIsSeeded (NsOpenSSLDriver * sdPtr);
static int SeedPRNG (NsOpenSSLDriver * sdPtr);
static RSA *IssueTmpRSAKey (SSL * ssl, int export, int keylen);

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLCreateDriver --
 *
 *       Create the SSL driver.
 *
 * Results:
 *       An NsOpenSSLDriver* or NULL.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

extern NsOpenSSLDriver *
#ifndef NS_MAJOR_VERSION
NsOpenSSLCreateDriver (char *server, char *module, Ns_DrvProc * procs)
#else
NsOpenSSLCreateDriver (char *server, char *module)
#endif
{
    NsOpenSSLDriver *sdPtr = NULL;

    sdPtr = (NsOpenSSLDriver *) ns_calloc (1, sizeof *sdPtr);

    Ns_MutexSetName (&sdPtr->lock, module);
    sdPtr->server = server;
    sdPtr->module = module;
    sdPtr->refcnt = 1;
    sdPtr->lsock = INVALID_SOCKET;
    sdPtr->configPath = Ns_ConfigGetPath (server, module, NULL);

    if (NsOpenSSLInitThreads () == NS_ERROR
	|| InitializeOpenSSL () == NS_ERROR
	|| CheckModuleDir (sdPtr) == NS_ERROR
	|| MakeDriverSSLContext (sdPtr) == NS_ERROR
	|| MakeSockServerSSLContext (sdPtr) == NS_ERROR
	|| MakeSockClientSSLContext (sdPtr) == NS_ERROR
	|| InitLocation (sdPtr) == NS_ERROR) {
	NsOpenSSLFreeDriver (sdPtr);
	return NULL;
    }

    sdPtr->randomFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
					   CONFIG_RANDOMFILE, sdPtr->dir,
					   NULL);

    /*
     * If the OpenSSL's PRNG is not seeded, seed it now.
     */

    if (PRNGIsSeeded (sdPtr) != NS_TRUE) {
	Ns_Log (Warning, "%s: PRNG does not have enough entropy",
		sdPtr->module);
	SeedPRNG (sdPtr);
	if (PRNGIsSeeded (sdPtr) == NS_TRUE) {
	    Ns_Log (Notice, "%s: PRNG now has enough entropy", sdPtr->module);
	} else {
	    Ns_Log (Error, "%s: PRNG STILL does not have enough entropy",
		    sdPtr->module);
	}
    }

    sdPtr->timeout = ConfigIntDefault (module, sdPtr->configPath,
				       CONFIG_SERVER_SOCKTIMEOUT,
				       DEFAULT_SERVER_SOCKTIMEOUT);
    if (sdPtr->timeout < 1) {
	sdPtr->timeout = DEFAULT_SERVER_SOCKTIMEOUT;
    }

    sdPtr->bufsize = ConfigIntDefault (module, sdPtr->configPath,
				       CONFIG_SERVER_BUFFERSIZE,
				       DEFAULT_SERVER_BUFFERSIZE);
    if (sdPtr->bufsize < 1) {
	sdPtr->bufsize = DEFAULT_SERVER_BUFFERSIZE;
    }

#ifndef NS_MAJOR_VERSION
    sdPtr->driver = Ns_RegisterDriver (server, module, procs, sdPtr);
    if (sdPtr->driver == NULL) {
	NsOpenSSLFreeDriver (sdPtr);
	return NULL;
    }
#endif

    return sdPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLFreeDriver --
 *
 *      Destroy an NsOpenSSLDriver.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern void
NsOpenSSLFreeDriver (NsOpenSSLDriver * sdPtr)
{
    Ns_OpenSSLConn *scPtr;

    Ns_Log (Debug, "%s: freeing(%p)",
	    sdPtr == NULL ? DRIVER_NAME : sdPtr->module, sdPtr);

    if (sdPtr != NULL) {
	while ((scPtr = sdPtr->firstFreePtr) != NULL) {
	    sdPtr->firstFreePtr = scPtr->nextPtr;
	    ns_free (scPtr);
	}
	Ns_MutexDestroy (&sdPtr->lock);
	if (sdPtr->context != NULL)
	    SSL_CTX_free (sdPtr->context);
	if (sdPtr->sockServerContext != NULL)
	    SSL_CTX_free (sdPtr->sockServerContext);
	if (sdPtr->sockClientContext != NULL)
	    SSL_CTX_free (sdPtr->sockClientContext);
	if (sdPtr->dir != NULL)
	    ns_free (sdPtr->dir);
	if (sdPtr->address != NULL)
	    ns_free (sdPtr->address);
	if (sdPtr->location != NULL)
	    ns_free (sdPtr->location);
	if (sdPtr->randomFile != NULL)
	    ns_free (sdPtr->randomFile);
	ns_free (sdPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * InitializeOpenSSL --
 *
 *       Initialize the SSL library.
 *
 * Results:
 *       NS_OK
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

static int
InitializeOpenSSL (void)
{
    SSL_load_error_strings ();
    OpenSSL_add_ssl_algorithms ();
    SSL_library_init ();
    X509V3_add_standard_extensions ();

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CheckModuleDir --
 *
 *       Set sdPtr->dir to the absolute path of the module's directory.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       May create the directory on disk.
 *
 *----------------------------------------------------------------------
 */

static int
CheckModuleDir (NsOpenSSLDriver * sdPtr)
{
    char *value;
    Ns_DString ds;

    value = Ns_ConfigGetValue (sdPtr->configPath, CONFIG_MODULE_DIR);

    if (value == NULL) {
	Ns_DStringInit (&ds);
	Ns_ModulePath (&ds, sdPtr->server, sdPtr->module, NULL);
	sdPtr->dir = Ns_DStringExport (&ds);
	Ns_Log (Notice, "Module directory defaults to %s", sdPtr->dir);
	if (mkdir (sdPtr->dir, 0755) != 0 && errno != EEXIST) {
	    Ns_Log (Error, "mkdir(%s) failed: %s", sdPtr->dir,
		    strerror (errno));
	    ns_free (sdPtr->dir);
	    sdPtr->dir = NULL;
	    return NS_ERROR;
	}
    } else {
	if (Ns_PathIsAbsolute (value)) {
	    sdPtr->dir = ns_strdup (value);
	} else {
	    Ns_DStringInit (&ds);
	    Ns_DStringVarAppend (&ds, sdPtr->dir, value, NULL);
	    sdPtr->dir = Ns_DStringExport (&ds);
	    Ns_DStringFree (&ds);
	}
	Ns_Log (Notice, "Module directory set by ModuleDir to %s",
		sdPtr->dir);
    }

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MakeDriverSSLContext --
 *
 *       Create a new SSL context for the specified SSLDriver and set
 *       default values or values from the configuration file.
 *
 * Results:
 *       NS_OK or NS_ERROR
 *
 * Side effects:
 *       Sets sdPtr->context.
 *
 *----------------------------------------------------------------------
 */

static int
MakeDriverSSLContext (NsOpenSSLDriver * sdPtr)
{
    char *protocols;
    char *cipherSuite;
    char *certFile;
    char *keyFile;
    char *caFile;
    char *caDir;
    int connTrace;
    int peerVerify;
    int verifyDepth;
    int cacheEnabled;
    int cacheId;
    int cacheSize;
    int cacheTimeout;

    sdPtr->context = SSL_CTX_new (SSLv23_server_method ());
    if (sdPtr->context == NULL) {
	Ns_Log (Error, "%s: error creating SSL context", sdPtr->module);
	return NS_ERROR;
    }

    SSL_CTX_set_app_data (sdPtr->context, sdPtr);

    /*
     * Enable SSL bug compatibility.
     */

    SSL_CTX_set_options (sdPtr->context, SSL_OP_ALL);

    /*
     * This apparently prevents some sort of DH attack.
     */

    SSL_CTX_set_options (sdPtr->context, SSL_OP_SINGLE_DH_USE);

    /*
     * Temporary key callback required for 40-bit export browsers
     */

    SSL_CTX_set_tmp_rsa_callback (sdPtr->context, IssueTmpRSAKey);

    /*
     * Set peer verify and verify depth
     */

    peerVerify = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				    CONFIG_SERVER_PEERVERIFY,
				    DEFAULT_SERVER_PEERVERIFY);

    if (peerVerify) {
	SSL_CTX_set_verify (sdPtr->context,
			    (SSL_VERIFY_PEER | SSL_VERIFY_CLIENT_ONCE),
			    PeerVerifyCallback);

	verifyDepth =
	    (int) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				    CONFIG_SERVER_VERIFYDEPTH,
				    DEFAULT_SERVER_VERIFYDEPTH);
	SSL_CTX_set_verify_depth (sdPtr->context, verifyDepth);

    } else {
	SSL_CTX_set_verify (sdPtr->context, SSL_VERIFY_NONE, NULL);
    }

    /*
     * Set SSL handshake and connection tracing
     */

    connTrace = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				   CONFIG_SERVER_TRACE, DEFAULT_SERVER_TRACE);

    if (connTrace) {
	SSL_CTX_set_info_callback (sdPtr->context, NsOpenSSLTrace);
    }

    /*
     * Set protocols
     */

    protocols = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
				     CONFIG_SERVER_PROTOCOLS,
				     DEFAULT_SERVER_PROTOCOLS);

    if (SetProtocols (sdPtr->module, sdPtr->context, protocols) != NS_OK)
	return NS_ERROR;

    /*
     * Set cipher suite
     */

    cipherSuite = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
				       CONFIG_SERVER_CIPHERSUITE,
				       DEFAULT_SERVER_CIPHERSUITE);

    if (SetCipherSuite (sdPtr->module, sdPtr->context, cipherSuite) != NS_OK)
	return NS_ERROR;

    /*
     * Load certificate
     */

    certFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				  CONFIG_SERVER_CERTFILE, sdPtr->dir,
				  DEFAULT_SERVER_CERTFILE);

    if (LoadCertificate (sdPtr->module, sdPtr->context, certFile) != NS_OK)
	return NS_ERROR;

    /*
     * Load the key that unlocks the certificate
     */

    keyFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				 CONFIG_SERVER_KEYFILE, sdPtr->dir,
				 DEFAULT_SERVER_KEYFILE);

    if (LoadKey (sdPtr->module, sdPtr->context, keyFile) != NS_OK)
	return NS_ERROR;

    /*
     * Check the key against the certificate
     */

    if (CheckKey (sdPtr->module, sdPtr->context) != NS_OK)
	return NS_ERROR;

    /*
     * Load CA certificates
     */

    caFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				CONFIG_SERVER_CAFILE, sdPtr->dir,
				DEFAULT_SERVER_CAFILE);

    caDir = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
			       CONFIG_SERVER_CADIR, sdPtr->dir,
			       DEFAULT_SERVER_CADIR);

    if (LoadCACerts (sdPtr->module, sdPtr->context, caFile, caDir) != NS_OK)
	return NS_ERROR;

    /*
     * Initialize the session cache
     */

    cacheEnabled = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				      CONFIG_SERVER_SESSIONCACHE,
				      DEFAULT_SERVER_SESSIONCACHE);

    cacheId = (int) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				      CONFIG_SERVER_SESSIONCACHEID,
				      DEFAULT_SERVER_SESSIONCACHEID);

    cacheTimeout = (long) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
					    CONFIG_SERVER_SESSIONTIMEOUT,
					    DEFAULT_SERVER_SESSIONTIMEOUT);

    cacheSize = ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				  CONFIG_SERVER_SESSIONCACHESIZE,
				  DEFAULT_SERVER_SESSIONCACHESIZE);

    if (InitSessionCache
	(sdPtr->module, sdPtr->context, cacheEnabled, cacheId, cacheTimeout,
	 cacheSize) != NS_OK)
	return NS_ERROR;

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MakeSockServerSSLContext --
 *
 *       Create a new SSL sock server context for the specified
 *       SSLDriver.
 *
 * Results:
 *       NS_OK or NS_ERROR
 *
 * Side effects:
 *       Sets sdPtr->sockServerContext.
 *
 *----------------------------------------------------------------------
 */

static int
MakeSockServerSSLContext (NsOpenSSLDriver * sdPtr)
{
    char *protocols;
    char *cipherSuite;
    char *certFile;
    char *keyFile;
    char *caFile;
    char *caDir;
    int connTrace;
    int peerVerify;
    int verifyDepth;
    int cacheEnabled;
    int cacheId;
    int cacheSize;
    int cacheTimeout;

    sdPtr->sockServerContext = SSL_CTX_new (SSLv23_server_method ());
    if (sdPtr->sockServerContext == NULL) {
	Ns_Log (Error, "%s: error creating SSL context", sdPtr->module);
	return NS_ERROR;
    }

    SSL_CTX_set_app_data (sdPtr->sockServerContext, sdPtr);

    /*
     * Enable SSL bug compatibility.
     */

    SSL_CTX_set_options (sdPtr->sockServerContext, SSL_OP_ALL);

    /*
     * This apparently prevents some sort of DH attack.
     */

    SSL_CTX_set_options (sdPtr->sockServerContext, SSL_OP_SINGLE_DH_USE);

    /*
     * Temporary key callback required for 40-bit export browsers
     */

    SSL_CTX_set_tmp_rsa_callback (sdPtr->sockServerContext, IssueTmpRSAKey);

    /*
     * Set peer verify and verify depth
     */

    peerVerify = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				    CONFIG_SOCKSERVER_PEERVERIFY,
				    DEFAULT_SOCKSERVER_PEERVERIFY);

    if (peerVerify) {
	SSL_CTX_set_verify (sdPtr->sockServerContext,
			    (SSL_VERIFY_PEER | SSL_VERIFY_CLIENT_ONCE),
			    PeerVerifyCallback);
	verifyDepth =
	    (int) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				    CONFIG_SOCKSERVER_VERIFYDEPTH,
				    DEFAULT_SOCKSERVER_VERIFYDEPTH);
	SSL_CTX_set_verify_depth (sdPtr->sockServerContext, verifyDepth);
    } else {
	SSL_CTX_set_verify (sdPtr->sockServerContext, SSL_VERIFY_NONE,
			    PeerVerifyCallback);
    }

    /*
     * Set SSL handshake and connection tracing
     */

    connTrace = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				   CONFIG_SOCKSERVER_TRACE,
				   DEFAULT_SOCKSERVER_TRACE);

    if (connTrace) {
	SSL_CTX_set_info_callback (sdPtr->sockServerContext, NsOpenSSLTrace);
    }

    /*
     * Set protocols
     */

    protocols = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
				     CONFIG_SOCKSERVER_PROTOCOLS,
				     DEFAULT_SOCKSERVER_PROTOCOLS);

    if (SetProtocols (sdPtr->module, sdPtr->sockServerContext, protocols) !=
	NS_OK) return NS_ERROR;

    /*
     * Set cipher suite
     */

    cipherSuite = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
				       CONFIG_SOCKSERVER_CIPHERSUITE,
				       DEFAULT_SOCKSERVER_CIPHERSUITE);

    if (SetCipherSuite (sdPtr->module, sdPtr->sockServerContext, cipherSuite)
	!= NS_OK)
	return NS_ERROR;

    /*
     * Load certificate
     */

    certFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				  CONFIG_SOCKSERVER_CERTFILE, sdPtr->dir,
				  DEFAULT_SOCKSERVER_CERTFILE);

    if (LoadCertificate (sdPtr->module, sdPtr->sockServerContext, certFile) !=
	NS_OK) return NS_ERROR;

    /*
     * Load the key that unlocks the certificate
     */

    keyFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				 CONFIG_SOCKSERVER_KEYFILE, sdPtr->dir,
				 DEFAULT_SOCKSERVER_KEYFILE);

    if (LoadKey (sdPtr->module, sdPtr->sockServerContext, keyFile) != NS_OK)
	return NS_ERROR;

    /*
     * Check the key against the certificate
     */

    if (CheckKey (sdPtr->module, sdPtr->sockServerContext) != NS_OK)
	return NS_ERROR;

    /*
     * Load CA certificates
     */

    caFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				CONFIG_SOCKSERVER_CAFILE, sdPtr->dir,
				DEFAULT_SOCKSERVER_CAFILE);

    caDir = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
			       CONFIG_SOCKSERVER_CADIR, sdPtr->dir,
			       DEFAULT_SOCKSERVER_CADIR);

    if (LoadCACerts (sdPtr->module, sdPtr->sockServerContext, caFile, caDir)
	!= NS_OK)
	return NS_ERROR;

    /*
     * Initialize the session cache
     */

    cacheEnabled = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				      CONFIG_SOCKSERVER_SESSIONCACHE,
				      DEFAULT_SOCKSERVER_SESSIONCACHE);

    cacheId = (int) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				      CONFIG_SOCKSERVER_SESSIONCACHEID,
				      DEFAULT_SOCKSERVER_SESSIONCACHEID);

    cacheTimeout = (long) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
					    CONFIG_SOCKSERVER_SESSIONTIMEOUT,
					    DEFAULT_SOCKSERVER_SESSIONTIMEOUT);

    cacheSize = ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				  CONFIG_SOCKSERVER_SESSIONCACHESIZE,
				  DEFAULT_SOCKSERVER_SESSIONCACHESIZE);

    if (InitSessionCache
	(sdPtr->module, sdPtr->sockServerContext, cacheEnabled, cacheId,
	 cacheTimeout, cacheSize) != NS_OK)
	return NS_ERROR;

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * MakeSockClientSSLContext --
 *
 *       Create a new SSL sock client context for the specified
 *       SSLDriver.
 *
 * Results:
 *       NS_OK or NS_ERROR
 *
 * Side effects:
 *       Sets sdPtr->sockServerContext.
 *
 *----------------------------------------------------------------------
 */

static int
MakeSockClientSSLContext (NsOpenSSLDriver * sdPtr)
{
    char *protocols;
    char *cipherSuite;
    char *certFile;
    char *keyFile;
    char *caFile;
    char *caDir;
    int connTrace;
    int peerVerify;
    int verifyDepth;
    int cacheEnabled;
    int cacheId;
    int cacheSize;
    int cacheTimeout;

    sdPtr->sockClientContext = SSL_CTX_new (SSLv23_client_method ());
    if (sdPtr->sockClientContext == NULL) {
	Ns_Log (Error, "%s: error creating SSL context", sdPtr->module);
	return NS_ERROR;
    }

    SSL_CTX_set_app_data (sdPtr->sockClientContext, sdPtr);

    /*
     * Enable SSL bug compatibility.
     */

    SSL_CTX_set_options (sdPtr->sockClientContext, SSL_OP_ALL);

    /*
     * This apparently prevents some sort of DH attack.
     */

    SSL_CTX_set_options (sdPtr->sockClientContext, SSL_OP_SINGLE_DH_USE);

    /*
     * Temporary key callback required for 40-bit export browsers
     */

    SSL_CTX_set_tmp_rsa_callback (sdPtr->sockClientContext, IssueTmpRSAKey);

    /*
     * Set peer verify and verify depth
     */

    peerVerify = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				    CONFIG_SOCKCLIENT_PEERVERIFY,
				    DEFAULT_SOCKCLIENT_PEERVERIFY);

    if (peerVerify) {
	SSL_CTX_set_verify (sdPtr->sockClientContext, SSL_VERIFY_PEER,
			    PeerVerifyCallback);
	verifyDepth =
	    (int) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				    CONFIG_SOCKCLIENT_VERIFYDEPTH,
				    DEFAULT_SOCKCLIENT_VERIFYDEPTH);
	SSL_CTX_set_verify_depth (sdPtr->sockClientContext, verifyDepth);
    } else {
	SSL_CTX_set_verify (sdPtr->sockClientContext, SSL_VERIFY_NONE,
			    PeerVerifyCallback);
    }

    /*
     * Set SSL handshake and connection tracing
     */

    connTrace = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				   CONFIG_SOCKCLIENT_TRACE,
				   DEFAULT_SOCKCLIENT_TRACE);

    if (connTrace) {
	SSL_CTX_set_info_callback (sdPtr->sockClientContext, NsOpenSSLTrace);
    }

    /*
     * Set protocols
     */

    protocols = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
				     CONFIG_SOCKCLIENT_PROTOCOLS,
				     DEFAULT_SOCKCLIENT_PROTOCOLS);

    if (SetProtocols (sdPtr->module, sdPtr->sockClientContext, protocols) !=
	NS_OK) return NS_ERROR;

    /*
     * Set cipher suite
     */

    cipherSuite = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
				       CONFIG_SOCKCLIENT_CIPHERSUITE,
				       DEFAULT_SOCKCLIENT_CIPHERSUITE);

    if (SetCipherSuite (sdPtr->module, sdPtr->sockClientContext, cipherSuite)
	!= NS_OK)
	return NS_ERROR;

    /*
     * Load certificate
     */

    certFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				  CONFIG_SOCKCLIENT_CERTFILE, sdPtr->dir,
				  DEFAULT_SOCKCLIENT_CERTFILE);

    if (certFile != NULL) {

	if (LoadCertificate
	    (sdPtr->module, sdPtr->sockClientContext, certFile) != NS_OK)
	    return NS_ERROR;

	/*
	 * Load the key that unlocks the certificate
	 */

	keyFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				     CONFIG_SOCKCLIENT_KEYFILE, sdPtr->dir,
				     DEFAULT_SOCKCLIENT_KEYFILE);

	if (LoadKey (sdPtr->module, sdPtr->sockClientContext, keyFile) !=
	    NS_OK) return NS_ERROR;

	/*
	 * Check the key against the certificate
	 */

	if (CheckKey (sdPtr->module, sdPtr->sockClientContext) != NS_OK)
	    return NS_ERROR;
    }

    /*
     * Load CA certificates
     */

    caFile = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
				CONFIG_SOCKCLIENT_CAFILE, sdPtr->dir,
				DEFAULT_SOCKCLIENT_CAFILE);

    caDir = ConfigPathDefault (sdPtr->module, sdPtr->configPath,
			       CONFIG_SOCKCLIENT_CADIR, sdPtr->dir,
			       DEFAULT_SOCKCLIENT_CADIR);

    if (LoadCACerts (sdPtr->module, sdPtr->sockClientContext, caFile, caDir)
	!= NS_OK)
	return NS_ERROR;

    /*
     * Initialize the session cache
     */

    cacheEnabled = ConfigBoolDefault (sdPtr->module, sdPtr->configPath,
				      CONFIG_SOCKCLIENT_SESSIONCACHE,
				      DEFAULT_SOCKCLIENT_SESSIONCACHE);

    cacheId = (int) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				      CONFIG_SOCKCLIENT_SESSIONCACHEID,
				      DEFAULT_SOCKCLIENT_SESSIONCACHEID);

    cacheTimeout = (long) ConfigIntDefault (sdPtr->module, sdPtr->configPath,
					    CONFIG_SOCKCLIENT_SESSIONTIMEOUT,
					    DEFAULT_SOCKCLIENT_SESSIONTIMEOUT);

    cacheSize = ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				  CONFIG_SOCKCLIENT_SESSIONCACHESIZE,
				  DEFAULT_SOCKCLIENT_SESSIONCACHESIZE);

    if (InitSessionCache
	(sdPtr->module, sdPtr->sockClientContext, cacheEnabled, cacheId,
	 cacheTimeout, cacheSize) != NS_OK)
	return NS_ERROR;

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SetCipherSuite --
 *
 *       Set the cipher suite to be used by the SSL server according
 *       to the config file.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

static int
SetCipherSuite (char *module, SSL_CTX * context, char *cipherSuite)
{
    int rc;

    rc = SSL_CTX_set_cipher_list (context, cipherSuite);

    if (rc == 0) {
	Ns_Log (Error, "%s: error configuring cipher suite to \"%s\"",
		module, cipherSuite);
	return NS_ERROR;
    }

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SetProtocols --
 *
 *       Set the list of protocols that the server will
 *       use.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       Sets pdPtr->protocols.
 *
 *----------------------------------------------------------------------
 */

static int
SetProtocols (char *module, SSL_CTX * context, char *protocols)
{
    int bits;

    protocols = ns_strdup (protocols);
    protocols = Ns_StrToLower (protocols);

    bits = SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_TLSv1;

    if (strstr (protocols, "all") != NULL) {
	bits = 1;
	Ns_Log (Notice, "%s: using all protocols: SSLv2, SSLv3 and TLSv1",
		module);
    } else {
	if (strstr (protocols, "sslv2") != NULL) {
	    bits &= ~SSL_OP_NO_SSLv2;
	    Ns_Log (Notice, "%s: Using SSLv2 protocol", module);
	}
	if (strstr (protocols, "sslv3") != NULL) {
	    bits &= ~SSL_OP_NO_SSLv3;
	    Ns_Log (Notice, "%s: Using SSLv3 protocol", module);
	}
	if (strstr (protocols, "tlsv1") != NULL) {
	    bits &= ~SSL_OP_NO_TLSv1;
	    Ns_Log (Notice, "%s: Using TLSv1 protocol", module);
	}
    }

    SSL_CTX_set_options (context, bits);

    ns_free (protocols);

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * LoadCertificate --
 *
 *       Load the certificate for the SSL server and SSL sock server
 *       from the file specified in the server config. Also loads a
 *       certificate chain that follows the certificate in the same
 *       file. To use a cert chain, simply append the CA certs to the
 *       end of your certificate file and they'll be passed to the
 *       client at connection time. If no certs are appended, no cert
 *       chain will be passed to the client.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       Frees *file.
 *
 *----------------------------------------------------------------------
 */

static int
LoadCertificate (char *module, SSL_CTX * context, char *certFile)
{
    int rc;

    /*
     * This allows the server to pass the entire certificate
     * chain to the client. It can simply hold just the server's
     * certificate if there is no chain.
     */

    rc = SSL_CTX_use_certificate_chain_file (context, certFile);

    if (rc == 0) {
	Ns_Log (Error, "%s: error loading certificate file \"%s\"",
		module, certFile);
    }

    ns_free (certFile);

    return (rc == 0) ? NS_ERROR : NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * LoadKey --
 *
 *       Load the private key for the SSL server and SSL sock server
 *       from the file specified in the server config.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

static int
LoadKey (char *module, SSL_CTX * context, char *keyFile)
{
    int rc;

    rc = SSL_CTX_use_PrivateKey_file (context, keyFile, SSL_FILETYPE_PEM);

    if (rc == 0) {
	Ns_Log (Error, "%s: error loading private key file \"%s\"",
		module, keyFile);
    }

    ns_free (keyFile);

    return (rc == 0) ? NS_ERROR : NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CheckKey --
 *
 *       Make sure that the private key for the SSL server and SSL sock server
 *       matches the certificate.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

static int
CheckKey (char *module, SSL_CTX * context)
{
    if (SSL_CTX_check_private_key (context) == 0) {
	Ns_Log (Error, "%s: private key does not match certificate", module);
	return NS_ERROR;
    }

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * LoadCACerts --
 *
 *       Load the CA certificates for the SSL server from the file
 *       specified in the server config.  Not an error if there
 *       are no CA certificates.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

static int
LoadCACerts (char *module, SSL_CTX * context, char *caFile, char *caDir)
{
    int status;
    int rc;
    int fd;
    DIR *dd;

    status = NS_OK;

    /*
     * Load CAs from a file
     */

    fd = open (caFile, O_RDONLY);
    if (fd < 0) {
	if (errno == ENOENT) {
	    Ns_Log (Notice, "%s: CA certificate file does not exist", module);
	} else {
	    Ns_Log (Error, "%s: error opening CA certificate file", module);
	    status = NS_ERROR;
	}
	ns_free (caFile);
	caFile = NULL;
    }

    else {
	close (fd);
    }

    /*
     * Load CAs from directory
     */

    dd = opendir (caDir);
    if (dd == NULL) {
	if (errno == ENOENT) {
	    Ns_Log (Notice, "%s: CA certificate directory does not exist",
		    module);
	} else {
	    Ns_Log (Error, "%s: error opening CA certificate directory",
		    module);
	    status = NS_ERROR;
	}

	ns_free (caDir);
	caDir = NULL;
    }

    else {
	closedir (dd);
    }

    if (status == NS_OK && (caFile != NULL || caDir != NULL)) {
	rc = SSL_CTX_load_verify_locations (context, caFile, caDir);

	if (rc == 0) {
	    Ns_Log (Error, "%s: error loading CA certificates", module);
	    status = NS_ERROR;
	}
    }

    if (caFile != NULL)
	ns_free (caFile);
    if (caDir != NULL)
	ns_free (caDir);

    return status;
}

/*
 *----------------------------------------------------------------------
 *
 * InitSessionCache --
 *
 *       Initialize the session cache for the SSL server as specified
 *       in the server config. This is an internal OpenSSL cache, so
 *       we don't do anything other than set a timeout and size.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

static int
InitSessionCache (char *module, SSL_CTX * context, int cacheEnabled,
		  int cacheId, int cacheTimeout, int cacheSize)
{
    if (cacheEnabled) {

	SSL_CTX_set_session_cache_mode (context, SSL_SESS_CACHE_SERVER);

	SSL_CTX_set_session_id_context (context,
					(void *) &cacheId, sizeof (cacheId));

	SSL_CTX_set_timeout (context, cacheTimeout);

	SSL_CTX_sess_set_cache_size (context, cacheSize);

    } else {

	SSL_CTX_set_session_cache_mode (context, SSL_SESS_CACHE_OFF);
    }

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * InitLocation --
 *
 *       Set the location, hostname, advertised address, bind address,
 *       and port of the driver as specified in the server config.
 *
 * Results:
 *       NS_ERROR or NS_OK
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

static int
InitLocation (NsOpenSSLDriver * sdPtr)
{
    char *hostname;
    char *lookupHostname;
    Ns_DString ds;

    sdPtr->bindaddr = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
					   "ServerAddress", NULL);

    hostname = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
				    "ServerHostname", NULL);

    if (sdPtr->bindaddr == NULL) {
	lookupHostname = (hostname != NULL) ? hostname : Ns_InfoHostname ();
	Ns_DStringInit (&ds);
	if (Ns_GetAddrByHost (&ds, lookupHostname) == NS_ERROR) {
	    Ns_Log (Error, "%s: failed to resolve '%s': %s",
		    sdPtr->module, lookupHostname, strerror (errno));
	    return NS_ERROR;
	}

	sdPtr->address = Ns_DStringExport (&ds);
    } else {
	sdPtr->address = ns_strdup (sdPtr->bindaddr);
    }

    if (hostname == NULL) {
	Ns_DStringInit (&ds);
	if (Ns_GetHostByAddr (&ds, sdPtr->address) == NS_ERROR) {
	    Ns_Log (Warning, "%s: failed to reverse resolve '%s': %s",
		    sdPtr->module, sdPtr->address, strerror (errno));
	    hostname = ns_strdup (sdPtr->address);
	} else {
	    hostname = Ns_DStringExport (&ds);
	}
    }

    sdPtr->port = ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				    "ServerPort", DEFAULT_PORT);

    sdPtr->location = ConfigStringDefault (sdPtr->module, sdPtr->configPath,
					   "ServerLocation", NULL);
    if (sdPtr->location != NULL) {
	sdPtr->location = ns_strdup (sdPtr->location);
    } else {
	Ns_DStringInit (&ds);
	Ns_DStringVarAppend (&ds, DEFAULT_PROTOCOL "://", hostname, NULL);
	if (sdPtr->port != DEFAULT_PORT) {
	    Ns_DStringPrintf (&ds, ":%d", sdPtr->port);
	}
	sdPtr->location = Ns_DStringExport (&ds);
    }
    Ns_Log (Notice, "%s: location %s", sdPtr->module, sdPtr->location);

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * PeerVerifyCallback --
 *
 *      Called by the SSL library at each stage of client certificate
 *      verification.
 *
 * Results:
 *
 *      Always returns 1 to prevent verification errors from halting
 *      the SSL handshake.  We'd rather finish the handshake so we
 *      can either authenticate by other means or return an HTTP error.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
PeerVerifyCallback (int preverify_ok, X509_STORE_CTX * x509_ctx)
{
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * SeedPRNG --
 *
 *       Seed OpenSSL's PRNG. Note that OpenSSL will seed the PRNG
 *       transparently if /dev/urandom is available, which it is
 *       on Linux.
 *
 * Results:
 *       NS_TRUE or NS_FALSE.
 *
 * Side effects:
 *       An NS_FALSE will result in the connection failing. This function
 *       might be called at any time by the temporary key generating
 *       function if the PRNG is not sufficiently entropinous (yes, I
 *       made that word up).
 *       
 *
 *----------------------------------------------------------------------
 */

static int
SeedPRNG (NsOpenSSLDriver * sdPtr)
{
    int i;
    double *buf_ptr = NULL;
    double *bufoffset_ptr = NULL;
    size_t size;
    int seedbytes;

    if (PRNGIsSeeded (sdPtr)) {
	Ns_Log (Debug, "%s: PRNG already has enough entropy", sdPtr->module);
	return NS_TRUE;
    }

    Ns_Log (Notice, "%s: Seeding the PRNG", sdPtr->module);

    seedbytes = ConfigIntDefault (sdPtr->module, sdPtr->configPath,
				  CONFIG_SEEDBYTES, DEFAULT_SEEDBYTES);

    /*
     * Try to use the file specified by the user.
     */

    AddEntropyFromRandomFile (sdPtr, seedbytes);

    if (PRNGIsSeeded (sdPtr)) {
	return NS_TRUE;
    }

    /*
     * Use Ns API; I have no idea how to measure the amount of
     * entropy, so for now I just pass the same number as the 2nd arg
     * to RAND_add Also know that not all of the buffer is used
     */

    size = sizeof (double) * seedbytes;
    buf_ptr = Ns_Malloc (size);
    bufoffset_ptr = buf_ptr;
    for (i = 0; i < seedbytes; i++) {
	*bufoffset_ptr = Ns_DRand ();
	bufoffset_ptr++;
    }
    RAND_add (buf_ptr, seedbytes, (long) seedbytes);
    Ns_Free (buf_ptr);
    Ns_Log (Notice, "%s: Seeded PRNG with %d bytes from Ns_DRand",
	    sdPtr->module, seedbytes);

    if (PRNGIsSeeded (sdPtr)) {
	return NS_TRUE;
    }

    Ns_Log (Warning, "%s: Failed to seed PRNG with enough entropy",
	    sdPtr->module);
    return NS_FALSE;
}

/*
 *----------------------------------------------------------------------
 *
 * PRNGIsSeeded --
 *
 *       See if the PRNG contains enough entropy.
 *
 * Results:
 *       NS_TRUE or NS_FALSE
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

static int
PRNGIsSeeded (NsOpenSSLDriver * sdPtr)
{
    if (RAND_status ()) {
	Ns_Log (Debug,
		"%s: RAND_status reports sufficient entropy for the PRNG",
		sdPtr->module);
	return NS_TRUE;
    }

    /* Assume we don't have enough */
    return NS_FALSE;
}

/*
 *----------------------------------------------------------------------
 *
 * IssueTmpRSAKey --
 *
 *       Give out the temporary key when needed. This is a callback
 *       function used by OpenSSL.
 *
 * Results:
 *       Returns a pointer to the new temporary key.
 *
 * Side effects:
 *       Attempts to Seed the PRNG if needed. If PRNG doesn't contain 
 *       enough entropy, key won't be returned and the connection
 *       will fail.
 *
 *----------------------------------------------------------------------
 */

static RSA *
IssueTmpRSAKey (SSL * ssl, int export, int keylen)
{
    Ns_OpenSSLConn *scPtr;
    NsOpenSSLDriver *sdPtr;
    static RSA *rsa_tmp = NULL;

    scPtr = (Ns_OpenSSLConn *) SSL_get_app_data (ssl);
    sdPtr = scPtr->sdPtr;

    if (SeedPRNG (sdPtr)) {
	rsa_tmp = RSA_generate_key (keylen, RSA_F4, NULL, NULL);
	Ns_Log (Notice, "%s: Generated %d-bit temporary RSA key",
		sdPtr->module, keylen);
	return rsa_tmp;
    } else {
	Ns_Log (Warning,
		"%s: Cannot generate temporary RSA key due to insufficient entropy in PRNG",
		sdPtr->module);
	return NULL;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * AddEntropyFromRandomFile --
 *
 *       Grabs a number of bytes from a file to seed the OpenSSL
 *       PRNG.
 *
 * Results:
 *       None.
 *
 * Side effects:
 *       Directly seeds OpenSSL's PRNG by calling RAND_load_file.
 *
 *----------------------------------------------------------------------
 */

static int
AddEntropyFromRandomFile (NsOpenSSLDriver * sdPtr, long maxbytes)
{
    int readbytes;

    if (sdPtr->randomFile == NULL) {
	return NS_FALSE;
    }

    if (access (sdPtr->randomFile, F_OK) == 0) {
	if ((readbytes = RAND_load_file (sdPtr->randomFile, maxbytes))) {
	    Ns_Log (Debug, "%s: Obtained %d random bytes from %s",
		    sdPtr->module, readbytes, sdPtr->randomFile);
	    return NS_TRUE;
	} else {
	    Ns_Log (Warning, "%s: Unable to retrieve any random data from %s",
		    sdPtr->module, sdPtr->randomFile);
	    return NS_FALSE;
	}
    }
    return NS_FALSE;
}

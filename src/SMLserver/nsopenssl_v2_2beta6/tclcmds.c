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

#include "nsopenssl.h"
#include "tclcmds.h"

#ifdef WIN32
#define SockError(i)    NsWin32ErrMsg(GetLastError())
#else
#include <sys/ioctl.h>
#define SockError(i)    Tcl_PosixError((i))
#endif

#ifdef __sun
#include <sys/filio.h>
#endif

/*
 * The following structure is used to maintain the state for a socket callback.
 */

typedef struct Callback {
    int when;
    char script[1];
} Callback;

/*
 * Local Functions
 */

static void SetResultToX509Name (Tcl_Interp * interp, X509_NAME * name);
static void SetResultToObjectName (Tcl_Interp * interp, ASN1_OBJECT * obj);
static char *ValidTime (ASN1_UTCTIME * tm);
static char *PEMCertificate (X509 * peercert);
static Ns_OpenSSLConn *NsOpenSSLGetConn (Tcl_Interp * interp);

static int CreateTclChannel (Ns_OpenSSLConn * ccPtr, Tcl_Interp * interp);
static int ChanCloseProc (ClientData instanceData, Tcl_Interp * interp);
static int ChanInputProc (ClientData instanceData, char *buf, int bufSize,
			  int *errorCodePtr);
static int ChanOutputProc (ClientData instanceData, char *buf, int toWrite,
			   int *errorCodePtr);
static void ChanWatchProc (ClientData instanceData, int mask);
static int ChanFlushProc (ClientData instanceData);
static int ChanGetHandleProc (ClientData instanceData, int direction,
			      ClientData * handlePtr);

#if 0				/* these Tcl channel procs are not implemented at this time */
static int ChanSetOptionProc (ClientData instanceData, Tcl_Interp * interp,
			      char *optionName, char *value);
static int ChanGetOptionProc (ClientData instanceData, Tcl_Interp * interp,
			      char *optionName, Tcl_DString * dsPtr);
#endif

static int EnterSock (Tcl_Interp * interp, SOCKET sock);
static int EnterDup (Tcl_Interp * interp, SOCKET sock);
static int GetSet (Tcl_Interp * interp, char *flist, int write,
		   fd_set ** ppset, fd_set * pset, SOCKET * maxPtr);
static void AppendReadyFiles (Tcl_Interp * interp, fd_set * pset, int write,
			      char *flist, Tcl_DString * pds);
static int SSLSockSetBlocking (char *value, Tcl_Interp * interp, int argc,
			       char **argv);

static Ns_SockProc NsTclSSLSockProc;
static Ns_SockProc SSLSockListenCallback;
static int NsTclEval (Tcl_Interp * interp, char *script);

/* We define our own Tcl channel so that we can use Tcl gets, puts and friends */
static Tcl_ChannelType opensslChannelType = {
    "openssl",			/* Type name. */
    TCL_CHANNEL_VERSION_2,	/* channel version 2 */
    ChanCloseProc,		/* Close proc. */
    ChanInputProc,		/* Input proc. */
    ChanOutputProc,		/* Output proc. */
    NULL,			/* Seek proc. */
    NULL,			/* Set option proc. */
    NULL,			/* Get option proc. */
    ChanWatchProc,		/* Watch proc. (mandatory) */
    ChanGetHandleProc,		/* Get Handle */
    NULL,			/* Close2 proc */
    NULL,			/* Set blocking/nonblocking mode. */
    ChanFlushProc,		/* Flush proc */
    NULL,			/* Handler proc */
};

static SSLTclCmd nsopensslCmds[] = {
    {"ns_openssl", NsTclOpenSSLCmd, NULL},
    {"ns_openssl_sockopen", NsTclSSLSockOpenCmd, NULL},
    {"ns_openssl_geturl", NsTclSSLGetUrlCmd, NULL},
    {"ns_openssl_sockaccept", NsTclSSLSockAcceptCmd, NULL},
    {"ns_openssl_socklisten", NsTclSSLSockListenCmd, NULL},
    {"ns_openssl_socknread", NsTclSSLSockNReadCmd, NULL},
    {"ns_openssl_sockselect", NsTclSSLSockSelectCmd, NULL},
    {"ns_openssl_sockcheck", NsTclSSLSockCheckCmd, NULL},
    {"ns_openssl_sockblocking", NsTclSSLSockSetBlockingCmd, NULL},
    {"ns_openssl_socknonblocking", NsTclSSLSockSetNonBlockingCmd, NULL},
    {"ns_openssl_sockcallback", NsTclSSLSockCallbackCmd, NULL},
    {"ns_openssl_socklistencallback", NsTclSSLSockListenCallbackCmd, NULL},

#if 0				/* these ns_openssl_sock* commands are not implemented yet */
    {"ns_openssl_socketpair", NsTclSSLSocketPairCmd, NULL},
    {"ns_openssl_hostbyaddr", NsTclSSLGetByCmd, NULL},
    {"ns_openssl_addrbyhost", NsTclSSLGetByCmd, (ClientData) 1},
#endif
    {NULL, NULL, NULL}
};

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLCreateCmds --
 *
 *      Add nsopenssl commands to Tcl interpreter.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern int
NsOpenSSLCreateCmds (Tcl_Interp * interp, void *arg)
{
    SSLTclCmd *cmds = (SSLTclCmd *) & nsopensslCmds;

    while (cmds->name != NULL) {
	if (Tcl_CreateCommand (interp,
			       cmds->name,
			       cmds->proc, cmds->clientData, NULL) == NULL) {
	    return NS_ERROR;
	}
	++cmds;
    }

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclOpenSSLCmd --
 *
 *      Returns information about clients connected to the nsopenssl
 *      server, including client certificates.
 *
 * Results:
 *      Tcl string result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclOpenSSLCmd (ClientData dummy, Tcl_Interp * interp, int argc, char **argv)
{
    Ns_OpenSSLConn *scPtr;
    X509 *peercert;
    SSL_CIPHER *cipher;
    char *string;
    int integer;
    int status = TCL_OK;

    if (argc < 2) {
	Tcl_AppendResult (interp, "wrong # args:  should be \"",
			  argv[0], " command \"", NULL);
	return TCL_ERROR;
    }

    /* ns_openssl info doesn't require a conn to run */

    if (STREQ (argv[1], "info")) {
	Tcl_AppendElement (interp, SSL_LIBRARY_NAME);
	Tcl_AppendElement (interp, SSL_LIBRARY_VERSION);
	Tcl_AppendElement (interp, SSL_CRYPTO_LIBRARY_NAME);
	Tcl_AppendElement (interp, SSL_CRYPTO_LIBRARY_VERSION);
	return TCL_OK;
    }

    scPtr = NsOpenSSLGetConn (interp);

    if (scPtr == NULL) {
	Tcl_AppendResult (interp, "no SSL connection", NULL);
	return TCL_ERROR;
    }

    /* The following variants of the cmd require an active SSL connection */

    if (STREQ (argv[1], "module")) {

	if (argc != 3) {

	    Tcl_AppendResult (interp, "wrong # args:  should be \"", argv[0],
			      argv[1], " name\"", NULL);
	    status = TCL_ERROR;

	} else if (STREQ (argv[2], "name")) {

	    Tcl_SetResult (interp, scPtr->module, TCL_VOLATILE);

	} else if (STREQ (argv[2], "port")) {

	    sprintf (interp->result, "%d", scPtr->port);

	}

    } else if (STREQ (argv[1], "protocol")) {

	switch (scPtr->ssl->session->ssl_version) {
	case SSL2_VERSION:
	    string = "SSLv2";
	    break;
	case SSL3_VERSION:
	    string = "SSLv3";
	    break;
	case TLS1_VERSION:
	    string = "TLSv1";
	    break;
	default:
	    string = "UNKNOWN";
	}

	Tcl_SetResult (interp, string, TCL_VOLATILE);

    } else if ((STREQ (argv[1], "port")) || (STREQ (argv[1], "peerport"))) {

	sprintf (interp->result, "%d", scPtr->peerport);

    } else if (STREQ (argv[1], "cipher")) {

	cipher = SSL_get_current_cipher (scPtr->ssl);

	if (STREQ (argv[2], "name")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " name\"", NULL);
		status = TCL_ERROR;

	    } else {
		string =
		    (scPtr->ssl !=
		     NULL ? (char *) SSL_CIPHER_get_name (cipher) : NULL);
		Tcl_SetResult (interp, string, TCL_VOLATILE);
	    }

	} else if (STREQ (argv[2], "strength")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " strength\"", NULL);
		status = TCL_ERROR;
	    } else {
		integer = SSL_CIPHER_get_bits (cipher, &integer);
		sprintf (interp->result, "%d", integer);
	    }
	}

    } else if (STREQ (argv[1], "clientcert")) {

	peercert = (scPtr == NULL) ? NULL : scPtr->peercert;

	if (STREQ (argv[2], "exists")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " exists\"", NULL);
		status = TCL_ERROR;

	    } else {
		Tcl_SetResult (interp, peercert == NULL ? "0" : "1",
			       TCL_STATIC);
	    }

	} else if (STREQ (argv[2], "version")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " version\"", NULL);
		status = TCL_ERROR;

	    } else {
		sprintf (interp->result, "%lu",
			 peercert == NULL
			 ? 0 : X509_get_version (peercert) + 1);
	    }

	} else if (STREQ (argv[2], "serial")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " serial\"", NULL);
		status = TCL_ERROR;

	    } else {
		sprintf (interp->result, "%ld",
			 peercert == NULL
			 ? 0
			 :
			 ASN1_INTEGER_get (X509_get_serialNumber (peercert)));
	    }

	} else if (STREQ (argv[2], "subject")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " subject\"", NULL);
		status = TCL_ERROR;

	    } else if (peercert != NULL) {
		SetResultToX509Name (interp,
				     X509_get_subject_name (peercert));
	    }

	} else if (STREQ (argv[2], "issuer")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " issuer\"", NULL);
		status = TCL_ERROR;

	    } else if (peercert != NULL) {
		SetResultToX509Name (interp, X509_get_issuer_name (peercert));
	    }

	} else if (STREQ (argv[2], "notbefore")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " notbefore\"", NULL);
		status = TCL_ERROR;

	    } else if (peercert != NULL) {
		string = ValidTime (X509_get_notBefore (peercert));
		if (string == NULL) {
		    Tcl_SetResult (interp, "error getting notbefore",
				   TCL_STATIC);
		    status = TCL_ERROR;
		} else {
		    Tcl_SetResult (interp, string, TCL_DYNAMIC);
		}
	    }

	} else if (STREQ (argv[2], "notafter")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " notafter\"", NULL);
		status = TCL_ERROR;
	    } else if (peercert != NULL) {
		string = ValidTime (X509_get_notAfter (peercert));
		if (string == NULL) {
		    Tcl_SetResult (interp, "error getting notafter",
				   TCL_STATIC);
		    status = TCL_ERROR;
		} else {
		    Tcl_SetResult (interp, string, TCL_DYNAMIC);
		}
	    }

	} else if (STREQ (argv[2], "signature_algorithm")) {
	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " signature_algorithm\"",
				  NULL);
		status = TCL_ERROR;

	    } else if (peercert != NULL) {
		SetResultToObjectName (interp,
				       peercert->cert_info->signature->
				       algorithm);
	    }

	} else if (STREQ (argv[2], "key_algorithm")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " key_algorithm\"", NULL);
		status = TCL_ERROR;

	    } else if (peercert != NULL) {
		SetResultToObjectName (interp,
				       peercert->cert_info->key->algor->
				       algorithm);
	    }

	} else if (STREQ (argv[2], "pem")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " pem\"", NULL);
		status = TCL_ERROR;

	    } else if (peercert != NULL) {
		string = PEMCertificate (peercert);
		if (string == NULL) {
		    Tcl_SetResult (interp, "error getting pem", TCL_STATIC);
		    status = TCL_ERROR;
		} else {
		    Tcl_SetResult (interp, string, TCL_DYNAMIC);
		}
	    }

	} else if (STREQ (argv[2], "valid")) {

	    if (argc != 3) {
		Tcl_AppendResult (interp, "wrong # args:  should be \"",
				  argv[0], argv[1], " valid\"", NULL);
		status = TCL_ERROR;

	    } else {
		sprintf (interp->result, "%d",
			 peercert != NULL
			 && SSL_get_verify_result (scPtr->ssl) == X509_V_OK);
	    }

	} else {
	    Tcl_AppendResult (interp, "unknown command \"", argv[2],
			      "\": should be one of: exists version serial subject issuer notbefore notafter signature_algorithm key_algorithm pem valid",
			      NULL);
	    status = TCL_ERROR;
	}

    } else {
	Tcl_AppendResult (interp, "unknown command \"", argv[1],
			  "\": should be one of: info clientcert", NULL);
	status = TCL_ERROR;
    }

    return status;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockOpenCmd --
 *
 *	Open a tcp connection to a host/port via SSL. 
 *
 * Results:
 *	Tcl result. 
 *
 * Side effects:
 *	Will open a connection and register two Tcl channels.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockOpenCmd (ClientData dummy, Tcl_Interp * interp, int argc,
		     char **argv)
{
    Ns_OpenSSLConn *ccPtr = NULL;
    int port;
    int timeout;
    int first;
    int async;

    if (argc < 3 || argc > 5) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0],
			  " ?-nonblock|-timeout seconds? host port\"", NULL);
	return TCL_ERROR;
    }
    first = 1;
    async = 0;
    timeout = -1;
    if (argc == 4) {

	/*
	 * ns_sockopen -nonblock host port
	 */

	if (!STREQ (argv[1], "-nonblock") && !STREQ (argv[1], "-async")) {
	    Tcl_AppendResult (interp, "wrong # args: should be \"",
			      argv[0],
			      " ?-nonblock|-timeout seconds? host port\"",
			      NULL);
	    return TCL_ERROR;
	}

	first = 2;
	async = 1;
    } else if (argc == 5) {

	/*
	 * ns_sockopen -timeout seconds host port
	 */

	if (!STREQ (argv[1], "-timeout")) {
	    Tcl_AppendResult (interp, "wrong # args: should be \"",
			      argv[0],
			      " ?-nonblock|-timeout seconds? host port\"",
			      NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetInt (interp, argv[2], &timeout) != TCL_OK) {
	    return TCL_ERROR;
	}
	first = 3;
    }
    if (Tcl_GetInt (interp, argv[first + 1], &port) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * Perform the connection.
     */

    ccPtr = Ns_OpenSSLSockConnect (argv[first], port, async, timeout);

    if (ccPtr == NULL) {
	Tcl_AppendResult (interp, "could not connect to \"",
			  argv[first], ":", argv[first + 1], "\"", NULL);
	return TCL_ERROR;
    }

    if (CreateTclChannel (ccPtr, interp) != NS_OK) {
	Ns_Log (Warning, "%s: %s: Tcl channel not available", ccPtr->module,
		ccPtr->type);
    }

    /*
     * Append "1" as the third element returned if peer certificate
     * is found to be valid; "0" otherwise. Is this the best way to do
     * it? 
     */

    if (Ns_OpenSSLIsPeerCertValid (ccPtr)) {
	Tcl_AppendElement (interp, "1");
    } else {
	Tcl_AppendElement (interp, "0");
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockListenCmd --
 *
 *      Listen on a TCP port.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      Will listen on a port.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockListenCmd (ClientData dummy, Tcl_Interp * interp, int argc,
		       char **argv)
{
    SOCKET sock;
    char *addr;
    int port;

    if (argc != 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " address port\"", NULL);
	return TCL_ERROR;
    }
    addr = argv[1];
    if (STREQ (addr, "*")) {
	addr = NULL;
    }
    if (Tcl_GetInt (interp, argv[2], &port) != TCL_OK) {
	return TCL_ERROR;
    }
    sock = Ns_OpenSSLSockListen (addr, port);
    if (sock == INVALID_SOCKET) {
	Tcl_AppendResult (interp, "could not listen on \"",
			  argv[1], ":", argv[2], "\"", NULL);
	return TCL_ERROR;
    }
    return EnterSock (interp, sock);
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockAcceptCmd --
 *
 *      Accept a connection from a listening socket.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockAcceptCmd (ClientData dummy, Tcl_Interp * interp, int argc,
		       char **argv)
{
    Ns_OpenSSLConn *ccPtr;
    SOCKET sock;

    if (argc != 2) {
	Tcl_AppendResult (interp, "wrong # of args: should be \"",
			  argv[0], " sockId\"", NULL);
	return TCL_ERROR;
    }
    if (Ns_TclGetOpenFd (interp, argv[1], 0, (int *) &sock) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Do normal accept on the socket */
    sock = Ns_SockAccept (sock, NULL, 0);

    if (sock == INVALID_SOCKET) {
	Tcl_AppendResult (interp, "accept failed: ",
			  SockError (interp), NULL);
	return TCL_ERROR;
    }

    ccPtr = Ns_OpenSSLSockAccept (sock);

    if (ccPtr == NULL) {
	Tcl_AppendResult (interp, "SSL accept failed \"", NULL);
	return TCL_ERROR;
    }

    if (CreateTclChannel (ccPtr, interp) != NS_OK) {
	Ns_Log (Warning, "%s: %s: Tcl channel not available", ccPtr->module,
		ccPtr->type);
    }

    /*
     * Append "1" as the third element returned if peer certificate
     * is found to be valid; "0" otherwise. Is this the best way to do
     * it? 
     */

    if (Ns_OpenSSLIsPeerCertValid (ccPtr)) {
	Tcl_AppendElement (interp, "1");
    } else {
	Tcl_AppendElement (interp, "0");
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLGetUrlCmd --
 *
 *      Implements ns_geturl.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      See docs.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLGetUrlCmd (ClientData dummy, Tcl_Interp * interp, int argc,
		   char **argv)
{
    Ns_DString ds;
    Ns_Set *headers;
    int status;

    if ((argc != 3) && (argc != 2)) {
	Tcl_AppendResult (interp, "wrong # of args:  should be \"",
			  argv[0], " url ?headersSetIdVar?", NULL);
	return TCL_ERROR;
    }
    if (argc == 2) {
	headers = NULL;
    } else {
	headers = Ns_SetCreate (NULL);
    }
    Ns_DStringInit (&ds);
    if (*argv[1] == '/') {
	if (Ns_OpenSSLFetchPage (&ds, argv[1], Ns_TclInterpServer (interp)) !=
	    NS_OK) {
	    Tcl_AppendResult (interp, "Could not get contents of URL \"",
			      argv[1], "\"", NULL);
	    status = TCL_ERROR;
	    goto done;
	}
    } else {
	if (Ns_OpenSSLFetchURL (&ds, argv[1], headers) != NS_OK) {
	    Tcl_AppendResult (interp, "Could not get contents of URL \"",
			      argv[1], "\"", NULL);
	    if (headers != NULL) {
		Ns_SetFree (headers);
	    }
	    status = TCL_ERROR;
	    goto done;
	}
    }
    if (argc == 3) {
	Ns_TclEnterSet (interp, headers, 1);
	Tcl_SetVar (interp, argv[2], interp->result, 0);
    }
    Tcl_SetResult (interp, ds.string, TCL_VOLATILE);
    status = TCL_OK;

  done:
    Ns_DStringFree (&ds);

    return status;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockNReadCmd --
 *
 *      Gets the number of bytes that a socket has waiting to be
 *      read.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      None.
 *   
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockNReadCmd (ClientData dummy, Tcl_Interp * interp, int argc,
		      char **argv)
{
    int nread;
    Tcl_Channel chan;
    SOCKET sock;

    if (argc != 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " sockId\"", NULL);
	return TCL_ERROR;
    }
    chan = Tcl_GetChannel (interp, argv[1], NULL);
    if (chan == NULL || Ns_TclGetOpenFd (interp, argv[1], 0,
					 (int *) &sock) != TCL_OK) {
	return TCL_ERROR;
    }
    if (ns_sockioctl (sock, FIONREAD, &nread) != 0) {
	Tcl_AppendResult (interp, "ns_sockioctl failed: ",
			  SockError (interp), NULL);
	return TCL_ERROR;
    }
    nread += Tcl_InputBuffered (chan);
    sprintf (interp->result, "%d", nread);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockCheckCmd --
 *
 *      Check if a socket is still connected, useful for nonblocking.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockCheckCmd (ClientData dummy, Tcl_Interp * interp, int argc,
		      char **argv)
{
    SOCKET sock;

    if (argc != 2) {
	Tcl_AppendResult (interp, "wrong # of args: should be \"",
			  argv[0], " sockId\"", NULL);
	return TCL_ERROR;
    }
    if (Ns_TclGetOpenFd (interp, argv[1], 1, (int *) &sock) != TCL_OK) {
	return TCL_ERROR;
    }
    Ns_Log (Debug, "#### SOCKET sock = %d", sock);
    if (send (sock, NULL, 0, 0) != 0) {
	interp->result = "0";
    } else {
	interp->result = "1";
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSelectCmd --
 *
 *      Imlements ns_sockselect: basically a tcl version of
 *      select(2).
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      See docs.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockSelectCmd (ClientData dummy, Tcl_Interp * interp, int argc,
		       char **argv)
{
    fd_set rset, wset, eset, *rPtr, *wPtr, *ePtr;
    SOCKET maxfd;
    int i, status, arg;
    Tcl_Channel chan;
    struct timeval tv, *tvPtr;
    Tcl_DString dsRfd, dsNbuf;
    char **fargv;
    int fargc;

    status = TCL_ERROR;
    if (argc != 6 && argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " ?-timeout sec? rfds wfds efds\"", NULL);
	return TCL_ERROR;
    }
    if (argc == 4) {
	tvPtr = NULL;
	arg = 1;
    } else {
	tvPtr = &tv;
	if (strcmp (argv[1], "-timeout") != 0) {
	    Tcl_AppendResult (interp, "wrong # args: should be \"",
			      argv[0], " ?-timeout sec? rfds wfds efds\"",
			      NULL);
	    return TCL_ERROR;
	}
	tv.tv_usec = 0;
	if (Tcl_GetInt (interp, argv[2], &i) != TCL_OK) {
	    return TCL_ERROR;
	}
	tv.tv_sec = i;
	arg = 3;
    }

    /*
     * Readable fd's are treated differently because they may
     * have buffered input. Before doing a select, see if they
     * have any waiting data that's been buffered by the channel.
     */

    if (Tcl_SplitList (interp, argv[arg++], &fargc, &fargv) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_DStringInit (&dsRfd);
    Tcl_DStringInit (&dsNbuf);
    for (i = 0; i < fargc; ++i) {
	chan = Tcl_GetChannel (interp, fargv[i], NULL);
	if (chan == NULL) {
	    goto done;
	}
	if (Tcl_InputBuffered (chan) > 0) {
	    Tcl_DStringAppendElement (&dsNbuf, fargv[i]);
	} else {
	    Tcl_DStringAppendElement (&dsRfd, fargv[i]);
	}
    }

    if (dsNbuf.length > 0) {
	/*
	 * Since at least one read fd had buffered input,
	 * turn the select into a polling select just
	 * to pick up anything else ready right now.
	 */

	tv.tv_sec = 0;
	tv.tv_usec = 0;
	tvPtr = &tv;
    }
    maxfd = 0;
    if (GetSet (interp, dsRfd.string, 0, &rPtr, &rset, &maxfd) != TCL_OK) {
	goto done;
    }
    if (GetSet (interp, argv[arg++], 1, &wPtr, &wset, &maxfd) != TCL_OK) {
	goto done;
    }
    if (GetSet (interp, argv[arg++], 0, &ePtr, &eset, &maxfd) != TCL_OK) {
	goto done;
    }

    /*
     * Return immediately if we're not doing a select on anything.
     */

    if (dsNbuf.length == 0 &&
	rPtr == NULL && wPtr == NULL && ePtr == NULL && tvPtr == NULL) {

	status = TCL_OK;
    } else {

	/*
	 * Actually perform the select.
	 */

	do {
	    i = select (maxfd + 1, rPtr, wPtr, ePtr, tvPtr);
	} while (i < 0 && ns_sockerrno == EINTR);

	if (i == -1) {
	    Tcl_AppendResult (interp, "select failed: ",
			      SockError (interp), NULL);
	} else {
	    if (i == 0) {
		/*
		 * The sets can have any random value now
		 */

		if (rPtr != NULL) {
		    FD_ZERO (rPtr);
		}
		if (wPtr != NULL) {
		    FD_ZERO (wPtr);
		}
		if (ePtr != NULL) {
		    FD_ZERO (ePtr);
		}
	    }
	    AppendReadyFiles (interp, rPtr, 0, dsRfd.string, &dsNbuf);
	    arg -= 2;
	    AppendReadyFiles (interp, wPtr, 1, argv[arg++], NULL);
	    AppendReadyFiles (interp, ePtr, 0, argv[arg++], NULL);
	    status = TCL_OK;
	}
    }

  done:
    Tcl_DStringFree (&dsRfd);
    Tcl_DStringFree (&dsNbuf);
    ckfree ((char *) fargv);

    return status;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockCallbackCmd --
 *
 *      Register a Tcl callback to be run when a certain state exists
 *      on a socket.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      A callback will be registered.
 *
 *----------------------------------------------------------------------
 */

void
NsTclSSLSockArgProc (Tcl_DString * dsPtr, void *arg)
{
    Callback *cbPtr = arg;

    Tcl_DStringAppendElement (dsPtr, cbPtr->script);
}

extern int
NsTclSSLSockCallbackCmd (ClientData dummy, Tcl_Interp * interp, int argc,
			 char **argv)
{
    SOCKET sock;
    int when;
    char *s;
    Callback *cbPtr;

    if (argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " sockId script when\"", NULL);
	return TCL_ERROR;
    }
    s = argv[3];
    when = 0;
    while (*s != '\0') {
	if (*s == 'r') {
	    when |= NS_SOCK_READ;
	} else if (*s == 'w') {
	    when |= NS_SOCK_WRITE;
	} else if (*s == 'e') {
	    when |= NS_SOCK_EXCEPTION;
	} else if (*s == 'x') {
	    when |= NS_SOCK_EXIT;
	} else {
	    Tcl_AppendResult (interp, "invalid when specification \"",
			      argv[3],
			      "\": should be one or more of r, w, e, or x",
			      NULL);
	    return TCL_ERROR;
	}
	++s;
    }
    if (when == 0) {
	Tcl_AppendResult (interp, "invalid when specification \"", argv[3],
			  "\": should be one or more of r, w, e, or x", NULL);
	return TCL_ERROR;
    }
    if (Ns_TclGetOpenFd (interp, argv[1], (when & NS_SOCK_WRITE),
			 (int *) &sock) != TCL_OK) {
	return TCL_ERROR;
    }
    sock = ns_sockdup (sock);
    if (sock == INVALID_SOCKET) {
	Tcl_AppendResult (interp, "dup failed: ", SockError (interp), NULL);
	return TCL_ERROR;
    }
    cbPtr = ns_malloc (sizeof (Callback) + strlen (argv[2]));
    cbPtr->when = when;
    strcpy (cbPtr->script, argv[2]);
    if (Ns_OpenSSLSockCallback (sock, NsTclSSLSockProc, cbPtr,
				when | NS_SOCK_EXIT) != NS_OK) {
	interp->result = "could not register callback";
	ns_sockclose (sock);
	ns_free (cbPtr);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockListenCallbackCmd --
 *
 *      Listen on a socket and register a callback to run when
 *      connections arrive.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      Will register a callback and listen on a socket.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockListenCallbackCmd (ClientData dummy, Tcl_Interp * interp,
			       int argc, char **argv)
{
    int port;
    char *addr, *script;

    if (argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " address port script\"", NULL);
	return TCL_ERROR;
    }
    if (Tcl_GetInt (interp, argv[2], &port) != TCL_OK) {
	return TCL_ERROR;
    }
    addr = argv[1];
    if (STREQ (addr, "*")) {
	addr = NULL;
    }
    script = ns_strdup (argv[3]);
    if (Ns_OpenSSLSockListenCallback
	(addr, port, SSLSockListenCallback, script) != NS_OK) {
	interp->result = "could not register callback";
	ns_free (script);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockSetBlockingCmd --
 *
 *      Sets a socket blocking.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockSetBlockingCmd (ClientData dummy, Tcl_Interp * interp, int argc,
			    char **argv)
{
    return SSLSockSetBlocking ("1", interp, argc, argv);
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockSetNonBlockingCmd --
 *
 *      Sets a socket nonblocking.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockSetNonBlockingCmd (ClientData dummy, Tcl_Interp * interp,
			       int argc, char **argv)
{
    return SSLSockSetBlocking ("0", interp, argc, argv);
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclSSLSockProc --
 *
 *      This is the C wrapper callback that is registered from
 *      ns_sockcallback.
 *
 * Results:
 *      NS_TRUE or NS_FALSE on error
 *
 * Side effects:
 *      Will run Tcl script.
 *
 *----------------------------------------------------------------------
 */

extern int
NsTclSSLSockProc (SOCKET sock, void *arg, int why)
{
    Tcl_Interp *interp;
    Tcl_DString script;
    char *w;
    int result;
    Callback *cbPtr = arg;

    if (why != NS_SOCK_EXIT || (cbPtr->when & NS_SOCK_EXIT)) {
	interp = Ns_TclAllocateInterp (NULL);
	result = EnterDup (interp, sock);
	if (result == TCL_OK) {
	    Tcl_DStringInit (&script);
	    Tcl_DStringAppend (&script, cbPtr->script, -1);
	    Tcl_DStringAppendElement (&script, interp->result);
	    if (why == NS_SOCK_READ) {
		w = "r";
	    } else if (why == NS_SOCK_WRITE) {
		w = "w";
	    } else if (why == NS_SOCK_EXCEPTION) {
		w = "e";
	    } else {
		w = "x";
	    }
	    Tcl_DStringAppendElement (&script, w);
	    result = NsTclEval (interp, script.string);
	    Tcl_DStringFree (&script);
	}
	if (result != TCL_OK) {
	    Ns_TclLogError (interp);
	} else if (!STREQ (interp->result, "1")) {
	    why = NS_SOCK_EXIT;
	}
	Ns_TclDeAllocateInterp (interp);
    }
    if (why == NS_SOCK_EXIT) {
	ns_sockclose (sock);
	ns_free (cbPtr);
	return NS_FALSE;
    }
    return NS_TRUE;
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLGetConn --
 *
 *      Return the SSL connection struct for the current connection.
 *
 * Results:
 *      Ns_OpenSSLConn* or NULL.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static Ns_OpenSSLConn *
NsOpenSSLGetConn (Tcl_Interp * interp)
{
    Ns_Conn *conn;
    char *name;

    /* conn = Ns_GetConn();  ** Ns_GetConn is gone */
    conn = Ns_TclGetConn (interp);
    if (conn != NULL) {
	name = Ns_ConnDriverName (conn);
	if (name != NULL && STREQ (name, DRIVER_NAME)) {
	    return (Ns_OpenSSLConn *) Ns_ConnDriverContext (conn);
	}
    }

    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * SockSetBlocking --
 *
 *      Set a socket blocking.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
SSLSockSetBlocking (char *value, Tcl_Interp * interp, int argc, char **argv)
{
    Tcl_Channel chan;

    if (argc != 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " sockId\"", NULL);
	return TCL_ERROR;
    }
    chan = Tcl_GetChannel (interp, argv[1], NULL);

    if (chan == NULL) {
	return TCL_ERROR;
    }
    return Tcl_SetChannelOption (interp, chan, "-blocking", value);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateTclChannel --
 *
 *	Dup connection sock and wrap read and write Tcl channels
 *      around them.
 *
 * Results:
 *	Tcl result. 
 *
 * Side effects:
 *
 *----------------------------------------------------------------------
 */

static int
CreateTclChannel (Ns_OpenSSLConn * ccPtr, Tcl_Interp * interp)
{
    Tcl_Channel chan;
    Tcl_DString ds;
    char channelName[16 + TCL_INTEGER_SPACE];

    Tcl_DStringInit (&ds);

    /* channel for reading */
    sprintf (channelName, "openssl%d", ccPtr->sock);

    /*
     * Although it's the read channel we make it writable
     * so we can do an ns_openssl_sockcheck on it to see if
     * it's still alive.
     */

    chan = Tcl_CreateChannel (&opensslChannelType,
			      channelName,
			      (ClientData) ccPtr,
			      (TCL_READABLE | TCL_WRITABLE));

    if (chan == (Tcl_Channel) NULL) {
	NsOpenSSLDestroyConn (ccPtr);
	Ns_Log (Error, "%s: %s: could not create new Tcl channel",
		ccPtr->module, ccPtr->type);
	Tcl_AppendResult (interp, "could not create new Tcl channel", NULL);
	return TCL_ERROR;
    }
    ccPtr->refcnt++;

    Tcl_SetChannelBufferSize (chan, BUFSIZ);
    Tcl_SetChannelOption (interp, chan, "-translation", "binary");
    Tcl_RegisterChannel (interp, chan);
    Tcl_DStringAppendElement (&ds, Tcl_GetChannelName (chan));

    /* channel for writing */
    ccPtr->wsock = ns_sockdup (ccPtr->sock);

    sprintf (channelName, "openssl%d", ccPtr->wsock);

    chan = Tcl_CreateChannel (&opensslChannelType,
			      channelName, (ClientData) ccPtr, TCL_WRITABLE);

    if (chan == (Tcl_Channel) NULL) {
	NsOpenSSLDestroyConn (ccPtr);
	Ns_Log (Error, "%s: %s: could not create new Tcl channel",
		ccPtr->module, ccPtr->type);
	Tcl_AppendResult (interp, "could not create new Tcl channel", NULL);
	return TCL_ERROR;
    }
    ccPtr->refcnt++;

    Tcl_SetChannelBufferSize (chan, BUFSIZ);
    Tcl_SetChannelOption (interp, chan, "-translation", "binary");
    Tcl_RegisterChannel (interp, chan);
    Tcl_DStringAppendElement (&ds, Tcl_GetChannelName (chan));
    Tcl_DStringResult (interp, &ds);

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ChanOutputProc --
 *
 *	Callback activated by Tcl puts and write commands. Sends data
 *      to the connected system.
 *
 * Results:
 *	Tcl result. 
 *
 * Side effects:
 *
 *----------------------------------------------------------------------
 */

static int
ChanOutputProc (ClientData instanceData, char *buf, int toWrite,
		int *errorCodePtr)
{
    Ns_OpenSSLConn *ccPtr = (Ns_OpenSSLConn *) instanceData;

    return NsOpenSSLSend (ccPtr, (void *) buf, toWrite);
}

/*
 *----------------------------------------------------------------------
 *
 * ChanInputProc --
 *
 *	Callback activated by Tcl gets and read on the Tcl channel. Reads
 *      data from the connected system.
 *
 * Results:
 *	Number of bytes read.
 *
 * Side effects:
 *	Places read data into buf, may set errorCodePtr, and adjusts
 *      connection state's read buffer pointer.
 *
 *----------------------------------------------------------------------
 */

static int
ChanInputProc (ClientData instanceData, char *buf, int bufSize,
	       int *errorCodePtr)
{
    Ns_OpenSSLConn *ccPtr = (Ns_OpenSSLConn *) instanceData;

    return NsOpenSSLRecv (ccPtr, (void *) buf, bufSize);
}

/*
 *----------------------------------------------------------------------
 *
 * ChanCloseProc --
 *
 *	Close down the Tcl channels and clean up the connection state
 *      data.
 *
 * Results:
 *	Tcl result. 
 *
 * Side effects:
 *	Will call functions to shutdown the SSL connection and free all
 *      data associated with the connection.
 *
 *      Note that this proc is called twice, once for the read channel
 *      and once for the write channel, so we need to check and see if
 *      ccPtr has already been freed.
 *
 *----------------------------------------------------------------------
 */

static int
ChanCloseProc (ClientData instanceData, Tcl_Interp * interp)
{
    Ns_OpenSSLConn *ccPtr = (Ns_OpenSSLConn *) instanceData;

    ccPtr->refcnt--;

    NsOpenSSLDestroyConn (ccPtr);

    /* XXX if errors occur, I should store the error in the interp's result. */

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ChanFlushProc --
 *
 *	Flush the date in the connection buffers.
 *
 * Results:
 *	TCL_OK.
 *
 * Side effects:
 *	Will open a connection and register two Tcl channels.
 *
 *----------------------------------------------------------------------
 */

static int
ChanFlushProc (ClientData instanceData)
{
    Ns_OpenSSLConn *ccPtr = (Ns_OpenSSLConn *) instanceData;

    NsOpenSSLFlush (ccPtr);

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ChanGetHandleProc --
 *
 *	Return the read or write socket.
 *
 * Results:
 *	TCL_OK
 *
 * Side effects:
 *	
 *
 *----------------------------------------------------------------------
 */

static int
ChanGetHandleProc (ClientData instanceData, int direction,
		   ClientData * handlePtr)
{
    Ns_OpenSSLConn *ccPtr = (Ns_OpenSSLConn *) instanceData;

    if (direction == TCL_READABLE) {
	*handlePtr = (ClientData) ccPtr->sock;
    } else {
	*handlePtr = (ClientData) ccPtr->wsock;
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ChanWatchProc --
 *
 *	Callback proc used by the Tcl channels. Doesn't do anything for
 *      us at the moment, but it is still required to be defined.
 *      Not having it causes a segfault when Tcl tries to
 *      work with it. Go read the Tcl_CreateChannel man page for Tcl 8.3+.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	
 *
 *----------------------------------------------------------------------
 */

static void
ChanWatchProc (ClientData instanceData, int mask)
{
#if 0				/* XXX ChanWatchProc: instanceData isn't used here yet */
    Ns_OpenSSLConn *ccPtr = (Ns_OpenSSLConn *) instanceData;
#endif

    return;
}

/*
 *----------------------------------------------------------------------
 *
 * SetResultToX509Name --
 *
 *      Set the Tcl interpreter's result to the string form of the
 *      specified X.509 name.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static void
SetResultToX509Name (Tcl_Interp * interp, X509_NAME * name)
{
    char *string;

    string = X509_NAME_oneline (name, NULL, 0);
    Tcl_SetResult (interp, string, TCL_VOLATILE);
    OPENSSL_free (string);
}

/*
 *----------------------------------------------------------------------
 *
 * SetResultToObjectName --
 *
 *      Set the Tcl interpreter's result to the string form of the
 *      specified ASN.1 object name.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static void
SetResultToObjectName (Tcl_Interp * interp, ASN1_OBJECT * obj)
{
    int nid;
    char *string;

    nid = OBJ_obj2nid (obj);
    if (nid == NID_undef) {
	Tcl_SetResult (interp, "UNKNOWN", TCL_STATIC);
    } else {
	string = (char *) OBJ_nid2ln (nid);
	if (string == NULL) {
	    Tcl_SetResult (interp, "ERROR", TCL_STATIC);
	} else {
	    Tcl_SetResult (interp, string, TCL_VOLATILE);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ValidTime --
 *
 *      Takes an ASN1_UTCTIME value and converts it into a string of
 *      the form "Aug 28 20:00:38 2002 GMT"
 *
 * Results:
 *      Pointer to null-terminated string allocated by Tcl_Alloc.
 *
 * Side effects:
 *      None.
 *
 *---------------------------------------------------------------------- */

static char *
ValidTime (ASN1_UTCTIME * tm)
{
    char *result;
    BIO *bio;
    int n;

    if ((bio = BIO_new (BIO_s_mem ())) == NULL)
	return NULL;

    ASN1_UTCTIME_print (bio, tm);
    n = BIO_pending (bio);
    result = Tcl_Alloc (n + 1);
    n = BIO_read (bio, result, n);
    result[n] = '\0';
    BIO_free (bio);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * PEMCertificate --
 *
 *      Retrieves the certificate in PEM format
 *
 * Results:
 *      Pointer to null-terminated string that contains the PEM
 *      certificate, allocated by Tcl_Alloc.
 *
 * Side effects:
 *      None.
 *
 *---------------------------------------------------------------------- */

static char *
PEMCertificate (X509 * peercert)
{
    char *result;
    BIO *bio;
    int n;

    if ((bio = BIO_new (BIO_s_mem ())) == NULL)
	return NULL;

    PEM_write_bio_X509 (bio, peercert);

    n = BIO_pending (bio);
    result = Tcl_Alloc (n + 1);
    n = BIO_read (bio, result, n);
    result[n] = '\0';
    BIO_free (bio);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * EnterSock, EnterDup --
 *
 *      Append a socket handle to the tcl result and register its
 *      channel.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      Will create channel, append handle to result.
 *
 *----------------------------------------------------------------------
 */

static int
EnterSock (Tcl_Interp * interp, SOCKET sock)
{
    Tcl_Channel chan;

    chan = Tcl_MakeTcpClientChannel ((ClientData) sock);
    if (chan == NULL) {
	Tcl_AppendResult (interp, "could not open socket", NULL);
	ns_sockclose (sock);
	return TCL_ERROR;
    }
    Tcl_SetChannelOption (interp, chan, "-translation", "binary");
    Tcl_RegisterChannel (interp, chan);
    sprintf (interp->result, "%s", Tcl_GetChannelName (chan));
    return TCL_OK;
}

static int
EnterDup (Tcl_Interp * interp, SOCKET sock)
{
    sock = ns_sockdup (sock);
    if (sock == INVALID_SOCKET) {
	Tcl_AppendResult (interp, "could not dup socket: ",
			  ns_sockstrerror (ns_sockerrno), NULL);
	return TCL_ERROR;
    }
    return EnterSock (interp, sock);
}

/*
 *----------------------------------------------------------------------
 *
 * GetSet --
 *
 *      Take a Tcl list of files and set bits for each in the list in
 *      an fd_set.
 *
 * Results:
 *      Tcl result.
 *
 * Side effects:
 *      Will set bits in fd_set. ppset may be NULL on error, or
 *      a valid fd_set on success. Max fd will be returned in *maxPtr.
 *
 *----------------------------------------------------------------------
 */

static int
GetSet (Tcl_Interp * interp, char *flist, int write, fd_set ** setPtrPtr,
	fd_set * setPtr, SOCKET * maxPtr)
{
    SOCKET sock;
    int fargc;
    char **fargv;
    int status;

    if (Tcl_SplitList (interp, flist, &fargc, &fargv) != TCL_OK) {
	return TCL_ERROR;
    }
    if (fargc == 0) {

	/*
	 * Tcl_SplitList failed, so abort.
	 */

	ckfree ((char *) fargv);
	*setPtrPtr = NULL;
	return TCL_OK;
    } else {
	*setPtrPtr = setPtr;
    }

    FD_ZERO (setPtr);
    status = TCL_OK;

    /*
     * Loop over each file, try to get its FD, and set the bit in
     * the fd_set.
     */

    while (fargc--) {
	if (Ns_TclGetOpenFd (interp, fargv[fargc], write,
			     (int *) &sock) != TCL_OK) {
	    status = TCL_ERROR;
	    break;
	}
	if (sock > *maxPtr) {
	    *maxPtr = sock;
	}
	FD_SET (sock, setPtr);
    }
    ckfree ((char *) fargv);

    return status;
}

/*
 *----------------------------------------------------------------------
 *
 * AppendReadyFiles --
 *
 *      Find files in an fd_set that are selected and append them to
 *      the tcl result, and also an optional passed-in dstring.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Ready files will be appended to pds if not null, and also
 *      interp->result.
 *
 *----------------------------------------------------------------------
 */

static void
AppendReadyFiles (Tcl_Interp * interp, fd_set * setPtr, int write,
		  char *flist, Tcl_DString * dsPtr)
{
    int fargc;
    char **fargv;
    SOCKET sock;
    Tcl_DString ds;

    Tcl_DStringInit (&ds);
    if (dsPtr == NULL) {
	dsPtr = &ds;
    }
    Tcl_SplitList (interp, flist, &fargc, &fargv);
    while (fargc--) {
	Ns_TclGetOpenFd (interp, fargv[fargc], write, (int *) &sock);
	if (FD_ISSET (sock, setPtr)) {
	    Tcl_DStringAppendElement (dsPtr, fargv[fargc]);
	}
    }

    /*
     * Append the ready files to the tcl interp.
     */

    Tcl_AppendElement (interp, dsPtr->string);
    ckfree ((char *) fargv);
    Tcl_DStringFree (&ds);
}

/*
 *----------------------------------------------------------------------
 *
 * SSLSockListenCallback --
 *
 *      This is the C wrapper callback that is registered from
 *      ns_openssl_socklistencallback.
 *
 * Results:
 *      NS_TRUE or NS_FALSE on error
 *
 * Side effects:
 *      Will run Tcl script.
 *
 *----------------------------------------------------------------------
 */

static int
SSLSockListenCallback (SOCKET sock, void *arg, int why)
{
    Ns_OpenSSLConn *ccPtr;
    Tcl_Interp *interp;
    Tcl_DString script;
    /* XXX   Callback      *cbPtr = arg; */
    char **sockv;
    int sockc, result;

    interp = Ns_TclAllocateInterp (NULL);

    ccPtr = Ns_OpenSSLSockAccept (sock);
    if (ccPtr == NULL) {
	Tcl_AppendResult (interp, "SSL accept failed \"", NULL);
	return TCL_ERROR;
    }

    result = CreateTclChannel (ccPtr, interp);

    if (result == TCL_OK) {
	Tcl_SplitList (interp, interp->result, &sockc, &sockv);
	Tcl_DStringInit (&script);
	Tcl_DStringAppend (&script, (char *) arg, -1);
	Tcl_DStringAppendElement (&script, sockv[0]);
	Tcl_DStringAppendElement (&script, sockv[1]);
	ckfree ((char *) sockv);
	result = NsTclEval (interp, script.string);
	Tcl_DStringFree (&script);
    }
    if (result != TCL_OK) {
	Ns_Log (Warning, "%s: %s: Tcl channel not available", ccPtr->module,
		ccPtr->type);
	Ns_TclLogError (interp);
    }
    Ns_TclDeAllocateInterp (interp);

    return NS_TRUE;
}

/*
 *----------------------------------------------------------------------
 *
 * NsTclEval --
 *
 *      Wraps Tcl_Eval() of Tcl_EvalEx() to avoid byte code compiling.
 *
 * Results:
 *      See Tcl_Eval
 *
 * Side effects:
 *      See Tcl_Eval
 *
 *----------------------------------------------------------------------
 */

static int
NsTclEval (Tcl_Interp * interp, char *script)
{
    int status;

    /*
     * Eval without the byte code compiler, and ensure that we
     * have a string result so old code can reference interp->result.
     */

    status = Tcl_EvalEx (interp, script, strlen (script), TCL_EVAL_DIRECT);
    Tcl_SetResult (interp, Tcl_GetString (Tcl_GetObjResult (interp)),
		   TCL_VOLATILE);

    return status;
}

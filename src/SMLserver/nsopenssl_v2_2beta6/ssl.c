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

#ifndef WIN32
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#include "nsopenssl.h"
#include "config.h"

#define BUFSIZE 2048

typedef struct Stream {
    Ns_OpenSSLConn *ccPtr;
    int error;
    int cnt;
    char *ptr;
    char buf[BUFSIZE + 1];
} Stream;

/*
 * Local functions defined in this file
 */

static int CreateSSL (Ns_OpenSSLConn * ccPtr);
static int CreateBIOStack (Ns_OpenSSLConn * ccPtr);
static int RunSSLHandshake (Ns_OpenSSLConn * ccPtr);
static int RunServerSSLHandshake (Ns_OpenSSLConn * ccPtr);

static Ns_OpenSSLConn *CreateSSLSockConn (int role, int conntype);
static void DestroySSLSockConn (Ns_OpenSSLConn * ccPtr);
static int GetLine (Stream * sPtr, Ns_DString * dsPtr);
static int FillBuf (Stream * sPtr);

#if 0				/* XXX not used right now, but may be later */
static int SetNonBlocking (Ns_OpenSSLConn * ccPtr, int flag);
#endif

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLCreateConn --
 *
 *	Create an SSL connection. The socket has already been accept()ed
 *      and is ready for reading/writing.
 *
 * Results:
 *      NS_ERROR or NS_OK.
 *
 * Side effects:
 *      If the SSL connection was open then it will be forced to close
 *      first.
 *
 *----------------------------------------------------------------------
 */

int
NsOpenSSLCreateConn (Ns_OpenSSLConn * ccPtr)
{
    if (CreateSSL (ccPtr) == NS_ERROR
	|| CreateBIOStack (ccPtr) == NS_ERROR
	|| RunSSLHandshake (ccPtr) == NS_ERROR) {
	Ns_Log (Debug, "%s: %s: NsOpenSSLCreateConn failed", ccPtr->module,
		ccPtr->type);
	SSL_set_shutdown (ccPtr->ssl,
			  SSL_SENT_SHUTDOWN | SSL_RECEIVED_SHUTDOWN);
	NsOpenSSLShutdown (ccPtr->ssl);
	NsOpenSSLDestroyConn (ccPtr);

	return NS_ERROR;
    }

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLDestroyConn --
 *
 *      Destroy an SSL connection.
 *
 * Results:
 *      None.
 *
 * Side effects: If the SSL connection was open then it will be forced
 *      to close first. If the connection is still being referenced,
 *      no action is taken.
 *
 *----------------------------------------------------------------------
 */

void
NsOpenSSLDestroyConn (Ns_OpenSSLConn * ccPtr)
{
    if (ccPtr->refcnt > 0)
		return;

#if 0
    Ns_Log (Debug, "%s: destroying conn (%p)",
	    ccPtr == NULL ? DRIVER_NAME : ccPtr->module, ccPtr);
#endif

    if (ccPtr != NULL) {

	/*
	 * We disallow sending through the socket,
	 * since BIO_free_all triggers SSL_shutdown,
	 * which is sending something (2 bytes).
	 * It confuses Win32 clients, since they automatically
	 * close socket on FIN packet
	 * only if there is no waiting received bytes
	 * (it gives "connection reset" message in MSIE when
	 * socket is freed by keepalive thread).
	 */

        if (ccPtr->sock != INVALID_SOCKET) {
            shutdown(ccPtr->sock, SHUT_WR);
        }

	if (ccPtr->peercert != NULL) {
	    X509_free (ccPtr->peercert);
	    ccPtr->peercert = NULL;
	}
	if (ccPtr->io != NULL) {
	    BIO_free_all (ccPtr->io);
	    ccPtr->io = NULL;
	}
	if (ccPtr->ssl != NULL) {
	    SSL_free (ccPtr->ssl);
	    ccPtr->ssl = NULL;
	}
#ifndef NS_MAJOR_VERSION
	if (ccPtr->sock != INVALID_SOCKET) {
	    ns_sockclose (ccPtr->sock);
	    ccPtr->sock = INVALID_SOCKET;
	}
#endif

	/*
	 * We only free the connection structure if it's an
	 * SSL socket type not tied to the comm API. If it is
	 * tied to the comm API, it's freed when the comm driver
	 * shuts down.
	 */

	if (ccPtr->conntype == CONNTYPE_SSL_SOCK) {
	    DestroySSLSockConn (ccPtr);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLRecv --
 *
 *      Read data from an SSL connection
 *
 * Results:
 *      The number of bytes read or a negative number in case of
 *      an error.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

int
NsOpenSSLRecv (Ns_OpenSSLConn * ccPtr, void *buffer, int toread)
{
    int rc;

    /*
     * Check the socket to see if it's still alive. If the client
     * aborts the connection during a file upload, the BIO read will
     * loop forever, using cpu cycles, but reading no further
     * data. Note that checking to see if sock is INVALID_SOCKET
     * doesn't always work here.
     */

    if (send (ccPtr->sock, NULL, 0, 0) != 0) {
	Ns_Log (Notice, "%s: %s: connection reset by peer",
		ccPtr->module, ccPtr->type);
	return NS_ERROR;
    }
#ifndef NS_MAJOR_VERSION
    do {
	rc = BIO_read (ccPtr->io, buffer, toread);
    } while (rc < 0 && BIO_should_retry (ccPtr->io));
#else
    rc = BIO_read (ccPtr->io, buffer, toread);
    if (rc < 0 && BIO_should_retry (ccPtr->io)
	&& Ns_SockWait (ccPtr->sock, NS_SOCK_READ, 2) == NS_OK) {
	rc = BIO_read (ccPtr->io, buffer, toread);
    }
    Ns_Log (Debug, "read: %d %d\n", toread, rc);
#endif

    return rc;
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLSend --
 *
 *  Send data through an SSL connection
 *
 * Results:
 *  The number of bytes send or a negative number in case of
 *      an error.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

int
NsOpenSSLSend (Ns_OpenSSLConn * ccPtr, void *buffer, int towrite)
{

#if 0
    /* XXX how it was done in nsopenssl 1.1c */
    return SSL_write (ccPtr->ssl, buffer, towrite);
#endif

    int rc;
    int total;

    total = towrite;

    do {
	rc = SSL_write (ccPtr->ssl, buffer, towrite);
	if (rc > 0)
	    towrite -= rc;

#if 0
	/*
	 * XXX On my Debian Linux box, the ns_openssl_socklisten (1) | connect test
	 * always generates a SSL_ERROR_SYSCALL. I think that's because I close
	 * the FD's on the server-side test too soon. The thing works anyway, but I need to
	 * debug this and make whatever changes necessary to get a clean closure
	 * for both ends.
	 */

	if (rc <= 0) {
	    switch (SSL_get_error (ccPtr->ssl, rc)) {
	    case SSL_ERROR_NONE:
		/* Perform operations */
		Ns_Log (Error, "SSL_write failure (%d): SSL_ERROR_NONE", rc);
		break;
	    case SSL_ERROR_SSL:
		/* Perform operations */
		Ns_Log (Error, "SSL_write failure (%d): SSL_ERROR_SSL", rc);
		break;
		/*
		 * The next four options are used with
		 * non-blocking semantics. This may not be
		 * applicable, depending on the underlying
		 * socket/BIO 
		 */
	    case SSL_ERROR_WANT_READ:
		Ns_Log (Error, "SSL_write failure (%d): SSL_ERROR_WANT_READ",
			rc);
		/*
		 * Read failed because there was no data to read
		 * and the process/thread was blocked waiting for
		 * input. Recall SSL_read when data is
		 * available. The select(2) system call is
		 * normally used.
		 */
		break;
	    case SSL_ERROR_WANT_WRITE:
		Ns_Log (Error, "SSL_write failure (%d): SSL_ERROR_WANT_WRITE",
			rc);
		/*
		 * Same as above for write. Because an SSL_read
		 * occurs does not mean a .WANT_WRITE. error
		 * will not appear as the SSL protocol involves
		 * message exchange.
		 */
		break;
	    case SSL_ERROR_WANT_CONNECT:
		Ns_Log (Error,
			"SSL_write failure(%d): SSL_ERROR_WANT_CONNECT", rc);
		/*
		 * If the application is using a connect BIO 
		 * eg. BIO_new_connect(), this error can be
		 * returned. Under Win32, it is possible to
		 * detect a completing connection. This is not
		 * as applicable under Unix.
		 */
		break;
	    case SSL_ERROR_WANT_X509_LOOKUP:
		Ns_Log (Error,
			"SSL_write failure (%d): SSL_ERROR_WANT_X509_LOOKUP",
			rc);
		/*
		 * This option is only returned if an application
		 * callback (used to retrieve a certificate)
		 * sets this condition for failure. The
		 * application must recall SSL_read() when the
		 * callback is able to find a certificate.
		 */
		break;
	    case SSL_ERROR_SYSCALL:
		Ns_Log (Error, "SSL_write failure (%d): SSL_ERROR_SYSCALL",
			rc);
		/*
		 * Call failed because an operating system
		 * dependent function failed. This is normally
		 * fatal.  Use SSL_get_error for further
		 * information.
		 */

		if (errno == EINTR)
		    continue;
		if (errno > 0)
		    Ns_Log (Error,
			    "SSL handshake interrupted by system (Stop button pressed in browser?)");
		else
		    Ns_Log (Error,
			    "Spurious SSL handshake interrupt (Usually just one of those OpenSSL confusions)");

		break;
	    case SSL_ERROR_ZERO_RETURN:
		Ns_Log (Error,
			"SSL_write failure (%d): SSL_ERROR_ZERO_RETURN", rc);
		/*
		 * Low level operating system call to read/write data
		 * returned 0. For most operating systems, when using
		 * sockets, this implies the other end of the socket
		 * was closed.
		 */
		break;
	    }
	} else {
	    towrite -= rc;
	    Ns_Log (Notice,
		    "NsOpenSSLSend: bytes written to client on this pass: %d of %d",
		    rc, total);
	    if (towrite == 0) {
		return rc;
	    } else if (towrite < 0) {
		Ns_Log (Error,
			"NsOpenSSLSend: %d bytes left to write, but rc went negative (%d), meaning an error occurred",
			towrite, rc);
		return rc;
	    }
	}
#endif

    } while (BIO_should_retry (ccPtr->ssl->wbio) &&
	     BIO_should_write (ccPtr->ssl->wbio));

    return rc;
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLFlush --
 *
 *      Flush an SSL connection.
 *
 * Results:
 *      Always NS_OK.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

int
NsOpenSSLFlush (Ns_OpenSSLConn * ccPtr)
{
    if (ccPtr->ssl == NULL) {
	return NS_ERROR;
    } else {
	if (BIO_flush (SSL_get_wbio (ccPtr->ssl)) < 1) {
	    Ns_Log (Error, "%s: BIO returned error on flushing buffer",
		    ccPtr->module);
	}
	return NS_OK;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLShutdown --
 *
 *      Shut down an SSL connection.
 *
 * Results:
 *	OpenSSL error code.
 *
 * Side effects:
 *	Calls SSL_shutdown multiple times to ensure the connection
 *      really has been shutdown.
 *
 * Note: based on SSL_smart_shutdown from mod_ssl, by Ralf Engelschall.
 *
 *----------------------------------------------------------------------
 */

int
NsOpenSSLShutdown (SSL * ssl)
{
    int i;
    int rc;

    /*
     * Call SSL_shutdown repeatedly until we're sure it's done.
     */

    for (i = rc = 0; rc == 0 && i < 4; i++) {
	rc = SSL_shutdown (ssl);
    }

    return rc;
}

/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLTrace --
 *
 *	Log the progress of an SSL connection.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Server log output.
 *
 *----------------------------------------------------------------------
 */

void
NsOpenSSLTrace (SSL * ssl, int where, int rc)
{
    Ns_OpenSSLConn *ccPtr;
    char *alertTypePrefix;
    char *alertType;
    char *alertDescPrefix;
    char *alertDesc;

    ccPtr = (Ns_OpenSSLConn *) SSL_get_app_data (ssl);

    if (where & SSL_CB_ALERT) {
	alertTypePrefix = "; alert type = ";
	alertType = SSL_alert_type_string_long (rc);
	alertDescPrefix = "; alert desc = ";
	alertDesc = SSL_alert_desc_string_long (rc);
    } else {
	alertTypePrefix = alertType = "";
	alertDescPrefix = alertDesc = "";
    }

    Ns_Log (Notice, "%s: trace: %s: %s%s%s%s%s", ccPtr->module,
	    ccPtr->type,
	    SSL_state_string_long (ssl),
	    alertTypePrefix, alertType, alertDescPrefix, alertDesc);
}

/*
 *----------------------------------------------------------------------
 *
 * Ns_OpenSSLSockConnect --
 *
 *      Open an SSL connection to the given host and port.
 *
 * Results:
 *      A pointer to a new Ns_OpenSSLConn structure.
 *
 * Side effects:
 *      Runs the SSL handshake.
 *
 *----------------------------------------------------------------------
 */

Ns_OpenSSLConn *
Ns_OpenSSLSockConnect (char *host, int port, int async, int timeout)
{
    Ns_OpenSSLConn *ccPtr;

    ccPtr = CreateSSLSockConn (ROLE_SSL_CLIENT, CONNTYPE_SSL_SOCK);

    /*
     * We leave the socket blocking until after the handshake.
     */

    if (timeout < 0) {
	ccPtr->sock = Ns_SockConnect (host, port);
    } else {
	ccPtr->sock = Ns_SockTimedConnect (host, port, timeout);
    }

    if (ccPtr->sock == INVALID_SOCKET) {
	DestroySSLSockConn (ccPtr);
	return NULL;
    }

    if (NsOpenSSLCreateConn (ccPtr) == NS_ERROR)
	return NULL;

    if (async)
	Ns_SockSetNonBlocking (ccPtr->sock);

    SSL_set_app_data (ccPtr->ssl, ccPtr);

    return ccPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Ns_OpenSSLSockListen --
 *
 *      Listen for connections with default backlog. Just a wrapper
 *      around Ns_SockListen at the moment.
 *
 * Results:
 *      A socket.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern SOCKET
Ns_OpenSSLSockListen (char *address, int port)
{
    return Ns_SockListen (address, port);
}

/*
 *----------------------------------------------------------------------
 *
 * Ns_OpenSSLSockAccept --
 *
 *      Accept a TCP socket, setting close on exec.
 *
 * Results:
 *      A socket or INVALID_SOCKET on error.
 *
 * Side effects:
 *      The socket is always placed in non-blocking mode.
 *
 *----------------------------------------------------------------------
 */

Ns_OpenSSLConn *
Ns_OpenSSLSockAccept (SOCKET sock)
{
    Ns_OpenSSLConn *ccPtr = NULL;

    if (sock == INVALID_SOCKET)
	return NULL;

    ccPtr = CreateSSLSockConn (ROLE_SSL_SERVER, CONNTYPE_SSL_SOCK);
    ccPtr->sock = sock;

    if (NsOpenSSLCreateConn (ccPtr) == NS_ERROR) {
	return NULL;
    }

    Ns_SockSetNonBlocking (ccPtr->sock);

    SSL_set_app_data (ccPtr->ssl, ccPtr);

    return ccPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * Ns_OpenSSLSockCallback --
 *
 *      Register a callback to be run when a socket that underlies an
 *      SSL connection reaches a certain state. The callback proc is
 *      responsible for layering SSL on top of the connected socket.
 *
 * Results:
 *      NS_OK/NS_ERROR
 *
 * Side effects:
 *      Will wake up the callback thread.
 *
 *----------------------------------------------------------------------
 */

/* XXX unusable with a direct call except from NsTclSSLSockCallback */
/* XXX essentially, the callback proc is going to have to be reponsible */
/* XXX for layering SSL on top of the socket once a connection comes in, */
/* XXX and before the script is run. I might need a new type, Ns_OpenSSLSockProc */
/* XXX but we'll see. I may be able to create a generic way to do this */
/* XXX so the developer using the API won't have to */

int
Ns_OpenSSLSockCallback (SOCKET sock, Ns_SockProc * proc, void *arg, int when)
{
    return Ns_SockCallback (sock, proc, arg, when);
}

/*
 *----------------------------------------------------------------------
 *
 * Ns_OpenSSLSockListenCallback --
 *
 *      Listen on an address/port that underlies an SSL connection and
 *      register a callback to be run when connections come in on it.
 *
 * Results:
 *      NS_OK/NS_ERROR
 *
 * Side effects:
 *      Will wake up the callback thread.
 *
 *----------------------------------------------------------------------
 */

/* XXX unusable with a direct call except from NsTclSSLSockListenCallback */
/* XXX essentially, the callback proc is going to have to be reponsible */
/* XXX for layering SSL on top of the socket once a connection comes in, */
/* XXX and before the script is run. I might need a new type, Ns_OpenSSLSockProc */
/* XXX but we'll see. I may be able to create a generic way to do this */
/* XXX so the developer using the API won't have to */

extern int
Ns_OpenSSLSockListenCallback (char *addr, int port, Ns_SockProc * proc,
			      void *arg)
{
    return Ns_SockListenCallback (addr, port, proc, arg);
}

/*
 *----------------------------------------------------------------------
 *
 * Ns_OpenSSLIsPeerCertValid --
 *
 *      Determine if the peer's certificate is valid.
 *
 * Results:
 *      NS_TRUE or NS_FALSE.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

extern int
Ns_OpenSSLIsPeerCertValid (Ns_OpenSSLConn * ccPtr)
{
    if (SSL_get_verify_result (ccPtr->ssl) == X509_V_OK) {
	return NS_TRUE;
    } else {
	return NS_FALSE;
    }

    /* Possible (long) values from SSL_get_verify_result:
       X509_V_OK
       X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT
       X509_V_ERR_UNABLE_TO_GET_CRL
       X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE
       X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE
       X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY
       X509_V_ERR_CERT_SIGNATURE_FAILURE
       X509_V_ERR_CRL_SIGNATURE_FAILURE
       X509_V_ERR_CERT_NOT_YET_VALID
       X509_V_ERR_CERT_HAS_EXPIRED
       X509_V_ERR_CRL_NOT_YET_VALID
       X509_V_ERR_CRL_HAS_EXPIRED
       X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD
       X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD
       X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD
       X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD
       X509_V_ERR_OUT_OF_MEM
       X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT
       X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN
       X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY
       X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE
       X509_V_ERR_CERT_CHAIN_TOO_LONG
       X509_V_ERR_CERT_REVOKED
       X509_V_ERR_APPLICATION_VERIFICATION
     */
}

/*                     
 *----------------------------------------------------------------------
 *
 * Ns_OpenSSLFetchURL --
 *
 *      Open up an HTTPS connection to an arbitrary URL.
 *
 * Results:
 *      NS_OK or NS_ERROR.  
 *
 * Side effects: Page contents will be appended to the passed-in
 *      dstring.  Headers returned to us will be put into the
 *      passed-in Ns_Set.  The set name will be changed to a copy of
 *      the HTTP status line.
 *
 *----------------------------------------------------------------------
 */

int
Ns_OpenSSLFetchURL (Ns_DString * dsPtr, char *url, Ns_Set * headers)
{
    Ns_OpenSSLConn *ccPtr = NULL;
    char *p;
    Ns_DString ds;
    Stream stream;
    Ns_Request *request;
    int status, tosend, n;

    status = NS_ERROR;
    Ns_DStringInit (&ds);

    /*
     * Parse the URL and open a connection.
     */

    Ns_DStringVarAppend (&ds, "GET ", url, " HTTP/1.0", NULL);
    request = Ns_ParseRequest (ds.string);
    if (request == NULL || request->protocol == NULL ||
	!STREQ (request->protocol, "https") || request->host == NULL) {
	Ns_Log (Notice, "urlopen: invalid url '%s'", url);
	goto done;
    }
    if (request->port == 0) {
	request->port = 443;
    }
    ccPtr = Ns_OpenSSLSockConnect (request->host, request->port, 0, 300);
    if (ccPtr == NULL) {
	Ns_Log (Error, "Ns_OpenSSLFetchURL: failed to connect to '%s'", url);
	goto done;
    }

    /*
     * Send a simple HTTP GET request.
     */

    Ns_DStringTrunc (&ds, 0);
    Ns_DStringVarAppend (&ds, "GET ", request->url, NULL);
    if (request->query != NULL) {
	Ns_DStringVarAppend (&ds, "?", request->query, NULL);
    }
    Ns_DStringAppend (&ds, " HTTP/1.0\r\nAccept: */*\r\n\r\n");
    p = ds.string;
    tosend = ds.length;
    while (tosend > 0) {
	n = NsOpenSSLSend (ccPtr, p, tosend);
	if (n <= 0) {
	    Ns_Log (Error, "urlopen: failed to send data to '%s'", url);
	    goto done;
	}
	tosend -= n;
	p += n;
    }

    /*
     * Buffer the socket and read the response line and then
     * consume the headers, parsing them into any given header set.
     */

    stream.cnt = 0;
    stream.error = 0;
    stream.ptr = stream.buf;
    stream.ccPtr = (Ns_OpenSSLConn *) ccPtr;
    if (!GetLine (&stream, &ds)) {
	goto done;
    }
    if (headers != NULL && strncmp (ds.string, "HTTP", 4) == 0) {
	if (headers->name != NULL) {
	    ns_free (headers->name);
	}
	headers->name = Ns_DStringExport (&ds);
    }
    do {
	if (!GetLine (&stream, &ds)) {
	    goto done;
	}
	if (ds.length > 0
	    && headers != NULL
	    && Ns_ParseHeader (headers, ds.string, Preserve) != NS_OK) {
	    goto done;
	}
    } while (ds.length > 0);

    /*
     * Without any check on limit or total size, foolishly read
     * the remaining content into the dstring.
     */

    do {
	Ns_DStringNAppend (dsPtr, stream.ptr, stream.cnt);
    } while (FillBuf (&stream));
    if (!stream.error) {
	status = NS_OK;
    }

  done:
    if (request != NULL) {
	Ns_FreeRequest (request);
    }
    if (ccPtr != NULL) {
	NsOpenSSLDestroyConn (ccPtr);
    }
    Ns_DStringFree (&ds);
    return status;
}

/*
 *----------------------------------------------------------------------
 *
 * Ns_OpenSSLFetchPage --
 *
 *      Fetch a page off of this very server. Url must reference a
 *      file in the filesystem.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      The file contents will be put into the passed-in dstring.
 *
 *----------------------------------------------------------------------
 */

int
Ns_OpenSSLFetchPage (Ns_DString * dsPtr, char *url, char *server)
{
    return Ns_FetchPage (dsPtr, url, server);
}

/*
 *----------------------------------------------------------------------
 *
 * CreateSSLSockConn --
 *
 *      Create the connection structure for a new SSL socket.
 *
 * Results:
 *      A pointer to a new Ns_OpenSSLConnConn structure.
 *
 * Side effects:
 *      Memory is allocated for the new structure.
 *
 *----------------------------------------------------------------------
 */

static Ns_OpenSSLConn *
CreateSSLSockConn (int role, int conntype)
{
    Ns_OpenSSLConn *ccPtr = NULL;

    ccPtr = (Ns_OpenSSLConn *) ns_calloc (1, sizeof (Ns_OpenSSLConn));
    if (ccPtr == NULL) {
	Ns_Log (Error, "%s: no memory for SSL socket connection structure",
		DRIVER_NAME);
	return NULL;
    }

    ccPtr->module = NsOpenSSLGetModuleName ();
    ccPtr->role = role;
    ccPtr->conntype = conntype;
    ccPtr->refcnt = 0;
    ccPtr->sock = INVALID_SOCKET;
    ccPtr->wsock = INVALID_SOCKET;

    if (role == ROLE_SSL_CLIENT) {
	ccPtr->type = STR_SOCK_CLIENT;
    } else {
	ccPtr->type = STR_SOCK_SERVER;
    }

    return ccPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * DestroySSLSockConn --
 *
 *      Free memory associated with an Ns_OpenSSLConn.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *
 *
 *----------------------------------------------------------------------
 */

static void
DestroySSLSockConn (Ns_OpenSSLConn * ccPtr)
{
    if (ccPtr->sock != INVALID_SOCKET) {
	ns_sockclose (ccPtr->sock);
	ccPtr->sock = INVALID_SOCKET;
    }

    if (ccPtr->wsock != INVALID_SOCKET) {
	ns_sockclose (ccPtr->wsock);
	ccPtr->wsock = INVALID_SOCKET;
    }

    ns_free (ccPtr);
    ccPtr = NULL;

    return;
}

/*
 *----------------------------------------------------------------------
 * 
 * FillBuf --
 * 
 *      Fill the socket stream buffer.
 *
 * Results:
 *      NS_TRUE if fill ok, NS_FALSE otherwise.
 *
 * Side effects:       
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
FillBuf (Stream * sPtr)
{
    int n;

    n = NsOpenSSLRecv (sPtr->ccPtr, sPtr->buf, BUFSIZE);
    if (n <= 0) {
	if (n < 0) {
	    Ns_Log (Error, "Ns_OpenSSLFetchURL: "
		    "failed to fill socket stream buffer");
	    sPtr->error = 1;
	}
	return NS_FALSE;
    }
    sPtr->buf[n] = '\0';
    sPtr->ptr = sPtr->buf;
    sPtr->cnt = n;

    return NS_TRUE;
}

/*
 *----------------------------------------------------------------------
 *   
 * GetLine --
 *
 *      Copy the next line from the stream to a dstring, trimming
 *      the \n and \r.
 *
 * Results:
 *      NS_TRUE or NS_FALSE.
 *
 * Side effects:
 *      The dstring is truncated on entry.
 *
 *----------------------------------------------------------------------
 */

static int
GetLine (Stream * sPtr, Ns_DString * dsPtr)
{
    char *eol;
    int n;

    Ns_DStringTrunc (dsPtr, 0);
    do {
	if (sPtr->cnt > 0) {
	    eol = strchr (sPtr->ptr, '\n');
	    if (eol == NULL) {
		n = sPtr->cnt;
	    } else {
		*eol++ = '\0';
		n = eol - sPtr->ptr;
	    }
	    Ns_DStringNAppend (dsPtr, sPtr->ptr, n - 1);
	    sPtr->ptr += n;
	    sPtr->cnt -= n;
	    if (eol != NULL) {
		n = dsPtr->length;
		if (n > 0 && dsPtr->string[n - 1] == '\r') {
		    Ns_DStringTrunc (dsPtr, n - 1);
		}
		return NS_TRUE;
	    }
	}
    } while (FillBuf (sPtr));

    return NS_FALSE;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateSSL --
 *
 *	Create the SSL struct for a connection.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *	Memory is allocated by SSL_new.
 *
 *----------------------------------------------------------------------
 */

static int
CreateSSL (Ns_OpenSSLConn * ccPtr)
{
    /*
     * If the connection is managed by nsd's comm API, then
     * the context is already set.
     */

    if (ccPtr->context == NULL) {
	if (ccPtr->role == ROLE_SSL_CLIENT) {
	    ccPtr->context = NsOpenSSLGetSockClientSSLContext ();
	} else if (ccPtr->role == ROLE_SSL_SERVER) {
	    ccPtr->context = NsOpenSSLGetSockServerSSLContext ();
	}
    }

    ccPtr->ssl = SSL_new (ccPtr->context);
    if (ccPtr->ssl == NULL) {
	Ns_Log (Error, "%s: error creating new SSL", ccPtr->module);
	return NS_ERROR;
    }

    SSL_clear (ccPtr->ssl);

    if (ccPtr->role == ROLE_SSL_SERVER) {
	SSL_set_accept_state (ccPtr->ssl);
    } else {
	SSL_set_connect_state (ccPtr->ssl);
    }

    SSL_set_app_data (ccPtr->ssl, ccPtr);

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateBIOStack --
 *
 *	Create a BIO stack that will be used to read and write to via
 *      SSL.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
CreateBIOStack (Ns_OpenSSLConn * ccPtr)
{
    BIO *sock_bio;
    BIO *ssl_bio;

    /*
     * BIO stack:
     *
     *   nsopenssl module
     *   buffering BIO
     *   SSL BIO
     *   socket BIO
     *   TCP socket to client
     */

    /* Create socket BIO and attach it to the socket */

    sock_bio = BIO_new_socket (ccPtr->sock, BIO_NOCLOSE);

    /* Create SSL BIO */

    ssl_bio = BIO_new (BIO_f_ssl ());
    BIO_set_ssl (ssl_bio, ccPtr->ssl, BIO_NOCLOSE);
    BIO_push (ssl_bio, sock_bio);

    /* Create buffering BIO */

    ccPtr->io = BIO_new (BIO_f_buffer ());
    if (!BIO_set_write_buffer_size (ccPtr->io, ccPtr->bufsize))
	return NS_ERROR;
    BIO_push (ccPtr->io, ssl_bio);

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SetNonBlocking --
 *
 *	Put the socket in blocking/nonblocking mode.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

/* XXX not used right now, but may be shortly */

static int
SetNonBlocking (Ns_OpenSSLConn * ccPtr, int flag)
{
    return BIO_socket_nbio (ccPtr->sock, flag) ? NS_OK : NS_ERROR;

#if 0
    int rc;

    if (flag) {
	Ns_SockSetNonBlocking (ccPtr->sock);
    } else {
	Ns_SockSetBlocking (ccPtr->sock);
    }
    rc = BIO_set_nbio (ccPtr->io, flag);
    Ns_Log (Debug, "Set BIO to BIO_set_nbio = %d", flag);

    return NS_OK;
#endif

}

/*
 *----------------------------------------------------------------------
 *
 * RunSSLHandshake --
 *
 *	Run the SSL handshake sequence.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *	Sets pointer to peer certificate
 *
 *----------------------------------------------------------------------
 */

static int
RunSSLHandshake (Ns_OpenSSLConn * ccPtr)
{
    int rc;
    char buffer[256];
    char *buf = (char *) &buffer;

    if (ccPtr->role == ROLE_SSL_SERVER) {
	return RunServerSSLHandshake (ccPtr);
    }

    do {
	rc = BIO_do_handshake (ccPtr->io);
#if 0
	if (rc < 0) {
	    ERR_error_string (ERR_get_error (), buf);
	    Ns_Log (Error, "%s: %s", ccPtr->module, buf);
	}
#endif
    } while (rc < 0 && BIO_should_retry (ccPtr->io));

    if (rc < 0) {
	return NS_ERROR;
    }

    ccPtr->peercert = SSL_get_peer_certificate (ccPtr->ssl);

    /* Test cert validity in log file */
    if (Ns_OpenSSLIsPeerCertValid (ccPtr)) {
	Ns_Log (Notice, "%s: %s: SERVER's CERT is VALID", ccPtr->module,
		ccPtr->type);
    } else {
	Ns_Log (Notice, "%s: %s: SERVER's CERT is NOT VALID", ccPtr->module,
		ccPtr->type);
    }

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * RunServerSSLHandshake --
 *
 *      Run the Server SSL handshake sequence.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
RunServerSSLHandshake (Ns_OpenSSLConn * ccPtr)
{
    int rc;
    int error;
    time_t endtime;
    struct timeval tv;
    int n;
    fd_set *wfds;
    fd_set *rfds;
    fd_set fds;

    if (SetNonBlocking (ccPtr, 1) == NS_ERROR) {
	Ns_Log (Warning,
		"%s: could not put socket in non-blocking mode; "
		"timeout may not be enforced: %s",
		ccPtr->module, ns_sockstrerror (errno));
    }

    endtime = time (NULL) + ccPtr->timeout + 1;
    FD_ZERO (&fds);

    while (1) {

	rc = SSL_accept (ccPtr->ssl);

	if (rc == 1) {
	    break;
	}

	error = SSL_get_error (ccPtr->ssl, rc);

	if (error == SSL_ERROR_SYSCALL) {
	    if (rc == 0) {
		Ns_Log (Error, "%s: EOF during SSL handshake", ccPtr->module);
	    } else {
		Ns_Log (Error, "%s: error during SSL handshake: %s",
			ccPtr->module, ns_sockstrerror (errno));
	    }
	    return NS_ERROR;

	} else if (error == SSL_ERROR_WANT_READ) {
	    rfds = &fds;
	    wfds = NULL;

	} else if (error == SSL_ERROR_WANT_WRITE) {
	    rfds = NULL;
	    wfds = &fds;

	} else {
	    Ns_Log (Error, "%s: error %d/%d during SSL handshake",
		    ccPtr->module, rc, error);
	    return NS_ERROR;
	}

	FD_SET (ccPtr->sock, &fds);

	do {
	    tv.tv_sec = endtime - time (NULL);
	    tv.tv_usec = 0;
	    n = select (ccPtr->sock + 1, rfds, wfds, NULL, &tv);
	} while (n < 0 && errno == EINTR);

	if (n < 0) {
	    Ns_Log (Error, "%s: select failed: %s",
		    ccPtr->module, ns_sockstrerror (errno));
	    return NS_ERROR;
	}

	if (n == 0) {
	    Ns_Log (Notice, "%s: SSL handshake timeout", ccPtr->module);
	    return NS_ERROR;
	}
    }

    ccPtr->peercert = SSL_get_peer_certificate (ccPtr->ssl);

    if (SetNonBlocking (ccPtr, 0) == NS_ERROR) {
	Ns_Log (Warning,
		"%s: could not put socket in blocking mode; "
		"results unpredictable: %s",
		ccPtr->module, ns_sockstrerror (errno));
    }

    /* XXX log if the cert is valid as a test */
#if 0
    /* Test cert validity in log file */
    if (Ns_OpenSSLIsPeerCertValid (ccPtr)) {
	Ns_Log (Notice, "%s: %s: CLIENT's CERT is VALID", ccPtr->module,
		ccPtr->type);
    } else {
	Ns_Log (Notice, "%s: %s: CLIENT's CERT is NOT VALID", ccPtr->module,
		ccPtr->type);
    }
#endif

    return NS_OK;

}

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
 * Copyright (C) 2000-2001 Scott S. Goodwin
 * Copyright (C) 2000 Rob Mayoff
 * Copyright (C) 2000 Freddie Mendoza
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

/*
 * nsopenssl.c
 *
 * This module implements an SSL socket driver using the OpenSSL library.
 *
 * Originally by Stefan Arentz <stefan.arentz@soze.com>.
 *
 * AOLserver 3.0 adaptations by Freddie Mendoza <avm@satori.com>.
 *
 * Client certificate support, Tcl interface, bug fixes 
 * by Scott Goodwin <scott@scottg.net>
 *
 * Some serious code refactoring and cleanup by Rob Mayoff
 * <mayoff@arsdigita.com>.
 *
 * References:
 *
 *  Distribution - http://stefan.arentz.nl/software/
 *  OpenSSL - http://www.openssl.org
 *  AOLServer Home - http://www.aolserver.com
 *  SSL for Apache - http://www.modssl.org
 *  nsopenssl home - http://scottg.net
 *
 */

static const char *RCSID = "@(#) $Header$, compiled: " __DATE__ " " __TIME__;

#include <sys/stat.h>
#include <ctype.h>
#include <limits.h>
#include <time.h>

#include "nsopenssl.h"
#include "config.h"
#include "tclcmds.h"

/* Standard module interface symbols */

NS_EXPORT int Ns_ModuleVersion = 1;

NS_EXPORT int Ns_ModuleInit(char *server, char *module);




/*
 * The following int is used to identify SSL connections with a unique id.
 * Used for profiling only
 * 2003-09-14, nh
 */
#ifdef PROFILING
static unsigned int nextSSLConnId;
#endif /* PROFILING */

/*
 * Local functions defined in this file
 */

static Ns_ThreadProc SockThread;
static void SockFreeConn(NsOpenSSLDriver *sdPtr, NsOpenSSLConnection *scPtr);
static Ns_Thread sockThread;
static SOCKET trigPipe[2];

static Ns_DriverStartProc      SockStart;
static Ns_DriverStopProc       SockStop;
static Ns_ConnReadProc         SockRead;
static Ns_ConnWriteProc        SockWrite;
static Ns_ConnCloseProc        SockClose;
static Ns_ConnConnectionFdProc SockConnectionFd;
static Ns_ConnDetachProc       SockDetach;
static Ns_ConnPeerProc         SockPeer;
static Ns_ConnLocationProc     SockLocation;
static Ns_ConnPeerPortProc     SockPeerPort;
static Ns_ConnPortProc         SockPort;
static Ns_ConnHostProc         SockHost;
static Ns_ConnDriverNameProc   SockName;
static Ns_ConnInitProc         SockInit;

/* Linked list of all configured nsopenssl instances */
static NsOpenSSLDriver *firstSSLDriverPtr;

static Ns_DrvProc sockProcs[] = {
    {Ns_DrvIdStart,        (void *) SockStart},
    {Ns_DrvIdStop,         (void *) SockStop},
    {Ns_DrvIdRead,         (void *) SockRead},
    {Ns_DrvIdWrite,        (void *) SockWrite},
    {Ns_DrvIdClose,        (void *) SockClose},
    {Ns_DrvIdHost,         (void *) SockHost},
    {Ns_DrvIdPort,         (void *) SockPort},
    {Ns_DrvIdName,         (void *) SockName},
    {Ns_DrvIdPeer,         (void *) SockPeer},
    {Ns_DrvIdPeerPort,     (void *) SockPeerPort},
    {Ns_DrvIdLocation,     (void *) SockLocation},
    {Ns_DrvIdConnectionFd, (void *) SockConnectionFd},
    {Ns_DrvIdDetach,       (void *) SockDetach},
    {Ns_DrvIdInit,         (void *) SockInit},
    {0,                    NULL}
};


/*
 *----------------------------------------------------------------------
 *
 * Ns_ModuleInit --
 *
 *  Sock module init routine.
 *
 * Results:
 *  NS_OK if initialized ok, NS_ERROR otherwise.
 *
 * Side effects:
 *  Calls Ns_RegisterLocation as specified by this instance
 *  in the config file.
 *
 *----------------------------------------------------------------------
 */

NS_EXPORT int
Ns_ModuleInit(char *server, char *module)
{
    NsOpenSSLDriver *sdPtr;

    if (Ns_TclInitInterps(server, NsOpenSSLInterpInit, NULL)
	    != NS_OK) {
	return NS_ERROR;
    }

    sdPtr = NsOpenSSLCreateDriver(server, module, sockProcs);
    if (sdPtr == NULL) {
	return NS_ERROR;
    }

    sdPtr->nextPtr = firstSSLDriverPtr;
    firstSSLDriverPtr = sdPtr;

    return NS_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * SockStart --
 *
 *	Configure and then start the SockThread servicing new
 *	connections.  This is the final initializiation routine
 *	called from main().
 *
 * Results:
 *	NS_OK or NS_ERROR.
 *
 * Side effects:
 *	SockThread is created.
 *
 *----------------------------------------------------------------------
 */

static int
SockStart(char *server, char *label, void **drvDataPtr)
{
    NsOpenSSLDriver *sdPtr = *((NsOpenSSLDriver **) drvDataPtr);

    sdPtr->lsock = Ns_SockListen(sdPtr->bindaddr, sdPtr->port);
    if (sdPtr->lsock == INVALID_SOCKET) {
	Ns_Fatal("%s: could not listen on %s:%d: %s",
	    sdPtr->module, sdPtr->address ? sdPtr->address : "*",
	    sdPtr->port, ns_sockstrerror(ns_sockerrno));
	return NS_ERROR;
    }

    if (sockThread == NULL) {
	if (ns_sockpair(trigPipe) != 0) {
	    Ns_Fatal("ns_sockpair() failed: %s",
		ns_sockstrerror(ns_sockerrno));
	}
	Ns_ThreadCreate(SockThread, NULL, 0, &sockThread);
    }
    return NS_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * SockFreeConn --
 *
 *  Return a connection to the free list.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

static void
SockFreeConn(NsOpenSSLDriver *sdPtr, NsOpenSSLConnection *scPtr)
{
    int refcnt;

    Ns_MutexLock(&sdPtr->lock);
    if (scPtr != NULL) {
	scPtr->nextPtr = sdPtr->firstFreePtr;
	sdPtr->firstFreePtr = scPtr;
    }
    refcnt = --sdPtr->refcnt;
    Ns_MutexUnlock(&sdPtr->lock);

    if (refcnt == 0) {
	NsOpenSSLFreeDriver(sdPtr);
    }
}


/*
 *----------------------------------------------------------------------
 *
 * SockThread --
 *
 *  Main listening socket driver thread.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Connections are accepted on the configured listen sockets
 *  and placed on the run queue to be serviced.
 *
 *----------------------------------------------------------------------
 */

static void
SockThread(void *ignored)
{
    fd_set set, watch;
    char c;
    int slen, n, stop;
#ifdef PROFILING
    int ssl_conn_id;
#endif /* PROFILING */
    NsOpenSSLDriver *sdPtr, *nextPtr;
    NsOpenSSLConnection *scPtr;
    struct sockaddr_in sa;
    SOCKET max, sock;

    Ns_ThreadSetName("-nsopenssl-");
    Ns_Log(Notice, "waiting for startup");
    Ns_WaitForStartup();
    Ns_Log(Notice, "starting");

    FD_ZERO(&watch);
    FD_SET(trigPipe[0], &watch);
    max = trigPipe[0];

    sdPtr = firstSSLDriverPtr;
    firstSSLDriverPtr = NULL;
    while (sdPtr != NULL) {

	nextPtr = sdPtr->nextPtr;
	if (sdPtr->lsock != INVALID_SOCKET) {
	    Ns_Log(Notice, "%s: listening on %s (%s:%d)",
		sdPtr->module, sdPtr->location,
		sdPtr->address ? sdPtr->address : "*", sdPtr->port);
	    if (max < sdPtr->lsock) {
		max = sdPtr->lsock;
	    }
	    FD_SET(sdPtr->lsock, &watch);
	    Ns_SockSetNonBlocking(sdPtr->lsock);
	    sdPtr->nextPtr = firstSSLDriverPtr;
	    firstSSLDriverPtr = sdPtr;
	}
	sdPtr = nextPtr;

    }
    ++max;

    Ns_Log(Notice, "accepting connections");

    stop = 0;
    do {
        memcpy(&set, &watch, sizeof(fd_set));
	do {
	    n = select(max, &set, NULL, NULL, NULL);
	} while (n < 0  && ns_sockerrno == EINTR);
	if (n < 0) {
	    Ns_Fatal("select() failed: %s", ns_sockstrerror(ns_sockerrno));
	} else if (FD_ISSET(trigPipe[0], &set)) {
	    if (recv(trigPipe[0], &c, 1, 0) != 1) {
		Ns_Fatal("trigger recv() failed: %s",
		    ns_sockstrerror(ns_sockerrno));
	    }
	    Ns_Log(Notice, "stopping");
	    stop = 1;
	    --n;
	}

	sdPtr = firstSSLDriverPtr;
	while (n > 0 && sdPtr != NULL) {
	    if (FD_ISSET(sdPtr->lsock, &set)) {
		--n;
		slen = sizeof(sa);
		sock = accept(sdPtr->lsock, (struct sockaddr *) &sa, &slen);
		if (sock != INVALID_SOCKET) {
		    Ns_MutexLock(&sdPtr->lock);
		    sdPtr->refcnt++;
		    scPtr = sdPtr->firstFreePtr;
		    if (scPtr != NULL) {
			sdPtr->firstFreePtr = scPtr->nextPtr;
		    }
#ifdef PROFILING
		    ssl_conn_id = nextSSLConnId++;
#endif /* PROFILING */
		    Ns_MutexUnlock(&sdPtr->lock);
		    if (scPtr == NULL) {
			scPtr = (NsOpenSSLConnection *)
			    ns_malloc(sizeof *scPtr);
		    }

		    memset(scPtr, 0, sizeof *scPtr);
		    scPtr->sdPtr = sdPtr;
		    scPtr->sock = sock;
		    scPtr->port = ntohs(sa.sin_port);
#ifdef PROFILING
		    scPtr->ssl_conn_id = ssl_conn_id;
#endif /* PROFILING */
		    strcpy(scPtr->peer, ns_inet_ntoa(sa.sin_addr));

		    if (Ns_QueueConn(sdPtr->driver, scPtr) != NS_OK) {
			Ns_Log(Warning, "%s: connection dropped",
			    sdPtr->module);
			(void) SockClose(scPtr);
		    }
		}
	    }
	    sdPtr = sdPtr->nextPtr;
	}
    } while (!stop);

    while ((sdPtr = firstSSLDriverPtr) != NULL) {
	firstSSLDriverPtr = sdPtr->nextPtr;
	Ns_Log(Notice, "%s: closing %s", sdPtr->module, sdPtr->location);
	ns_sockclose(sdPtr->lsock);
	SockFreeConn(sdPtr, NULL);
    }

    ns_sockclose(trigPipe[0]);
    ns_sockclose(trigPipe[1]);
}


/*
 *----------------------------------------------------------------------
 *
 * SockStop --
 *
 *  Trigger the SockThread to shutdown.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  SockThread will close ports.
 *
 *----------------------------------------------------------------------
 */

static void
SockStop(void *arg)
{
    NsOpenSSLDriver *sdPtr = (NsOpenSSLDriver *) arg;

    if (sockThread != NULL) {
        Ns_Log(Notice, DEFAULT_NAME ":  exiting: triggering shutdown");
	if (send(trigPipe[1], "", 1, 0) != 1) {
	    Ns_Fatal("trigger send() failed: %s",
		ns_sockstrerror(ns_sockerrno));
	}
	Ns_ThreadJoin(&sockThread, NULL);
	sockThread = NULL;
        Ns_Log(Notice, DEFAULT_NAME ":  exiting: shutdown complete");
    }
}


/*
 *----------------------------------------------------------------------
 *
 * SockClose --
 *
 *  Close the socket
 *
 * Results:
 *  NS_OK/NS_ERROR
 *
 * Side effects:
 *  Socket will be closed and buffer returned to free list.
 *
 *----------------------------------------------------------------------
 */

static int
SockClose(void *arg)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;
    NsOpenSSLDriver *sdPtr = scPtr->sdPtr;

    Ns_Log(Debug, "%s: SockClose", sdPtr->module);
    if (scPtr->sock != INVALID_SOCKET) {
	if (scPtr->ssl != NULL) {
	    NsOpenSSLFlush(scPtr);
	}
	NsOpenSSLDestroyConn(scPtr);
    }
    SockFreeConn(sdPtr, scPtr);
    return NS_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * SockRead --
 *
 *  Read from the socket
 *
 * Results:
 *  # bytes read
 *
 * Side effects:
 *  Will read from socket
 *
 *----------------------------------------------------------------------
 */

static int
SockRead(void *arg, void *vbuf, int toread)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    return NsOpenSSLRecv(scPtr, vbuf, toread);
}


/*
 *----------------------------------------------------------------------
 *
 * SockWrite --
 *
 *  Writes data to a socket.
 *  NOTE: This may not write all of the data you send it!
 *
 * Results:
 *  Number of bytes written, -1 for error
 *
 * Side effects:
 *  Bytes may be written to a socket
 *
 *----------------------------------------------------------------------
 */

static int
SockWrite(void *arg, void *buf, int towrite)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    return NsOpenSSLSend(scPtr, buf, towrite);
}


/*
 *----------------------------------------------------------------------
 *
 * SockHost --
 *
 *  Return the host (addr) I'm bound to
 *
 * Results:
 *  String hostname
 *
 * Side effects:
 *  None
 *
 *----------------------------------------------------------------------
 */

static char *
SockHost(void *arg)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    return scPtr->sdPtr->address;
}


/*
 *----------------------------------------------------------------------
 *
 * SockPort --
 *
 *  Get the port I'm listening on.
 *
 * Results:
 *  A TCP port number
 *
 * Side effects:
 *  None
 *
 *----------------------------------------------------------------------
 */

static int
SockPort(void *arg)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    return scPtr->sdPtr->port;
}


/*
 *----------------------------------------------------------------------
 *
 * SockName --
 *
 * 	Return the name of this driver
 *
 * Results:
 *	DRIVER_NAME.
 *
 * Side effects:
 * 	None
 *
 *----------------------------------------------------------------------
 */

static char *
SockName(void *arg)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    return DRIVER_NAME;
}


/*
 *----------------------------------------------------------------------
 *
 * SockPeer --
 *
 *  Return the string name of the peer address
 *
 * Results:
 *  String peer (ip) addr
 *
 * Side effects:
 *  None
 *
 *----------------------------------------------------------------------
 */

static char *
SockPeer(void *arg)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    return scPtr->peer;
}


/*
 *----------------------------------------------------------------------
 *
 * SockConnectionFd --
 *
 *  Get the socket fd
 *
 * Results:
 *  The socket fd
 *
 * Side effects:
 *  None
 *
 *----------------------------------------------------------------------
 */

static int
SockConnectionFd(void *arg)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    if (NsOpenSSLFlush(scPtr) == NS_ERROR) {
	return -1;
    }

    return (int) scPtr->sock;
}


/*
 *----------------------------------------------------------------------
 *
 * SockDetach --
 *
 *  Detach the connection data from this connection for keep-alive.
 *
 * Results:
 *  Pointer to connection data.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

static void *
SockDetach(void *arg)
{
    return arg;
}


/*
 *----------------------------------------------------------------------
 *
 * SockPeerPort --
 *
 *  Get the peer's originating tcp port
 *
 * Results:
 *  A tcp port
 *
 * Side effects:
 *  None
 *
 *----------------------------------------------------------------------
 */

static int
SockPeerPort(void *arg)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    return scPtr->port;
}


/*
 *----------------------------------------------------------------------
 *
 * SockLocation --
 *
 *  Returns the location, suitable for making anchors
 *
 * Results:
 *  String location
 *
 * Side effects:
 *  none
 *
 *----------------------------------------------------------------------
 */

static char *
SockLocation(void *arg)
{
    NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

    return scPtr->sdPtr->location;
}


/*
 *----------------------------------------------------------------------
 *
 * SockInit --
 *
 *      Initialize the SSL connection.
 *
 * Results:
 *  NS_OK/NS_ERROR
 *
 * Side effects:
 *  Stuff may be written to a socket.
 *
 *----------------------------------------------------------------------
 */

static int
SockInit(void *arg)
{
  int res;
#ifdef PROFILING
  Ns_Time now;
#endif /* PROFILING */
  NsOpenSSLConnection *scPtr = (NsOpenSSLConnection *) arg;

#ifdef PROFILING
  Ns_GetTime(&now);
  Ns_Log(Notice, "SSL[%d,%d] start[%d,%d,%s,%d]",
	 scPtr->ssl_conn_id,Ns_InfoBootTime(),now.sec,now.usec,scPtr->peer,scPtr->port);
#endif /* PROFILING */

    if (scPtr->ssl == NULL) {
      res = NsOpenSSLCreateConn(scPtr);
#ifdef PROFILING
      Ns_GetTime(&now);
      Ns_Log(Notice, "SSL[%d,%d] end[%d,%d]",
	     scPtr->ssl_conn_id,Ns_InfoBootTime(),now.sec,now.usec);
#endif /* PROFILING */
      return res;
    } else {
	return NS_OK;
    }
}


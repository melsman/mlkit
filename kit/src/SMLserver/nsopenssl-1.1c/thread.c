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

static const char *RCSID = "@(#) $Header$, compiled: " __DATE__ " " __TIME__;

#define OPENSSL_THREAD_DEFINES

#include "nsopenssl.h"
#include "thread.h"

#ifndef THREADS
#include "OpenSSL was not compiled with thread support!"
#endif

static Ns_Mutex *locks;

static void LockCallback(int mode, int n, const char *file, int line);
static unsigned long IdCallback(void);
static struct CRYPTO_dynlock_value *DynlockCreateCallback(char *file,
    int line);
static void DynlockLockCallback(int mode,
    struct CRYPTO_dynlock_value *dynlock, const char *file, int line);
static void DynlockDestroyCallback(struct CRYPTO_dynlock_value *dynlock,
    const char *file, int line);


/*
 *----------------------------------------------------------------------
 *
 * NsOpenSSLInitThreads --
 *
 *       Set up thread callbacks for the SSL library.
 *
 * Results:
 *       NS_OK or NS_ERROR.
 *
 * Side effects:
 *       None.
 *
 *----------------------------------------------------------------------
 */

int
NsOpenSSLInitThreads(void)
{
    static int initialized = 0;

    int  i;
    int  num_locks;
    char buf[100];

    if (!initialized) {
	initialized = 1;

	if (CRYPTO_set_mem_functions(ns_malloc, ns_realloc, ns_free) == 0) {
	    Ns_Log(Warning, DRIVER_NAME
		": could not set OpenSSL memory functions");
	}

	num_locks = CRYPTO_num_locks();
	locks = ns_calloc(num_locks, sizeof *locks);
	for (i = 0; i < num_locks; i++) {
	    sprintf(buf, "openssl-%d", i);
	    Ns_MutexSetName2(locks + i, DRIVER_NAME, buf);
	}

	CRYPTO_set_locking_callback(LockCallback);
	CRYPTO_set_id_callback(IdCallback);
    }

    return NS_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * LockCallback --
 *
 *      Lock or unlock a mutex for OpenSSL.
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
LockCallback(int mode, int n, const char *file, int line)
{
    if (mode & CRYPTO_LOCK) {
	Ns_MutexLock(locks + n);
    } else {
	Ns_MutexUnlock(locks + n);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * IdCallback --
 *
 *      Return this thread's id for OpenSSL.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static unsigned long
IdCallback(void)
{
    return (unsigned long) Ns_ThreadId();
}

/*
 *----------------------------------------------------------------------
 *
 * DynlockCreateCallback --
 *
 *      Create a dynamically-allocated mutex for OpenSSL.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static struct CRYPTO_dynlock_value *
DynlockCreateCallback(char *file, int line)
{
    Ns_Mutex *lock;
    Ns_DString ds;

    lock = ns_calloc(1, sizeof *lock);

    Ns_DStringInit(&ds);
    Ns_DStringVarAppend(&ds, "openssl: ", file, ": ");
    Ns_DStringPrintf(&ds, "%d", line);

    Ns_MutexSetName2(lock, DRIVER_NAME, Ns_DStringValue(&ds));

    return (struct CRYPTO_dynlock_value *) lock;
}

/*
 *----------------------------------------------------------------------
 *
 * DynlockLockCallback --
 *
 *      Lock or unlock a dynamically-allocated mutex for OpenSSL.
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
DynlockLockCallback(int mode, struct CRYPTO_dynlock_value *dynlock,
    const char *file, int line)
{
    if (mode & CRYPTO_LOCK) {
	Ns_MutexLock((Ns_Mutex *) dynlock);
    } else {
	Ns_MutexUnlock((Ns_Mutex *) dynlock);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DynlockDestroyCallback --
 *
 *      Destroy a dynamically-allocated mutex for OpenSSL.
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
DynlockDestroyCallback(struct CRYPTO_dynlock_value *dynlock,
    const char *file, int line)
{
    Ns_MutexDestroy((Ns_Mutex *) dynlock);
}


/* poll.c
 *
 * COPYRIGHT (c) 1998 Bell Laboratories, Lucent Technologies
 *
 * crude implementation of a polling function
 */

#include <windows.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

#include "win32-fault.h"
#include "winsock.h"


/* FROM posix-os/poll.c */
/* bit masks for polling descriptors (see src/sml-nj/boot/Unix/os-io.sml) */
#define RD_BIT		0x1
#define WR_BIT		0x2
#define ERR_BIT		0x4



/*
 *
 * THIS IS A BIG HACK TO GET UNIFORM SOCKETS AND IO POLLING 
 *
 * The idea is simple: a thread is used to select on sockets,
 * and the main thread selects on IO.
 * Some of tihs code is inspired by the cygwin32 implementation.
 *
 */


/* for polling sockets, select thread */
PVT fd_set	*rfds, *wfds, *efds;
PVT int         sts;
PVT HANDLE start_select;
PVT HANDLE select_done;
PVT SOCKET sock;

PVT HANDLE sth = NULL ;

DWORD WINAPI pollSockets (LPVOID lpvThreadParm) {
  fd_set except,*e;
  while (1) {
    WaitForSingleObject (start_select,INFINITE);
    FD_ZERO (&except);
    if (efds !=NULL)
      e = efds;
    else
      e = &except;
    FD_SET (sock, e);
#ifdef DEBUG
Say ("ST: About to select on sockets\n");  
#endif
    sts = select (0 /* ignored by Winsock */, rfds, wfds, e, NULL);
#ifdef DEBUG
Say ("ST: Done selecting on sockets, sts = %d\n",sts);
#endif
    SetEvent (select_done);
#ifdef DEBUG
Say ("ST: Done\n");
#endif
  }
}
    

/* _ml_win32_OS_poll : word32 list * word32 list * (Int32.int * int) option -> 
                       (word32 list * word32 list)
 *
 * Pass IO descs, Socket descs, and a timeout.
 *
 * MAKE SURE THIS FUNCTION IS GC-SAFE
 */
ml_val_t _ml_win32_OS_poll (ml_state_t *msp, ml_val_t arg)
{
  DWORD dwMilliseconds;
  ml_val_t pollIOList = REC_SEL(arg,0);
  ml_val_t pollSockList = REC_SEL(arg,1);
  ml_val_t timeout = REC_SEL (arg,2);
  int sec,usec;
  ml_val_t l,item;
  ml_val_t r;
  ml_val_t resIOList,resSockList;
  HANDLE handle,*hArray;
  int result;
  int maxFD;
  DWORD dwThreadId;

  int count,index,hcount;   /* count = # of iodescs, hcount = # of handles */

#ifdef DEBUG
Say ("About to convert timeout to milliseconds\n");
#endif

  /* first, convert timeout to milliseconds */
  if (timeout==OPTION_NONE)
    dwMilliseconds = INFINITE;
  else {
    timeout = OPTION_get(timeout);
    sec = REC_SELINT32(timeout,0);
    usec = REC_SELINT(timeout,1);
    dwMilliseconds = (sec*1000)+(usec/1000);
  }

#ifdef DEBUG
Say ("About to check that we are indeed polling something\n");
#endif

  /* check if we are indeed polling something */
  if (pollIOList==LIST_nil && pollSockList==LIST_nil) {
    resIOList = LIST_nil;
    resSockList = LIST_nil;
    goto getout;
  }

  /* setup polling for sockets */
  if (pollSockList != LIST_nil) {
     fd_set	rset, wset, eset;
     int flag;
     int fd;

#ifdef DEBUG
Say ("setting up polling for sockets\n");
#endif

    rfds = wfds = efds = NIL(fd_set *);
    maxFD = 0;
    for (l = pollSockList;  l != LIST_nil;  l = LIST_tl(l)) {
      item	= LIST_hd(l);
      fd	= REC_SELINT(item, 0);
      flag	= REC_SELINT(item, 1);
      if ((flag & RD_BIT) != 0) {
	if (rfds == NIL(fd_set *)) {
	  rfds = &rset;
	  FD_ZERO(rfds);
	}
	FD_SET (fd, rfds);
      }
      if ((flag & WR_BIT) != 0) {
	if (wfds == NIL(fd_set *)) {
	  wfds = &wset;
	  FD_ZERO(wfds);
	}
	FD_SET (fd, wfds);
      }
      if ((flag & ERR_BIT) != 0) {
	if (efds == NIL(fd_set *)) {
	  efds = &eset;
	  FD_ZERO(efds);
	}
	FD_SET (fd, efds);
      }
      if (fd > maxFD) maxFD = fd;
    }

    if (sts==NULL) {
      /* from cygwin CDK select.cc */
#ifdef DEBUG
Say ("Launching socket select thread\n");
#endif
      start_select = CreateEventA (NULL,FALSE,FALSE,NULL);
      select_done = CreateEventA (NULL,TRUE,FALSE,NULL);
      sth = CreateThread (NULL,0,pollSockets, NULL, 0, & dwThreadId);
      CloseHandle (sth);
    }
    sock = socket (PF_INET,SOCK_STREAM,0);
#ifdef DEBUG
Say ("Setting start_select event\n");
#endif
    SetEvent(start_select);
  }

#ifdef DEBUG
Say ("About to count number of handles\n");
#endif

  /* count number of handles */
  for (l=pollIOList,count=0; l!=LIST_nil; l=LIST_tl(l)) 
    count++;
  hcount = count + (pollSockList==LIST_nil? 0 : 1);

#ifdef DEBUG
Say ("Allocating array of handles (%d)\n",hcount);
#endif

  /* allocate array of handles */
  hArray = NEW_VEC (HANDLE,hcount);

#ifdef DEBUG
Say ("About to initialize the array\n");
#endif

  /* initialize the array */
  for (l=pollIOList,index=0; l!=LIST_nil; l=LIST_tl(l)) {
    item = LIST_hd (l);
    handle = (HANDLE) WORD_MLtoC (item);
    hArray[index++] = handle;
  }
  if (pollSockList!=LIST_nil) hArray[count] = select_done;

#ifdef DEBUG
Say ("About to perform a generalized poll, hcount = %d\n",hcount);
#endif

  /* generalized poll to see if anything is available */
  result = WaitForMultipleObjects (hcount,hArray,FALSE,dwMilliseconds);
#ifdef DEBUG
Say ("Generalized poll finished\n");
#endif

  if (result==WAIT_FAILED) {
#ifdef DEBUG
Say ("Wait_failed\n");
#endif

    if (pollSockList != LIST_nil) {
      closesocket (sock);
      WaitForSingleObject (select_done,INFINITE);
      ResetEvent (select_done);
    }
    resIOList = resSockList = LIST_nil;
    goto getout;
  } else if (result==WAIT_TIMEOUT) {
#ifdef DEBUG
Say ("Wait_timeout\n");
#endif
    if (pollSockList != LIST_nil) {
      closesocket (sock);
      WaitForSingleObject (select_done,INFINITE);
      ResetEvent (select_done);
    }
    resIOList = resSockList = LIST_nil;
    goto getout;
  }

#ifdef DEBUG
Say ("At least one handle was ready\n");
#endif

  /* at least one handle was ready. Find all that are */
  /* first check the IO handles */
  for (index = count,resIOList=LIST_nil; index>0; index--) {
    handle = hArray[index-1];
    result = WaitForSingleObject (handle,0);
    if (result==WAIT_FAILED || result==WAIT_TIMEOUT) {
      if (pollSockList != LIST_nil) {
        closesocket (sock);
        WaitForSingleObject (select_done, INFINITE);
        ResetEvent (select_done);
      }
      continue;
    }
    WORD_ALLOC(msp,item,(Word_t)handle);
    LIST_cons (msp,resIOList,item,resIOList);
  }
  
  /* handle Sockets */
  if (pollSockList != LIST_nil) {

#ifdef DEBUG
Say ("Checking if socket was selected\n");
#endif

    /* check if socket thread terminated */
    result = WaitForSingleObject (hArray[count],0);
    closesocket (sock);
    ResetEvent (select_done);   /* cygwin32 bug? */
    if (result!=WAIT_FAILED && result!=WAIT_TIMEOUT) {
      ml_val_t l;
      int fd,flag;
      PVT ml_val_t item;
      ml_val_t	*resVec = NEW_VEC(ml_val_t, sts);
      int		i, resFlag;

#ifdef DEBUG      
Say ("Figuring out what got selected\n");
#endif

      for (i = 0, l = pollSockList;  l != LIST_nil;  l = LIST_tl(l)) {
	item	= LIST_hd(l);
	fd		= REC_SELINT(item, 0);
	flag	= REC_SELINT(item, 1);
	resFlag	= 0;
	if (((flag & RD_BIT) != 0) && FD_ISSET(fd, rfds))
	  resFlag |= RD_BIT;
	if (((flag & WR_BIT) != 0) && FD_ISSET(fd, wfds))
	  resFlag |= WR_BIT;
	if (((flag & ERR_BIT) != 0) && FD_ISSET(fd, efds))
	  resFlag |= ERR_BIT;
	if (resFlag != 0) {
	  REC_ALLOC2 (msp, item, INT_CtoML(fd), INT_CtoML(resFlag));
	  resVec[i++] = item;
	}
      }
      
      ASSERT(i == sts);
      
      for (i = sts-1, l = LIST_nil;  i >= 0;  i--) {
	item = resVec[i];
	LIST_cons (msp, l, item, l);
      }
      
      FREE(resVec);
      
      resSockList = l;
    } else if (sts < 0)
      resSockList = LIST_nil;  /* RAISE_SYSERR (msp,sts) */
    else 
      resSockList = LIST_nil;
  }
  
  FREE(hArray);

  getout : 

#ifdef DEBUG
Say ("Getting out\n");
#endif

  REC_ALLOC2(msp, r, resIOList, resSockList);

  return r;
}    

/* end of poll.c */


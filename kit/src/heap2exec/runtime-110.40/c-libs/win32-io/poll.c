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


/* _ml_win32_OS_poll : word32 list * (Int32.int * int) option -> word32 list
 */
ml_val_t _ml_win32_OS_poll (ml_state_t *msp, ml_val_t arg)
{
  DWORD dwMilliseconds;
  ml_val_t pollList = REC_SEL(arg,0);
  ml_val_t timeout = REC_SEL (arg,1);
  int sec,usec;
  ml_val_t l,item;
  ml_val_t resList;
  HANDLE handle,*hArray;
  int result;

  int count,index;

  /* first, convert timeout to milliseconds */
  if (timeout==OPTION_NONE)
    dwMilliseconds = INFINITE;
  else {
    timeout = OPTION_get(timeout);
    sec = REC_SELINT32(timeout,0);
    usec = REC_SELINT(timeout,1);
    dwMilliseconds = (sec*1000)+(usec/1000);
  }

  /* count number of handles */
  for (l=pollList,count=0; l!=LIST_nil; l=LIST_tl(l)) 
    count++;
  
  /* allocate array of handles */
  hArray = NEW_VEC (HANDLE,count);
  
  /* initialize the array */
  for (l=pollList,index=0; l!=LIST_nil; l=LIST_tl(l)) {
    item = LIST_hd (l);
    handle = (HANDLE) WORD_MLtoC (item);
    hArray[index++] = handle;
  }
    
  /* generalized poll to see if anything is available */
  result = WaitForMultipleObjects (count,hArray,FALSE,dwMilliseconds);
  if (result==WAIT_FAILED) return LIST_nil;
  else if (result==WAIT_TIMEOUT) return LIST_nil;

  /* at least one handle was ready. Find all that are */
  for (index = count,resList=LIST_nil; index>0; index--) {
    handle = hArray[index-1];
    result = WaitForSingleObject (handle,0);
    if (result==WAIT_FAILED || result==WAIT_TIMEOUT) continue;
    WORD_ALLOC(msp,item,(Word_t)handle);
    LIST_cons (msp,resList,item,resList);
  }

  FREE(hArray);
  return resList;
}    

/* end of poll.c */








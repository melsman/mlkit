#!/bin/ksh
#

NETFUNS="\
  _ml_NetDB_ \
  gethostname \
  getnetbyname \
  setnetbyaddr \
  gethostbyname \
  gethostbyaddr \
  getprotbyname \
  getprotbynum \
  getservbyname \
  getservbyport \
  getrpcbyname \
  getrpcbynum \
"

function generate
{
    prefix=$1; shift
    for i in $*
    do
	file=$i.c
	func=$prefix$i
	cat - > $file <<XXXX
/* $file
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-osdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* $func : () -> ()
 */
ml_val_t $func (ml_state_t *msp, ml_val_t arg)
{
    return RaiseSysError (msp, "$func unimplemented");

} /* end of $func */
XXXX
    done
}

#generate $NETFUNS

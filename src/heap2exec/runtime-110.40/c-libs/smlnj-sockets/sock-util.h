/* sock-util.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Utility functions for the network database and socket routines.
 */

#ifndef _SOCK_UTIL_
#define _SOCK_UTIL_

typedef struct hostent *hostent_ptr_t;
typedef struct netent *netent_ptr_t;
typedef struct servent *servent_ptr_t;

extern ml_val_t _util_NetDB_mkhostent (ml_state_t *msp, hostent_ptr_t hentry);
extern ml_val_t _util_NetDB_mknetent (ml_state_t *msp, netent_ptr_t nentry);
extern ml_val_t _util_NetDB_mkservent (ml_state_t *msp, servent_ptr_t sentry);
extern ml_val_t _util_Sock_ControlFlg (ml_state_t *msp, ml_val_t arg, int option);

extern sysconst_tbl_t	_Sock_AddrFamily;
extern sysconst_tbl_t	_Sock_Type;

#endif /* !_SOCK_UTIL_ */


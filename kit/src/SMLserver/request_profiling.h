/*
 * request_profiling.c - Timing execution time for SML requests
 * 2003-09-14, nh
 */

#include "ns.h"

/* This is a HACK!!!

 * The Conn structure contains fields we want to log - however they
 * are hidden in the Ns_Conn struct. We therefore define a new Conn
 * struct here that is basically Ns_Conn with more fields from the
 * Conn structure.
 * This is a big HACK, 2003-09-14, nh
 */
typedef struct RequestConn {

    /*
     * Visible in an Ns_Conn: */
    
    Ns_Request  *request;
    Ns_Set      *headers;
    Ns_Set      *outputheaders;
    char        *authUser;
    char        *authPasswd;
    int          contentLength;
    int          flags;

    /*
     * Visible only in a Conn:
     */
    
    char	*server;
    struct Conn *prevPtr;
    struct Conn *nextPtr;
    int          id;
    time_t	 startTime;

    Ns_Time	 tqueue;
    Ns_Time	 tstart;
    Ns_Time	 tclose;
/*    Driver      *drvPtr;
    void        *drvData;
    Ns_Set      *query;
    char	*peer;
    void	*enc;
    char	 peerBuf[32];
*/
    /*
     * Tcl state for the ns_conn command.
     */
/*
    Tcl_Interp  *interp;
    int		 tclInit;
    char         tclHdrs[20];
    char         tclOutputHdrs[20];
    int     	 tclFormInit;
    char         tclForm[20];
    char         tclConn[20];

    ConnState    readState;
    ConnState    sendState;
    int          nContent;
    int          nContentSent;
    int          responseStatus;
    int          responseLength;
    int          recursionCount;
    int		 keepAlive;*/
} RequestConn;

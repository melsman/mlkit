/*
 * nsprof.c - Profiling connection in AOLserver
 * 
 * This module is based on the stat.c example in the AOLserver distribution.
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
typedef struct ProfConn {

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
} ProfConn;

typedef struct {
  char *hServer;
} ProfContext;

/*
 * Forward declarations of functions which will be referenced
 * in the stat module initialization function.
 */
static Ns_TraceProc ProfTrace;
static Ns_Callback ProfShutdown;

/*
 * The Ns_ModuleVersion exported integer is used to verify
 * this module version when loaded.  For AOLserver 3.0,
 * 1 (one) is the only valid value for this variable.
 */
int Ns_ModuleVersion = 1;

/*
 * The Ns_ModuleInit function is the function the AOLserver
 * will call each time the module is loaded into a 
 * server.  The function is passed two parameters:
 *
 * hServer:   The server 'handle' as a string. This is the
 *            short name given to the virutal server such
 *            as 'server1'.
 *
 * hModule:   The module 'handle' as a string. This is the
 *            short name given to the module such as 'stat'
 *
 * For example, if this module is known as 'stat' and loaded
 * into the 'server1' server with entries similar to the following
 * in the nsd.ini file:
 *
 * [ns/servers]
 * server1=My First Server
 *
 * [ns/server1/modules]
 * stat=stat.dll
 *
 * This function would be called with "server1" and "stat" as
 * its arguments.
 *
 */
int
Ns_ModuleInit(char *hServer, char *hModule)
{
  ProfContext *ctx;

  /*
   * Create and initalize the profiling context.
   */
  ctx = Ns_Malloc(sizeof(ProfContext));
  ctx->hServer = hServer;

  /*
   * Register the trace to accumulate the profiling data.
   */
  Ns_RegisterServerTrace(hServer, ProfTrace, ctx);

  /*
   * Register the profiling shutdown procedure which
   * cleans up the context on server shutdown.
   */
  Ns_RegisterServerShutdown(hServer, ProfShutdown, ctx);

  return NS_OK;
}

/*
 * ProfTrace is called after each connection and accumulates the
 * profiling data.
 * Remember to set
 *   ns_param globalstats true 
 * in the section 
 *  ns_section "ns/server/${user}"
 * Otherwise tqueue, tstart and tclose are empty
 */
static void
ProfTrace(void *ctx, Ns_Conn *conn)
{
  ProfConn *c;

  c=(ProfConn*)conn;

  Ns_Log(Notice,"NsProf[%d,%d] tqueue[%d,%d] tstart[%d,%d] tclose[%d,%d] url[%s] "
                "referer[%s,%d] location[%s]",
	 c->id,Ns_InfoBootTime(),
	 c->tqueue.sec,c->tqueue.usec,
	 c->tstart.sec,c->tstart.usec,
	 c->tclose.sec,c->tclose.usec,
	 c->request->url,
	 Ns_ConnPeer(conn),
	 Ns_ConnPeerPort(conn),
	 Ns_ConnLocation(conn));
}

/* 
 * ProfShutdown simple cleans up the ProfContext structure.
 */
static void
ProfShutdown(void *ctx)
{
  Ns_Free(ctx);
}


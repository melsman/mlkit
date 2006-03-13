#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "oci.h"
#include "../../Runtime/List.h"
#include "../../Runtime/String.h"
#include "DbCommon.h"

#define MAXMSG 1024

enum DBReturn
{
  DBError = 0, 
  DBData = 1, 
  DBDml = 2,
  DBEod = 3
};

typedef struct 
{
  OCIEnv *envhp;
  OCIError *errhp;
  OCISPool *poolhp;
  OraText *poolName;
  ub4 poolNameLength;
  int dbid;
  unsigned char msg[MAXMSG];
  void *freeSessionsGlobal;
  unsigned int number_of_sessions;
  unsigned char about_to_shutdown; // != 0 if we are shutting this environment down
} oDb_t;

typedef struct oSes
{
  struct oSes *next;
  struct oSes *prev;
  OCISvcCtx *svchp;
  OCIError *errhp;
  OCIStmt *stmthp;
  oDb_t *db;
  ub4 mode;
  int *datasizes;
  unsigned char *rowp;
  unsigned char msg[MAXMSG];
} oSes_t;

typedef struct
{
  int lazyInit;
  char *TNSname;
  char *username;
  char *password;
//  proc_lock plock;
//  char *plockname;
  thread_lock tlock;
  cond_var cvar;
//  unsigned long *totalNsessions;
//  unsigned long limit;
  int maxdepth;
  int maxsessions;
  int minsessions;
  oDb_t *dbspec;
} db_conf;

typedef struct
{
  void *dbSessions;
  void *freeSessions;
  int theOne;
  int depth;
} dbOraData;

#ifdef MAX
#undef MAX
#endif
#define MAX(a,b) ((a) < (b) ? (b) : (a))

#define ErrorCheck(status,type,dbmsg,code,rd) {                                      \
  if (status != OCI_SUCCESS)                                                         \
  {                                                                                  \
    if (putmsg(db, status, &errcode, type, dbmsg->msg, MAXMSG,   \
               dbmsg->errhp, rd)!=OCI_SUCCESS) \
    {                                                                                \
      code                                                                           \
    }                                                                                \
  }                                                                                  \
}

static sword
putmsg(oDb_t *db, sword status, sb4 *errcode, ub4 t, unsigned char *msg, int msgLength, 
       OCIError *errhp, void *ctx)/*{{{*/
{
  if (t == OCI_HTYPE_ENV)
  {
    if (status == OCI_SUCCESS_WITH_INFO) return OCI_SUCCESS;
    return status;
  }
  else 
  { // t == OCI_HTYPE_ERROR
    switch (status)
    {
      case OCI_SUCCESS:
        msg[0] = 0;
        return OCI_SUCCESS;
        break;
      case OCI_SUCCESS_WITH_INFO:
        OCIErrorGet(errhp, (ub4) 1, (text *) NULL, errcode, msg, msgLength, t);
        msg[msgLength-1] = 0;
        dblog1(ctx, (char *) msg);
        return OCI_SUCCESS;
        break;
      case OCI_ERROR:
      case OCI_INVALID_HANDLE:
        OCIErrorGet(errhp, (ub4) 1, (text *) NULL, errcode, msg, msgLength, t);
        msg[msgLength-1] = 0;
        dblog1(ctx, (char *) msg);
        return OCI_ERROR;
        break;
      default:
        msg[0] = 0;
        return status;
        break;
    }
  }
  return OCI_ERROR;
}/*}}}*/

static ub1 spool_getmode = OCI_SPOOL_ATTRVAL_FORCEGET;

static oDb_t * 
DBinitConn (void *ctx, char *TNSname, char *userid, char *password, int min, int max, int dbid)/*{{{*/
{
  sb4 errcode = 0;
  OCIEnv *envhp;
  sword status;
  oDb_t *db;
  status = OCIEnvCreate(&envhp, OCI_THREADED | OCI_NEW_LENGTH_SEMANTICS, 
                        (dvoid *) 0, 0, 0, 0, sizeof(oDb_t), (dvoid **) &db);
  if (!db)
  {
    dblog1(ctx, "DataBase init failed; oracle environment could not be created");
    return NULL;
  }
  ErrorCheck(status, OCI_HTYPE_ENV, db, 
      dblog1(ctx, "DataBase init failed; are you sure you have set ORACLE_HOME in your environment");
      return NULL;,
      ctx
      )
//  dblog1(ctx, "dbinit2");
  db->dbid = dbid;
  db->freeSessionsGlobal = NULL;
  db->envhp = envhp;
  db->errhp = NULL;
  db->number_of_sessions = 0;
  db->about_to_shutdown = 0;
  db->msg[0] = 0;
  status = OCIHandleAlloc ((dvoid *) db->envhp, (dvoid **) &(db->poolhp), 
      OCI_HTYPE_SPOOL, (size_t) 0, (dvoid **) 0);
  ErrorCheck(status, OCI_HTYPE_ENV, db,
      OCIHandleFree(db->envhp, OCI_HTYPE_ENV);
      dblog1(ctx, "oracleDB: DataBase init failed; are we out of memory?");
      return NULL;,
      ctx
      )
//  dblog1(ctx, "dbinit3");
  status = OCIHandleAlloc ((dvoid *) db->envhp, (dvoid **) &(db->errhp), 
      OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0);
  ErrorCheck(status, OCI_HTYPE_ENV, db,
      OCIHandleFree(db->poolhp, OCI_HTYPE_SPOOL);
      OCIHandleFree(db->envhp, OCI_HTYPE_ENV);
      dblog1(ctx, "oracleDB: DataBase init failed; are we out of memory?");
      return NULL;,
      ctx
      )
  status = OCISessionPoolCreate(db->envhp, db->errhp, db->poolhp, (OraText **) &(db->poolName), 
      &(db->poolNameLength), (CONST OraText *) TNSname, (ub4) strlen(TNSname),
      (ub4) min, (ub4) max, (ub4) 1, (OraText *) userid, (ub4) strlen(userid), 
      (OraText *) password, (ub4) strlen(password), OCI_SPC_STMTCACHE | OCI_SPC_HOMOGENEOUS);
  ErrorCheck(status, OCI_HTYPE_ERROR, db,
      OCIHandleFree(db->poolhp, OCI_HTYPE_SPOOL);
      OCIHandleFree(db->errhp, OCI_HTYPE_ERROR);
      OCIHandleFree(db->envhp, OCI_HTYPE_ENV);
      return NULL;,
      ctx
      )
  status = OCIAttrSet((dvoid *) db->poolhp, OCI_HTYPE_SPOOL, &spool_getmode, 0, 
                       OCI_ATTR_SPOOL_GETMODE, db->errhp);
  ErrorCheck(status, OCI_HTYPE_ERROR, db,
      OCIHandleFree(db->poolhp, OCI_HTYPE_SPOOL);
      OCIHandleFree(db->errhp, OCI_HTYPE_ERROR);
      OCIHandleFree(db->envhp, OCI_HTYPE_ENV);
      return NULL;,
      ctx
      )
  return db;
}/*}}}*/

static void
DBCheckNSetIfServerGoneBad(oDb_t *db, sb4 errcode, void *ctx, int lock)/*{{{*/
{
  db_conf *dbc;
  switch (errcode)
  {
    case 28: // your session has been killed
    case 1012: // not logged on
    case 1041: // internal error. hostdef extension doesn't exist
    case 3113: // end-of-file on communication channel
    case 3114: // not connected to ORACLE
    case 12571: // TNS:packet writer failure
    case 24324: // service handle not initialized
      dblog1(ctx, "Database gone bad. Oracle environment about to shutdown");
      dbc = (db_conf *) apsmlGetDBData(db->dbid,ctx);
      if (!dbc) return;
      if (lock) lock_thread(dbc->tlock);
      if (db == dbc->dbspec) dbc->dbspec = NULL;
      db->about_to_shutdown = 1;
      if (lock) unlock_thread(dbc->tlock);
      return;
      break;
    default:
      return;
      break;
  }
  return;
}/*}}}*/

static void 
DBShutDown(oDb_t *db, void *ctx)/*{{{*/
{
  sb4 errcode = 0;
  sword status;
  if (!db) return;
  status = OCISessionPoolDestroy(db->poolhp, db->errhp, OCI_SPD_FORCE);
  ErrorCheck(status, OCI_HTYPE_ERROR, db,
      dblog1(ctx, "Closing down the session pool gave an error, but I am still shutting down");,
      ctx
      )
  status = OCIHandleFree(db->poolhp, OCI_HTYPE_SPOOL);
  status = OCIHandleFree(db->errhp, OCI_HTYPE_ERROR);
  status = OCIHandleFree(db->envhp, OCI_HTYPE_ENV);
  return;
}/*}}}*/

static void
DBShutDownWconf(void *db2, void *ctx)/*{{{*/
{
  oDb_t *db;
  db_conf *db1 = (db_conf *) db2;
  if (!db1 || !(db1->dbspec)) return;
  db = db1->dbspec;
  db1->dbspec = NULL;
  DBShutDown(db, ctx);
  return;
}/*}}}*/

static oSes_t *
DBgetSession (oDb_t *db, void *rd)/*{{{*/
{
  sb4 errcode = 0;
  sword status;
  OCIError *errhp;
  oSes_t *ses;
  if (db == NULL) return NULL;
  status = OCIHandleAlloc ((dvoid *) db->envhp, (dvoid **) &errhp, 
      OCI_HTYPE_ERROR, sizeof(oSes_t), (dvoid **) &ses); // allocating ses
  ErrorCheck(status, OCI_HTYPE_ENV, db,
      dblog1(rd, "oracleDB: DataBase alloc failed; are we out of memory?");
      return NULL;,
      rd
      )
  ses->errhp = errhp;
  ses->db = db;
  ses->mode = OCI_COMMIT_ON_SUCCESS;
  ses->stmthp = NULL;
  ses->datasizes = NULL;
  status = OCISessionGet(db->envhp, ses->errhp, &(ses->svchp), NULL, db->poolName, 
      db->poolNameLength, NULL, 0, NULL, NULL, NULL, OCI_SESSGET_SPOOL);
  ErrorCheck(status, OCI_HTYPE_ERROR, db,
      DBCheckNSetIfServerGoneBad(db, status, rd, 0);
      OCIHandleFree ((dvoid *) errhp, OCI_HTYPE_ERROR);
      return NULL;,
      rd
      )
  db->number_of_sessions++;
  return ses;
}/*}}}*/

static void
DBFlushStmt (oSes_t *ses, void *ctx)/*{{{*/
{
  sword status;
  sb4 errcode = 0;
  dvoid *db;
  if (ses == NULL) return;
  db = ses->db;
//  if (ses->mode == OCI_DEFAULT)
//  {
//    ses->mode = OCI_COMMIT_ON_SUCCESS;
//    status = OCITransRollback(ses->svchp, ses->errhp, OCI_DEFAULT);
//    ErrorCheck(status, OCI_HTYPE_ERROR, ses, ;, ctx)
//  }
  if (ses->datasizes)
  {
    free(ses->datasizes);
    ses->datasizes = NULL;
  }
  if (ses->rowp)
  {
    free(ses->rowp);
    ses->rowp = NULL;
  }
  if (ses->stmthp != NULL)
  {
    status = OCIStmtRelease(ses->stmthp, ses->errhp, NULL, 0, OCI_STRLS_CACHE_DELETE);
    ses->stmthp = NULL;
  }
  return;
}/*}}}*/

enum DBReturn
DBORAExecuteSQL (oSes_t *ses, char *sql, void *ctx)/*{{{*/
{
  sb4 errcode = 0;
  sword status;
  ub4 type = 0;
  int count;
  dvoid *db;
  db = ses->db;
  status = OCIStmtPrepare2 (ses->svchp, &(ses->stmthp), ses->errhp, (CONST OraText *) sql, 
                            (ub4) strlen(sql), NULL, 0, OCI_NTV_SYNTAX, OCI_DEFAULT);
  ErrorCheck(status, OCI_HTYPE_ERROR, ses,
      DBCheckNSetIfServerGoneBad(ses->db, status, ctx, 1);
      return DBError;,
      ctx
      )
  status = OCIAttrGet((dvoid *) ses->stmthp, OCI_HTYPE_STMT, (dvoid *) &type, 
             NULL, OCI_ATTR_STMT_TYPE, ses->errhp);
  ErrorCheck(status, OCI_HTYPE_ERROR, ses,
      DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
      status = OCIStmtRelease (ses->stmthp, ses->errhp, NULL, 0, OCI_STRLS_CACHE_DELETE);
      return DBError;,
      ctx
      )
  switch (type) 
  {
    case OCI_STMT_SELECT:
      count = 0;
      break;
    default:
      count = 1;
      break;
  }
  status = OCIStmtExecute(ses->svchp, ses->stmthp, ses->errhp, count, 0, NULL, NULL, ses->mode);
  ErrorCheck(status, OCI_HTYPE_ERROR, ses,
      DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
      DBFlushStmt(ses, ctx);
      return DBError;,
      ctx
      )
  if (type == OCI_STMT_SELECT) return DBData;
  status = OCIStmtRelease(ses->stmthp, ses->errhp, NULL, 0, OCI_DEFAULT);
  ses->stmthp = NULL;
  ErrorCheck(status, OCI_HTYPE_ERROR, ses, 
      DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
      return DBError;, 
      ctx)
  return DBDml;
}/*}}}*/

static void *
DBGetColumnInfo (oSes_t *ses, void *dump(void *, int, int, unsigned char *), void **columnCtx, 
                 void *ctx)/*{{{*/
{
  sb4 errcode = 0;
  ub4 n, i;
  OCIParam *colhd = (OCIParam *) NULL;
  dvoid *db;
  sword status;
  ub2 type;
  sb4 coldatasize = 0;
  unsigned char *colname;
  ub4 colnamelength;
  int *datasizes;
  db = ses->db;
  if (ses->stmthp == NULL) return NULL;
  status = OCIAttrGet((dvoid *) ses->stmthp, OCI_HTYPE_STMT, (dvoid *) &n, 
                        0, OCI_ATTR_PARAM_COUNT, ses->errhp);
  ErrorCheck(status, OCI_HTYPE_ERROR, ses,
      DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
      DBFlushStmt(ses,ctx);
      return NULL;,
      ctx
      )
  ses->datasizes = (int *) malloc((n+1) * sizeof (int));
  
  if (ses->datasizes == NULL) return NULL;
  datasizes = ses->datasizes;
  datasizes[0] = n;
  for (i=1; i <= n; i++)
  {
    // Get column data
    status = OCIParamGet((dvoid *) ses->stmthp, OCI_HTYPE_STMT, ses->errhp, (dvoid **) &colhd, i);
    ErrorCheck(status, OCI_HTYPE_ERROR, ses,
        DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
        DBFlushStmt(ses,ctx);
        return NULL;,
        ctx
        )
    // Get column name
    status = OCIAttrGet ((dvoid *) colhd, OCI_DTYPE_PARAM, (dvoid *) &colname, &colnamelength, 
                         OCI_ATTR_NAME, ses->errhp);
    ErrorCheck(status, OCI_HTYPE_ERROR, ses,
        DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
        DBFlushStmt(ses,ctx);
        return NULL;,
        ctx
        )
    *columnCtx = dump(*columnCtx, i, (int) colnamelength, colname);
    // Get column type
    status = OCIAttrGet(colhd, OCI_DTYPE_PARAM, &type, NULL, OCI_ATTR_DATA_TYPE, ses->errhp);
    ErrorCheck(status, OCI_HTYPE_ERROR, ses, 
        DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
        DBFlushStmt(ses,ctx);
        return NULL;,
        ctx
        )
    // Get size of data
    status = OCIAttrGet(colhd, OCI_DTYPE_PARAM, &coldatasize, NULL, OCI_ATTR_DATA_SIZE, ses->errhp);
    ErrorCheck(status, OCI_HTYPE_ERROR, ses, 
        DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
        DBFlushStmt(ses,ctx);
        return NULL;,
        ctx
        )
    // correct size such that extraction as strings is possible without overflow
    switch (type)
    {
      case SQLT_NUM:
        // an over estimete of log_10(256^size) = size * log_10(256)
        coldatasize = (coldatasize * 5) / 2 + 1 + sizeof(sb2);
        break;
      case SQLT_DAT:
        coldatasize = 28;
        break;
      default:
        coldatasize = coldatasize + 1 + sizeof(sb2);
        break;
    }
    datasizes[i] = coldatasize;
  }
  return *columnCtx;
}/*}}}*/

static enum DBReturn
DBGetRow (oSes_t *ses, void *dump(void *, int, unsigned char *), void **rowCtx, void *ctx)/*{{{*/
{
  sb4 errcode = 0;
  ub4 i, n;
  sword status;
  int size = 0;
  dvoid *db;
  OCIDefine *defnpp = NULL;
  db = ses->db;
  if (ses->stmthp == NULL) return DBEod;
  n = ses->datasizes[0];
  if (!ses->rowp) 
  {
    for (i=1; i <= n; i++) size += ses->datasizes[i];
    ses->rowp = (unsigned char *) malloc(size);
    if (!ses->rowp)
    {
      DBFlushStmt(ses, ctx);
      return DBError;
    }
    for (i=1, size = 0; i <= n; i++)
    {
      status = OCIDefineByPos(ses->stmthp, &defnpp, ses->errhp, i, ses->rowp+size+sizeof(sb2), 
                              ses->datasizes[i] - sizeof(sb2), 
                              SQLT_STR, (dvoid *) (ses->rowp+size), NULL, NULL, OCI_DEFAULT);
      ErrorCheck(status, OCI_HTYPE_ERROR, ses,
          DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
          DBFlushStmt(ses,ctx);
          return DBError;,
          ctx
          )
      size += ses->datasizes[i];
    }
  }
  status = OCIStmtFetch2(ses->stmthp, ses->errhp, (ub4) 1, OCI_FETCH_NEXT, 0, OCI_DEFAULT);
  if (status == OCI_NO_DATA)
  {
    DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
    DBFlushStmt(ses,ctx);
    return DBEod;
  }
  ErrorCheck(status, OCI_HTYPE_ERROR, ses,
        DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
        DBFlushStmt(ses,ctx);
        return DBError;,
        ctx
        )
  for (i=1, size = 0; i < n; i++) size += ses->datasizes[i];
  for (i=n; i > 0; i--)
  {
//    dblog1(ses->db->ctx, "111");
    *rowCtx = dump(*rowCtx, i, ses->rowp+size);
    size -= ses->datasizes[i-1];
  }
  return DBData;
}/*}}}*/

enum DBReturn 
DBORATransStart (oSes_t *ses)/*{{{*/
{
  if (ses == NULL || ses->mode == OCI_DEFAULT) return DBError;
  ses->mode = OCI_DEFAULT;
  return DBDml;
}/*}}}*/

enum DBReturn
DBORATransCommit (oSes_t *ses, void *ctx)/*{{{*/
{
  sword status;
  sb4 errcode = 0;
  dvoid *db;
  if (ses == NULL)
  {
    dblog1(ctx, "Oracle Driver: DBORATransCommit, ses == NULL");
    return DBError;
  }
  db = ses->db;
  if (ses->mode == OCI_COMMIT_ON_SUCCESS) 
  {
    dblog1(ctx, "Oracle Driver: DBORATransCommit, ses->mode == OCI_COMMIT_ON_SUCCESS");
    DBFlushStmt(ses,ctx);
    return DBError;
  }
  ses->mode = OCI_COMMIT_ON_SUCCESS;
  status = OCITransCommit(ses->svchp, ses->errhp, OCI_DEFAULT);
  ErrorCheck(status, OCI_HTYPE_ERROR, ses,
      DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
      DBFlushStmt(ses,ctx);
      dblog1(ctx, "Oracle Driver: DBORATransCommit, OCITransCommit: some error occured");
      return DBError;,
      ctx
      )
  return DBDml;
}/*}}}*/

enum DBReturn 
DBORATransRollBack(oSes_t *ses, void *ctx)/*{{{*/
{
  sb4 errcode = 0;
  sword status;
  dvoid *db;
  if (ses == NULL) return DBError;
  db = ses->db;
  if (ses->mode == OCI_COMMIT_ON_SUCCESS) 
  {
    DBFlushStmt(ses,ctx);
    return DBError;
  }
  ses->mode = OCI_COMMIT_ON_SUCCESS;
  status = OCITransRollback(ses->svchp, ses->errhp, OCI_DEFAULT);
  ErrorCheck(status, OCI_HTYPE_ERROR, ses,
      DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 1);
      DBFlushStmt(ses,ctx);
      return DBError;,
      ctx
      )
  return DBDml;
}/*}}}*/

static enum DBReturn
DBReturnSession (oSes_t *ses, void *ctx)/*{{{*/
{
  sword status;
  sb4 errcode = 0;
  dvoid *db;
  OCIError *errhp;
  unsigned char should_we_shutdown;
  unsigned int number_of_sessions;
  ub4 type = 0;
  if (ses == NULL) return DBError;
  db = ses->db;
  if (ses->mode == OCI_DEFAULT)
  { // A transaction is open
//    DBFlushStmt(ses,ctx);
    dblog1(ctx, "Oracle Driver: DBReturnSession, Session in the midle of a Transaction");
    DBORATransRollBack(ses, ctx);
  }
  if (ses->stmthp)
  {
    status = OCIAttrGet((dvoid *) ses->stmthp, OCI_HTYPE_STMT, (dvoid *) &type, 
                   NULL, OCI_ATTR_STMT_STATE, ses->errhp);
    switch (type)
    {
      case OCI_STMT_STATE_INITIALIZED:
        status = OCIStmtRelease(ses->stmthp, ses->errhp, NULL, 0, OCI_STRLS_CACHE_DELETE);
        break;
      case OCI_STMT_STATE_EXECUTED:
        status = OCIStmtRelease(ses->stmthp, ses->errhp, NULL, 0, OCI_STRLS_CACHE_DELETE);
        break;
      case OCI_STMT_STATE_END_OF_FETCH:
        status = OCIStmtRelease(ses->stmthp, ses->errhp, NULL, 0, OCI_DEFAULT);
        break;
      default:
        status = OCIStmtRelease(ses->stmthp, ses->errhp, NULL, 0, OCI_STRLS_CACHE_DELETE);
        break;
    }
    ses->stmthp = NULL;
    DBFlushStmt(ses,ctx);
  }
  errhp = ses->errhp;
  status = OCISessionRelease(ses->svchp, errhp, NULL, 0, OCI_DEFAULT);
  ErrorCheck(status, OCI_HTYPE_ERROR, ses, 
      DBCheckNSetIfServerGoneBad(ses->db, errcode, ctx, 0);,
      ctx)
  ses->db->number_of_sessions--;
  should_we_shutdown = ses->db->about_to_shutdown;
  number_of_sessions = ses->db->number_of_sessions;
  status = OCIHandleFree(errhp, OCI_HTYPE_ERROR); // freeing ses
  if (should_we_shutdown && number_of_sessions == 0)
  {
    DBShutDown((oDb_t *) db, ctx);
  }
  return DBEod;
}/*}}}*/

enum DBReturn
apsmlORADropSession(oSes_t *ses, void *rd)/*{{{*/
{
  dbOraData *dbdata;
  db_conf *dbc;
  oSes_t *tmpses, *rses;
  int dbid;
  oDb_t *db;
  if (ses == NULL || rd == NULL) return DBError;
  dbid = ses->db->dbid;
  db = ses->db;
  dbdata = (dbOraData *) getDbData(dbid, rd);
  if (dbdata == NULL) return DBError;
  if (dbdata->dbSessions == ses)
  {
    dbdata->dbSessions = ses->next;
  }
  else
  {
    rses = (oSes_t *) dbdata->dbSessions;
    tmpses = rses;
    while (tmpses != NULL)
    {
      if (tmpses == ses)
      {
        rses->next = tmpses->next;
        break;
      }
      rses = tmpses;
      tmpses = tmpses->next;
    }
  }
  dbdata->depth--;
  dbc = (db_conf *) apsmlGetDBData(dbid, rd);
  lock_thread(dbc->tlock);
  if (dbdata->theOne)
  {
    if (db->number_of_sessions < dbc->maxsessions - dbc->maxdepth)
    {
      dbdata->theOne = 0;
      ses->next = dbdata->freeSessions;
      dbdata->freeSessions = ses;
      while ((ses = dbdata->freeSessions))
      {
        dbdata->freeSessions = ses->next;
        DBReturnSession(ses, rd);
      }
      while ((ses = db->freeSessionsGlobal))
      {
        db->freeSessionsGlobal = ses->next;
        DBReturnSession(ses,rd);
      }
      broadcast_cond(dbc->cvar);
    }
    else 
    {
      if (!dbdata->dbSessions)
      {
        dbdata->theOne = 0;
        ses->next = dbdata->freeSessions;
        dbdata->freeSessions = NULL;
        while ((rses = db->freeSessionsGlobal))
        {
          db->freeSessionsGlobal = ((oSes_t *)db->freeSessionsGlobal)->next;
          DBReturnSession(rses, rd);
        }
        db->freeSessionsGlobal = ses;
        broadcast_cond(dbc->cvar);
      }
      else
      {
        ses->next = dbdata->freeSessions;
        dbdata->freeSessions = ses;
      }
    }
  }
  else 
  {
    DBReturnSession(ses,rd);
    while ((ses = dbdata->freeSessions))
    {
      dbdata->freeSessions = ses->next;
      DBReturnSession(ses, rd);
    }
    broadcast_cond(dbc->cvar);
  }
  unlock_thread(dbc->tlock);
  if (dbdata->dbSessions == NULL) 
  {
    removeDbData(dbid, rd);
    free(dbdata);
  }
  return DBEod;
}/*}}}*/

oSes_t *
apsmlORAGetSession(int dbid, void *rd)/*{{{*/
{
  oSes_t *ses;
  oDb_t *db;
  db_conf *dbc;
  int i;
  dbOraData *dbdata = (dbOraData *) getDbData(dbid, rd);
  if (!dbdata) 
  {
    dbdata = (dbOraData *) malloc(sizeof (dbOraData));
    if (!dbdata) return NULL;
    dbdata->freeSessions = NULL;
    dbdata->dbSessions = NULL;
    dbdata->theOne = 0;
    dbdata->depth = 0;
    if (putDbData(dbid, dbdata, rd)) 
    {
      free(dbdata);
      return NULL;
    }
  }
//  if (dbdata->theOne && dbdata->freeSessions == NULL)
//  {
//    dblog1(rd, "Depth boundary exceeded");
//    return NULL;
//  }
//  dblog1(rd, "1");
  if (dbdata->freeSessions)
  {
    dbdata->depth++;
    ses = dbdata->freeSessions;
    dbdata->freeSessions = ses->next;
    return ses;
  }
//  dblog1(rd, "2");
  dbc = (db_conf *) apsmlGetDBData(dbid,rd);
  if (dbc == NULL)
  {
    dblog1(rd, "Database not configred");
    return NULL;
  }
  if (dbdata->depth >= dbc->maxdepth) 
  {
    return (oSes_t *) 1;
  }
  lock_thread(dbc->tlock);
  if (!dbc->dbspec)
  {
    if (!dbc->TNSname || !dbc->username || !dbc->password || 
         dbc->maxdepth < 1 || dbc->minsessions < 1 || dbc->maxsessions < 1)
    {
      unlock_thread(dbc->tlock);
      dblog1(rd, 
           "One or more of TNSname, UserName, PassWord, SessionMaxDepth,  not set");
      return NULL;
    }
    dblog1(rd, "Initializing database connection");
    dbc->dbspec = DBinitConn(rd, dbc->TNSname, dbc->username, 
                                    dbc->password, dbc->minsessions, dbc->maxsessions, dbid);
//    dblog1(rd, "Database initialization call done");
  }
//  dblog1(rd, "3");
  if (!dbc->dbspec)
  {
    unlock_thread(dbc->tlock);
    dblog1(rd, "Database did not start");
    return NULL;
  }
//  dblog1(rd, "4");
  db = dbc->dbspec;
  if (db->number_of_sessions < dbc->maxsessions - dbc->maxdepth)
  {
    ses = DBgetSession(dbc->dbspec, rd);
    if (ses == NULL)
    {
      dblog1(rd, "Could not get session");
      unlock_thread(dbc->tlock);
      return NULL;
    }
    dbdata->depth++;
    unlock_thread(dbc->tlock);
    return ses;
  }
  else
  {
    if (db->freeSessionsGlobal)
    {
      dbdata->theOne = 1;
      ses = db->freeSessionsGlobal;
      dbdata->freeSessions = ses->next;
      db->freeSessionsGlobal = NULL;
      unlock_thread(dbc->tlock);
      dbdata->depth++;
      return ses;
    }
    else
    {
      if (db->number_of_sessions == dbc->maxsessions - dbc->maxdepth)
      {
        dbdata->theOne = 1;
        for (i = 0; i < dbc->maxdepth; i++)
        {
          ses = DBgetSession(dbc->dbspec, rd);
          if (ses == NULL) 
          {
            dblog1(rd, "Could not get session");
            while ((ses = dbdata->freeSessions))
            {
              dbdata->freeSessions = ses->next;
              DBReturnSession(ses, rd);
            }
            dbdata->theOne = 0;
            unlock_thread(dbc->tlock);
            return NULL;
          }
          ses->next = dbdata->freeSessions;
          dbdata->freeSessions = ses;
        }
        dbdata->freeSessions = ses->next;
        unlock_thread(dbc->tlock);
        dbdata->depth++;
        return ses;
      }
      else
      {
        ses = DBgetSession(dbc->dbspec,rd);
        if (ses == NULL)
        {
          dblog1(rd, "Could not get session");
          wait_cond(dbc->cvar);
          unlock_thread(dbc->tlock);
          return apsmlORAGetSession(dbid, rd);
        }
        unlock_thread(dbc->tlock);
        dbdata->depth++;
        return ses;
      }
    }
  }
  dblog1(rd, "Oracle driver: End of apsmlORAGetSession reached. This is not suppose to happend");
  unlock_thread(dbc->tlock);
  return NULL;
}/*}}}*/

static void
apsmlDbCleanUpReq(void *rd, void *dbdata1)/*{{{*/
{
  oSes_t *ses;
  dbOraData *dbdata = (dbOraData *) dbdata1;
  if (rd == NULL || dbdata == NULL) return;
  while ((ses = dbdata->dbSessions))
  {
    dbdata->theOne = 0;
    apsmlORADropSession(ses, rd);
  }
  return;
}/*}}}*/

static void 
apsmlORAChildInit(void *c1, int num, void *pool, void *server)
{
  return;
}

int 
apsmlORASetVal (int i, void *rd, int pos, void *val)/*{{{*/
{
  int id;
  char *sd, *target;
  db_conf *cd;
  dblog1(rd, "apsmlORASetVal");
  cd = (db_conf *) apsmlGetDBData (i,rd);
  if (cd == NULL) 
  {
    cd = (db_conf *) malloc (sizeof (db_conf));
    if (!cd) return 2;
    cd->lazyInit = 1;
    cd->username = NULL;
    cd->password = NULL;
    cd->TNSname = NULL;
    cd->maxdepth = 0;
    cd->maxsessions = 0;
    cd->minsessions = 0;
    cd->dbspec = NULL;
    if (create_thread_lock(&(cd->tlock), rd))
    {
      free(cd);
      return 2;
    }
    if (create_cond_variable(&(cd->cvar), cd->tlock, rd))
    {
      destroy_thread_lock(cd->tlock);
      free(cd);
      return 2;
    }
    if (apsmlPutDBData (i,(void *) cd, apsmlORAChildInit, DBShutDownWconf, apsmlDbCleanUpReq, rd))
    {
      destroy_thread_lock(cd->tlock);
      free(cd);
      return 2;
    }
    cd = (db_conf *) apsmlGetDBData (i,rd);
  }
  switch (pos)
  {
    case 1:
      id = (int) val;
      cd->lazyInit = id;
      break;
    case 5:
      id = (int) val;
      cd->maxdepth = id;
      if (cd->maxsessions && cd->maxsessions < cd->maxdepth) return 3;
      break;
    case 6:
      id = (int) val;
      cd->minsessions = id;
      break;
    case 7:
      id = (int) val;
      cd->maxsessions = id;
      if (cd->maxdepth && cd->maxsessions < cd->maxdepth) return 3;
      break;
    case 2:
    case 3:
    case 4:
      sd = (char *) val;
      target = (char *) malloc (strlen (sd)+1);
      if (!target) return 2;
      strcpy(target, sd);
      switch (pos)
      {
        case 2:
          if (cd->username) free(cd->username);
          cd->username = target;
          break;
        case 3:
          if (cd->password) free(cd->password);
          cd->password = target;
          break;
        case 4:
          if (cd->TNSname) free(cd->TNSname);
          cd->TNSname = target;
          break;
      }
      break;
    default:
      return 1;
      break;
  }
  return 0;
}/*}}}*/


typedef struct
{
  Region rListAddr;
  Region rStringAddr;
  Region rAddrEPairs;
  int *list;
} cNames_t;

static void *
dumpCNames (void *ctx1, int pos, int length, unsigned char *data)/*{{{*/
{
  String rs;
  int *pair;
  cNames_t *ctx = (cNames_t *) ctx1;
  rs = convertBinStringToML(ctx->rStringAddr, length, data);
  allocRecordML(ctx->rListAddr, 2, pair);
  first(pair) = (int) rs;
  second(pair) = (int) ctx->list;
  makeCONS(pair, ctx->list);
  return ctx;
}/*}}}*/

int
apsmlORAGetCNames(Region rListAddr, Region rStringAddr, oSes_t *ses, void *rd)/*{{{*/
{
  cNames_t cn1;
  cNames_t *cn = &cn1;
  cn->rListAddr = rListAddr;
  cn->rStringAddr = rStringAddr;
  cn->rAddrEPairs = NULL;
  makeNIL(cn->list);
  if (DBGetColumnInfo(ses, dumpCNames, (void **) &cn, rd) == NULL)
  {
    raise_overflow();
    return (int) cn1.list;
  }
  return (int) cn1.list;
}/*}}}*/

static void *
dumpRows(void *ctx1, int pos, unsigned char *data)/*{{{*/
{
  String rs;
  sb2 *ivarp;
  int *pair, ivar, *pair2;
  cNames_t *ctx = (cNames_t *) ctx1;
  ivarp = (sb2 *) data;
  data += sizeof(sb2);
  ivar = (int) *ivarp;
  allocRecordML(ctx->rListAddr, 2, pair);
  allocRecordML(ctx->rAddrEPairs, 2, pair2);
  if (ivar != 0)
  {
    rs = NULL;
  }
  else 
  {
    rs = convertStringToML(ctx->rStringAddr, data);
  }
  first(pair2) = (int) rs;
  second(pair2) = ivar;
  first(pair) = (int) pair2;
  second(pair) = (int) ctx->list;
  makeCONS(pair, ctx->list);
  return ctx;
}/*}}}*/

int
apsmlORAGetRow(int vAddrPair, Region rAddrLPairs, Region rAddrEPairs, Region rAddrString, 
            oSes_t *ses, void *rd)/*{{{*/
{
  cNames_t cn1;
  int res;
  cNames_t *cn = &cn1;
  cn->rListAddr = rAddrLPairs;
  cn->rStringAddr = rAddrString;
  cn->rAddrEPairs = rAddrEPairs;
  makeNIL(cn->list);
  res = DBGetRow(ses, dumpRows, (void **) &cn, rd);
  first(vAddrPair) = (int) cn1.list;
  second(vAddrPair) = res;
  return vAddrPair;
}/*}}}*/


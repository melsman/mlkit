// The Apache module to serve SMLserver


#include <sys/stat.h> /*{{{*/
#include "httpd.h"
#include "http_config.h"
#include "http_log.h"
#include "../../Runtime/LoadKAM.h"
#include "../HashTable.h"
#include "../../Runtime/String.h"/*}}}*/

#define APSML_SCRIPT_HASHTABLE_SZ 1023/*{{{*/

#define APSML_PATH_MAX 255

#define DEFAULT_PRJID "sources"
#define DEFAULT_XT 0/*}}}*/

enum APSML_RETURNVALUES/*{{{*/
{
  APSML_FILENOTFOUND = -1,
  APSML_ULFILENOTFOUND = -2,
  APSML_OK = 0,
};/*}}}*/

typedef struct /*{{{*/
{
  Interp *interp;
  char *prjid;
  char *trapscript;
  char *initscript;
  char *smlpath;
  int extendedtyping;
  char ulFileName[APSML_PATH_MAX];
  time_t timeStamp;
  HashTable scripts;
} InterpContext; /*}}}*/


// mutexs/*{{{*/
apr_thread_mutex_t *stackPoolMutex;
apr_thread_mutex_t *freelistMutex;
apr_thread_mutex_t *codeCacheMutex;/*}}}*/

module AP_MODULE_DECLARE_DATA sml_module;

void
logMsg (char *msg)/*{{{*/
{
  fprintf (stderr, "Notice; apsml: %s", msg);
}

void
logLoading (char *file)
{
  fprintf (stderr, "Notice, apsml: loaded %s", file);
}/*}}}*/

void
codeCacheMutexLock () /*{{{*/
{
  apr_thread_mutex_lock (codeCacheMutex);
} /*}}}*/

void
codeCacheMutexUnlock ()/*{{{*/
{
  apr_thread_mutex_unlock (codeCacheMutex);
}/*}}}*/

static const char *
set_projid (cmd_parms * cmd, void *mconfig, const char *prjid)/*{{{*/
{
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->prjid = (char *) prjid;

  return NULL;
}/*}}}*/

static const char *
set_initscript (cmd_parms * cmd, void *mconfig, const char *initScript)/*{{{*/
{
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->initscript = (char *) initScript;
  return NULL;
}/*}}}*/

static const char *
set_trapscript (cmd_parms * cmd, void *mconfig, const char *script)/*{{{*/
{
  return NULL;
}/*}}}*/

static const char *
setXt (cmd_parms *cmd, void *mconfig, int flag)/*{{{*/
{
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->extendedtyping = flag;
  return NULL;
}/*}}}*/

static const char *
set_sml_path (cmd_parms *cmd, void *mconfig, const char *path) {/*{{{*/
  InterpContext *ctx =
    ap_get_module_config (cmd->server->module_config, &sml_module);
  ctx->smlpath = (char *) path;
  return NULL;
}/*}}}*/

static const 
command_rec mod_sml_cmds[] = /*{{{*/
{
  AP_INIT_TAKE1 ("SmlPrjId", set_projid, NULL, RSRC_CONF,
		 "apsml: You must set prjid in the config file"),
  AP_INIT_TAKE1 ("SmlInitScript", set_initscript, NULL, RSRC_CONF,
		 "SMLSYNTAX ERR SmlInitScript"),
  AP_INIT_TAKE1 ("SmlTrapScript", set_trapscript, NULL, RSRC_CONF,
		 "SMLSYNTAX ERR SmlTrapScript"),
  AP_INIT_TAKE1 ("SmlPath", set_sml_path, NULL, RSRC_CONF,
		  "SMLSYNTAX ERR SmlPath"),
  AP_INIT_FLAG ("SmlExtendedTyping", setXt, NULL, RSRC_CONF,
		"SMLSYNTAX ERR SmlExtendedTyping"),
  {NULL}
};/*}}}*/

static void *
perserver_init (apr_pool_t * p, server_rec * s) //{{{
{
  InterpContext *ctx =
    (InterpContext *) apr_pcalloc (p, sizeof (InterpContext));

  ctx->prjid = DEFAULT_PRJID;
  ctx->extendedtyping = DEFAULT_XT;
  ctx->initscript = NULL;
  ctx->trapscript = NULL;
  ctx->smlpath = NULL;
  return (void *) ctx;
} //}}}

static int
apsml_post_config (apr_pool_t * pconf, apr_pool_t * plog,
		   apr_pool_t * ptemp, server_rec * s) //{{{
{


  InterpContext *ctx = ap_get_module_config (s->module_config, &sml_module);

#ifdef REGION_PAGE_STAT
  rpMap = regionPageMapNew ();
#endif /* REGION_PAGE_STAT */

  // initialize stackPool Mutex, freelist Mutex, and codeCache Mutex
  apr_thread_mutex_create (&stackPoolMutex, APR_THREAD_MUTEX_DEFAULT, pconf);
  apr_thread_mutex_create (&freelistMutex, APR_THREAD_MUTEX_DEFAULT, pconf);
  apr_thread_mutex_create (&codeCacheMutex, APR_THREAD_MUTEX_DEFAULT, pconf);

  resolveGlobalCodeFragments ();

  ctx->interp = interpNew ();

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		"apsml: server->path is %s", s->path);

  sprintf (ctx->ulFileName, "%s/PM/%s.ul",
//        s->path, ctx->prjid);
	   ctx->smlpath, ctx->prjid);

  ctx->scripts = emptyHashTable (APSML_SCRIPT_HASHTABLE_SZ);

  printserver (s);

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		"apsml: module is now loaded");

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		"apsml: ulFileName is %s", ctx->ulFileName);

  if (ctx->initscript != NULL)
    {
      int res = 0;		// apsml_processSmlFile();
      ap_log_error (__FILE__, __LINE__, LOG_ALERT, 0, s,
		    "apsml: init script executed with return code %d", res);
    }
  else
    {
      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		    "apsml: No init script executed");
    }

  if (! ctx->smlpath ) 
  {
      ap_log_error (__FILE__, __LINE__, LOG_ERR, 0, s,
            "apsml: You must set SmlPath");
	  // Figure out how to report configuration error
  }

  return OK;
} //}}}

void
printrequest (request_rec * r) //{{{
{
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->the_request is %s", r->the_request);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->protocol is %s", r->protocol);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->hostname is %s", r->hostname);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->status_line is %s", r->status_line);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->method is %s", r->method);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->range is %s", r->range);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->content_type is %s", r->content_type);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->handler is %s", r->handler);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->content_encoding is %s",
		 r->content_encoding);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->content_languages is %s",
		 r->content_languages);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->vlist_validator is %s",
		 r->vlist_validator);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->user is %s", r->user);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->ap_auth_type is %s", r->ap_auth_type);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->unparsed_uri is %s", r->unparsed_uri);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->uri is %s", r->uri);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->filename is %s", r->filename);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->path_info is %s", r->path_info);
  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "apsml: request_rec->args is %s", r->args);
  return;
} //}}}

void
printserver (server_rec * s) //{{{
{

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		"apsml: server->defn_name is %s", s->defn_name);

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		"apsml: server->server_admin is %s", s->server_admin);

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		"apsml: server->server_hostname is %s", s->server_hostname);

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		"apsml: server->error_fname is %s", s->error_fname);

  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s,
		"apsml: server->path is %s", s->path);

  return;
} //}}}

time_t
apsml_fileModTime (char *file) //{{{
{
  struct stat buf;
  if (stat (file, &buf) != 0)
    return (time_t) - 1;
  return buf.st_mtime;
} //}}}

// static InterpContext *globalCtx;
// static char *extendedtyping = NULL;

int
apsml_next_sml0 (char *p) //{{{
{
  if (*(p + 1) == 's' && *(p + 2) == 'm'
      && *(p + 3) == 'l' && *(p + 4) == '\0')
    return 1;
  else
    return 0;
} //}}}

int
apsml_smlFileToUoFile (request_rec * r, char *url, char *uo, char *prjid,
		       int path_p) //{{{
{
  const char *pageRoot;
  char *p;			/*  = strrchr(url, '/'); */
  int i;
  InterpContext *ctx =
    ap_get_module_config (r->server->module_config, &sml_module);

//  pageRoot = r->server->path;
  pageRoot = ctx->smlpath;
  if (strstr (url, pageRoot) != url) 
  {
      ap_log_rerror (__FILE__, __LINE__, LOG_ERR, -1, r,
		     "pageRoot %s is not a substring of the requested url %s",
		     pageRoot, url);
      return -1;
    }
  if (path_p)
    {
      strcpy (uo, pageRoot);
      strcat (uo, "/PM/");
      strcat (uo, prjid);
    }
  else
    {
      strcpy (uo, prjid);
    }
  strcat (uo, "-");
  i = strlen (uo);
  p = url + strlen (pageRoot);
  if (*p == '/') p++;
  while (*p != '\0')
    {
      char c = *p;
      if (c == '.')
        {
		  if (ctx->extendedtyping && apsml_next_sml0 (p))
			{
			  uo[i++] = '%';
			  uo[i++] = 'g';
			  uo[i++] = 'e';
			  uo[i++] = 'n';
			}
		  if (*(p+1) == '.' )
		  { 
		  c = '%';
		  p++;
		  }
		}
      if (c == '/')
	c = '+';
      uo[i++] = c;
      p++;
    }
  uo[i] = '\0';
  strcat (uo, ".uo");
  return 0;
} //}}}

static int
apsml_processSmlFile (request_rec * r) //{{{
{
  char *urlfile = r->filename;	/* the requested url as file */
  char uo[APSML_PATH_MAX];
  char uo_file[APSML_PATH_MAX];
  int res;
  time_t t;
  char *errorStr = NULL;

  InterpContext *ctx =
    ap_get_module_config (r->server->module_config, &sml_module);

  /*
   * Test to see if the ul-file exists
   */

  t = apsml_fileModTime (ctx->ulFileName);

  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "mod_sml:Notice ul-file has time %i", t);

  if (t == (time_t) - 1)
    {
      ap_log_rerror (__FILE__, __LINE__, LOG_ERR, -1, r,
		     "mod_sml:Err ul-file %s does not exist - web service not working",
		     &ctx->ulFileName);
      return APSML_ULFILENOTFOUND;
    }

  /*
   * (Re)load interpreter if timeStamps do not match
   */

  if (ctx->timeStamp != t)
    {
      // Reload the interpreter

      FILE *is;
      char buff[APSML_PATH_MAX];
      int count = 0;

      // MEMO: somehow wait for all executions to finish!
      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		     "apsml: (re)loading interpreter oldtime: %i newtime %i", ctx->timeStamp, t);

      // free all code elements present in the
      // interpreter, including code cache entries...
      interpClear (ctx->interp);

      // clear the heap cache
      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		     "apsml: clearing heap cache");
      clearHeapCache ();

      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		     "apsml: opening ul-file %s", ctx->ulFileName);
      is = fopen (ctx->ulFileName, "r");
      if (is == NULL)
	{
	  ap_log_rerror (__FILE__, __LINE__, LOG_ERR, 0, r,
			 "apsml: Failed to open file %s for reading",
			 &ctx->ulFileName);
	  return APSML_ULFILENOTFOUND;
	}

      while (fgets (buff, APSML_PATH_MAX, is) != NULL)
	{
	  if (buff[strlen (buff) - 1] == '\n')
	    buff[strlen (buff) - 1] = '\0';

	  if (!strcmp (buff, "scripts:"))
	    break;

	  interpLoadExtend (ctx->interp, buff);
//	  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
//			 "apsml: Loading %s", buff);
	  count++;
	}

      // clear the script-name hash table
      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		     "apsml: clearing script-name hash table");
      freeHashTable (ctx->scripts);
      ctx->scripts = emptyHashTable (APSML_SCRIPT_HASHTABLE_SZ);

      if (!strcmp (buff, "scripts:"))
	{
	  while (fgets (buff, APSML_PATH_MAX, is) != NULL)
	    {
	      if (buff[strlen (buff) - 1] == '\n')
		buff[strlen (buff) - 1] = '\0';
	      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
			     "apsml: Accepting script: %s", buff);
	      insertHashTable (ctx->scripts, buff, "ok");
	    }
	}

      // close the ul-file
      fclose (is);
      ctx->timeStamp = t;
      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		     "apsml: (Re)loaded %d uo-files with timestamp %i", count, t);
    }

  if (apsml_smlFileToUoFile (r, urlfile, uo, ctx->prjid, 1) == -1)
    {
      return APSML_FILENOTFOUND;
    }

  // See if uo-file is a script that can be served
  if (apsml_smlFileToUoFile (r, urlfile, uo_file, ctx->prjid, 0) == -1)
    {
      return APSML_FILENOTFOUND;
    }

  if (!lookupHashTable (ctx->scripts, uo_file))
    {
      int i;
      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		     "apsml: Request not script: %s", uo_file);
      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		     "apsml: Size of hash table: %d", ctx->scripts->size);
      ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		     "apsml: Size of hash table array: %d",
		     ctx->scripts->arraySize);
      for (i = 0; i < ctx->scripts->arraySize; i++)
	{
	  ObjectListHashTable *ol;
	  if ((ol = ctx->scripts->array[i]) == 0)
	    continue;
	  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
			 "apsml: array[%d]:", i);
	  for (; ol; ol = ol->next)
	    //  {
	    ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
			   "apsml:   %s: %s", ol->key, (char *) (ol->value));
	  //  }
	}
      return APSML_FILENOTFOUND;
    }

  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "Starting interpreter on file %s", uo_file);

  res = interpLoadRun (ctx->interp, uo, &errorStr, (void *) r);

  ap_log_rerror (__FILE__, __LINE__, LOG_NOTICE, 0, r,
		 "Interpretation ended on file %s", uo_file);

  if (res < 0)
    {				// uncaught exception; errorStr allocated
      if (res == -1)		// exception other than Interrupt raised
	{
	  ap_log_rerror (__FILE__, __LINE__, LOG_WARNING, -1, r,
			 "%s raised %s", urlfile, errorStr);
	}
      free (errorStr);		// free the malloced string 
      errorStr = NULL;		// - and nullify field    
    }

  return APSML_OK;

} //}}}

static int
mod_sml_method_handler (request_rec * r) //{{{
{ 
  if (strcmp (r->handler, "sml-module"))
    return DECLINED;
//  printrequest (r);
  int res;
  res = apsml_processSmlFile (r);
  switch (res)
    {
    case APSML_FILENOTFOUND:
      {
//	printrequest (r);
	return HTTP_NOT_FOUND;
	break;
      }
    case APSML_ULFILENOTFOUND:
      {
//	printrequest (r);
	return HTTP_NOT_FOUND;
	break;
      }
    default:
      {
//	printrequest (r);
	return OK;
	break;
      }
    }
} //}}}

static void 
mod_sml_register_hooks (apr_pool_t * p) //{{{
{
  ap_hook_handler (mod_sml_method_handler, NULL, NULL, APR_HOOK_MIDDLE);
  ap_hook_post_config (apsml_post_config, NULL, NULL, APR_HOOK_MIDDLE);
} //}}}

module AP_MODULE_DECLARE_DATA sml_module = //{{{
{ 
  STANDARD20_MODULE_STUFF,	/* stuff that needs to be declared in every 2.0 mod */
  NULL,				/* create per-directory config structure            */
  NULL,				/* merge per-directory config structures            */
  perserver_init,		/* create per-server config structure               */
  NULL,				/* merge per-server config structures               */
  mod_sml_cmds,			/* command apr_table_t                              */
  mod_sml_register_hooks	/* register hooks                                   */
}; //}}}

void
apsml_returnHTML (int rr, StringDesc * str, int exn) //{{{
{
  request_rec *r = (request_rec *) rr;
  int stringSize = sizeStringDefine (str) + 1;
  char *s = (char *) apr_pcalloc (r->pool, stringSize);
  if (s == NULL) raise_exn (exn);
  convertStringToC (str, s, stringSize, exn);

  int rv = ap_rputs (s, r);
  return;
} //}}}

/*
 * nssml.c --
 *
 *      Standard ML language extension for AOLserver. Add the lines
 *
 *           ns_section "ns/server/mael/module/nssml"
 *           ns_param uoListFile "..../kit/basislib/basislib.ul"
 *
 *      (where .... is the exact path to the ML Kit) to your nsd.tcl 
 *      file and the line
 *
 *           ns_param nssml nssml.so
 *
 *      to the modules section.
 */

#include <stdio.h>
#include <sys/stat.h>
#include "ns.h"
#include "../RuntimeWithGC/LoadKAM.h"

#define NSSML_PATH_MAX 255
#define NSSML_ERROR_BUFF 4096

time_t
nssml_fileModTime(char* file) 
{
  struct stat buf;
  if ( stat(file, &buf) != 0 )
    return (time_t)-1;
  return buf.st_mtime;
}  
    
  

/*
 * The Ns_ModuleVersion exported integer is used to verify
 * this module version when loaded.  For AOLserver 2.0,
 * 1 (one) is the only valid value for this variable.
 */
int Ns_ModuleVersion = 1;

static Ns_OpProc nssml_handleSmlFile;

/*
 * Temporarily, we have only one interpreter, protected with a mutex
 */

typedef struct {
  Ns_Mutex lock;
  Interp* interp;
  char* hServer;
  char* prjid;
  char ulFileName[NSSML_PATH_MAX];
  char timeStampFileName[NSSML_PATH_MAX];
  time_t timeStamp;
} InterpContext;

/*
 *----------------------------------------------------------------------
 *
 * Ns_ModuleInit --
 *
 *      This is the sml module's entry point.  AOLserver runs this
 *      function right after the module is loaded.  It is used to read
 *      configuration data, initialize data structures, and do other
 *      things at startup. In particular, global regions are
 *      initialized and the SML Basis Library is loaded together with
 *      an SML interface to access the AOLserver.
 *
 * The function is passed two parameters:
 *
 * hServer:   The server `handle' as a string. This is the
 *            short name given to the virtual server such
 *            as `server1'.
 *
 * hModule:   The module `handle' as a string. This is the
 *            short name given to the module such as `nssml'
 *
 * For example, if this module is known as `nssml' and loaded
 * into the `server1' server with entries similar to the following
 * in the nsd.ini file:
 *
 * [ns\servers]
 * server1=My First Server
 *
 * [ns\server1\modules]
 * nssml=nssml.so
 *
 * This function would be called with "server1" and "nssml" as
 * its arguments.
 *
 * Results:
 *	NS_OK or NS_ERROR
 *
 * Side effects:
 *	Module loads and initializes itself.
 *
 *---------------------------------------------------------------------- */
 
int
Ns_ModuleInit(char *hServer, char *hModule)
{
  InterpContext* ctx;
  char* configPath;

  /*
   * Create and initalize the interpreter context.
   */
  ctx = (InterpContext*)Ns_Malloc(sizeof(InterpContext));
  Ns_InitializeMutex(&ctx->lock);
  ctx->interp = interpNew();
  ctx->hServer = hServer;
  configPath = Ns_ConfigGetPath(hServer, hModule, NULL);   // Fetch the name of the project (prjid)
  ctx->prjid = Ns_ConfigGetValue(configPath, "prjid");     // from the config file.

  if (ctx->prjid == NULL) {
    Ns_Log(Error, "nssml: You must set prjid in the config file");
    return NS_ERROR;
  }

  sprintf(ctx->ulFileName, "%s/PM/%s.ul", Ns_PageRoot(hServer), ctx->prjid);
  sprintf(ctx->timeStampFileName, "%s/PM/%s.timestamp", Ns_PageRoot(hServer), ctx->prjid);
  
  ctx->timeStamp = (time_t)-1; 

  Ns_RegisterRequest(hServer, "GET", "/*.sml", nssml_handleSmlFile, NULL, ctx, 0);
  Ns_RegisterRequest(hServer, "GET", "/*.msp", nssml_handleSmlFile, NULL, ctx, 0);
  Ns_RegisterRequest(hServer, "POST", "/*.sml", nssml_handleSmlFile, NULL, ctx, 0);
  Ns_RegisterRequest(hServer, "POST", "/*.msp", nssml_handleSmlFile, NULL, ctx, 0);
    
  Ns_Log(Notice, "nssml: module is now loaded");
  Ns_Log(Notice, "nssml: ulFileName is %s", ctx->ulFileName);
  Ns_Log(Notice, "nssml: timeStampFileName is %s", ctx->timeStampFileName);
  
  return NS_OK;
}

/* -------------------------------------------------
 * nssml_smlFileToUoFile - convert sml-absolute filename
 * into the uo-file for the sml-file. Also works for 
 * msp-files.
 * ------------------------------------------------- */

void 
nssml_smlFileToUoFile(char* url, char* uo, char* prjid) 
{
  char* p = strrchr(url, '/');   
  char name[NSSML_PATH_MAX];
  strcpy(name, p+1);
  strncpy(uo, url, p-url);
  uo[p-url] = 0;
  strcat(uo, "/PM/NoProf/"); 
  strcat(uo, prjid);
  strcat(uo, "-");
  strcat(uo, name);
  strcat(uo, ".uo");
}

/* ---------------------------------------------------------
 * nssml_handleSmlFile - function for handling requests
 * for sml-files; returns an error page if the sml-file
 * does not exist.
 * --------------------------------------------------------- */

static int
nssml_handleSmlFile(Ns_OpContext context, Ns_Conn *conn)
{
  InterpContext* ctx;
  char* url;             /* the requested url */
  Ns_DString ds;
  char *server;          /* the server */
  char uo[NSSML_PATH_MAX];
  int res;
  time_t t;

  ctx = (InterpContext*)context;
  server = Ns_ConnServer(conn);

  /* Check that sml-file exists */
  
  if ( Ns_UrlIsFile(server, conn->request->url) != 1 ) {
    Ns_ConnReturnNotFound(conn);
    return NS_ERROR;
  }

  /*
   * Test to see if the time stamp file is existing
   */

  t = nssml_fileModTime(ctx->timeStampFileName);
  
  if ( t == (time_t)-1 )
    {
      // Return error page
      Ns_ConnReturnNotice(conn, 200, "The web service is temporarily out of service",
			  "Please come back later!");
      Ns_Log(Error, "nssml: time stamp file %s not existing - web service not working",
	     &ctx->timeStampFileName);
      return NS_OK;
    }

  /*
   * Protect the running of this code from simultaneous requests
   */
  Ns_LockMutex(&ctx->lock);

  /*
   * (Re)load interpreter if timeStamps do not match
   */
 
  if ( ctx->timeStamp != t ) 
    {
      // Reload the interpreter
      FILE* is;
      char buff[NSSML_PATH_MAX];
      int count = 0;

      interpClear(ctx->interp);      /* free all code elements present in the
				      * interpreter... */

      is = fopen(ctx->ulFileName, "r");
      if ( is == NULL ) 
	{
	  // Return error page
	  Ns_ConnReturnNotice(conn, 200, "The web service is temporarily out of service",
			      "Please come back later!");
	  Ns_Log(Error, "nssml: Failed to open file %s for reading", &ctx->ulFileName);

	  // Release the lock
	  Ns_UnlockMutex(&ctx->lock);
	  return NS_OK;
	}
    
      while ( fgets ( buff, NSSML_PATH_MAX, is ) != NULL ) 
	{
	  if ( buff[strlen(buff) - 1] == '\n' ) 
	    buff[strlen(buff) - 1] = '\0';

	  interpLoadExtend(ctx->interp, buff);
	  // Ns_Log(Notice, "nssml: Loading %s", buff);
	  count++;
	}
      ctx->timeStamp = t;
      Ns_Log(Notice, "nssml: (Re)loaded %d uo-files", count);
    }

  Ns_DStringInit(&ds);
  Ns_UrlToFile(&ds, server, conn->request->url);
  url = ds.string;

  nssml_smlFileToUoFile(url,uo,ctx->prjid);
  // Ns_Log(Notice, "Starting interpreter on file %s", uo);
  res = interpLoadRun(ctx->interp, uo);
  if ( res == -1 ) {    /* exception other than Interrupt raised */
    Ns_Log(Warning, "%s raised %s", url, ctx->interp->error);
  }
  free(ctx->interp->error);      // free the malloced string 
  ctx->interp->error = NULL;     // - and nullify field
  Ns_DStringFree(&ds);

  // Release the lock
  Ns_UnlockMutex(&ctx->lock);
  return NS_OK; 
}

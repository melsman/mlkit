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
#include "ns.h"
#include "../RuntimeWithGC/LoadKAM.h"

/*
 * The Ns_ModuleVersion variable is required.
 */
int Ns_ModuleVersion = 1;

static Ns_OpProc nssml_handleSmlFile;

/*
 * Temporarily, we have only one interpreter, protected with mutexes
 */

Interp * theOneInterp;

/*
 * Private functions
 */
int
Ns_ModuleInit(char *hServer, char *hModule);

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
  FILE* is;

  #define BUFF_SIZE 1000
  char buff[BUFF_SIZE];

  char* configPath;
  char* uoListFile;

  /*
   * Initialize the one interpreter
   */
  theOneInterp = interpNew();

  /*
   * Fetch the name of the KAM uolistfile from the config file.
   */
  configPath = Ns_ConfigGetPath(hServer, hModule, NULL);
  uoListFile = Ns_ConfigGetValue(configPath, "uoListFile");

  if (uoListFile == NULL) {
    Ns_Log(Error, "nssml: uoListFile must be set in config file");
    return NS_ERROR;
  }

  if ( (is = fopen(uoListFile, "r")) == NULL ) {
    Ns_Log(Error, "nssml: Failed to open uoListFile for reading");
    return NS_ERROR;
  }
    
  while ( fgets ( buff, BUFF_SIZE, is ) != NULL ) {
    if ( buff[strlen(buff) - 1] == '\n' ) {
      buff[strlen(buff) - 1] = '\0';
    }
    interpLoadExtend(theOneInterp, buff);
    Ns_Log(Notice, "nssml: Loading %s", buff);
  }

  Ns_RegisterRequest(hServer, "GET", "/*.sml", nssml_handleSmlFile, 
		     NULL, theOneInterp, 0);
    
  Ns_Log(Notice, "nssml: It works - nssml module is loaded!");
  return NS_OK;
}

#define NSSML_PATH_MAX 255
#define NSSML_ERROR_BUFF 4096

/* -------------------------------------------------
 * smlFileToUoFile - convert sml-absolute filename
 * into the uo-file for the sml-file. 
 * ------------------------------------------------- */

void 
nssml_smlFileToUoFile(char* url, char* uo) 
{
  char* p = strrchr(url, '/');   
  char name[NSSML_PATH_MAX];
  strcpy(name, p+1);
  strncpy(uo, url, p-url);
  uo[p-url] = 0;
  strcat(uo, "/PM/NoProf/");
  strcat(uo, name);
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
  Interp* interp;        /* interpreter */
  char* url;             /* the requested url */
  Ns_DString ds;
  char *server;          /* the server */
  char uo[NSSML_PATH_MAX];
  int res;

  interp = (Interp*)context;
  server = Ns_ConnServer(conn);

  /* Check that sml-file exists */
  
  if ( Ns_UrlIsFile(server, conn->request->url) != 1 ) {
    char error_buff[NSSML_ERROR_BUFF];
    sprintf(error_buff, "The sml-file %s that you requested is not on the server!", 
	    conn->request->url);
    Ns_ConnReturnNotice(conn, 200, error_buff, NULL);
    return NS_ERROR;
  }

  Ns_DStringInit(&ds);
  Ns_UrlToFile(&ds, server, conn->request->url);
  url = ds.string;

  nssml_smlFileToUoFile(url,uo);
  /*  Ns_Log(Notice, "Starting interpreter on file %s", uo); */
  res = interpLoadRun(interp, uo, conn);
  /*  Ns_Log(Notice, "Interpreter returned %d", res); */
  Ns_DStringFree(&ds);
  return NS_OK; 
}


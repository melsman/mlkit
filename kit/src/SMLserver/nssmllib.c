/* ----------------------------------------------------------
 *
 *  AOLserver API for the ML Kit
 *
 *  The name of each function is prepended with nssml_
 *
 * ---------------------------------------------------------- */

#include "ns.h"
#include "../Runtime/Region.h"
#include "../Runtime/String.h"
#include "../Runtime/Exception.h"
#include "../Runtime/Tagging.h"

#ifdef REGION_PAGE_STAT
void
nssml_LogRegionPageStat(void)
{
  int i;
  char str[100];
  RegionPageMapHashList* p;
  Ns_Log(Notice, "nssml_LogRegionPageStat - begin");

  for ( i = 0 ; i < REGION_PAGE_MAP_HASH_TABLE_SIZE ; i++ ) 
    {
      p = rpMap[i];
      while ( p )
	{ 
	  if (p->n != 0) 
	    {
	      sprintf(str, "%d,", p->n);
	      Ns_Log(Notice, str);
	    }
	  p = p->next;
	}
    }
  Ns_Log(Notice, "nssml_LogRegionPageStat - end");
}
#else
void
nssml_LogRegionPageStat(void)
{
  Ns_Log(Notice, "nssml_LogRegionPageStat - disabled");
}
#endif /* REGION_PAGE_STAT */

// ML: set * string -> string ptr_option
String
nssml_SetGet(Region rAddr, Ns_Set* set, String key) 
{
  char* res_c = Ns_SetGet(set, &(key->data));
  if ( res_c == NULL ) 
    {
      return (String)NULL;
    }
  return convertStringToML(rAddr, res_c);
}

// ML: string ptr_option -> bool
int
nssml_isNullString(String s) 
{
  if ( s == NULL ) return mlTRUE;
  else return mlFALSE;
}

// ML: set * int -> string ptr_option
String
nssml_SetKey(Region rAddr, Ns_Set* set, int i)
{
  char* res_c = Ns_SetKey(set,i);
  if ( res_c == NULL ) 
    {
      return (String)NULL;
    }
  return convertStringToML(rAddr, res_c);
}

// ML: set * int -> string ptr_option
String
nssml_SetValue(Region rAddr, Ns_Set* set, int i)
{
  char* res_c = Ns_SetValue(set,i);
  if ( res_c == NULL ) 
    {
      return (String)NULL;
    }
  return convertStringToML(rAddr, res_c);
}

// ML: set -> int
int
nssml_SetSize(Ns_Set* set)
{
  return Ns_SetSize(set);   // macro
}


// ML: conn -> string
String
nssml_PageRoot(Region rAddr, Ns_Conn *c) 
{
  char* s_c = Ns_PageRoot(Ns_ConnServer(c));
  return convertStringToML(rAddr, s_c);
}

// ML: conn -> string
String
nssml_ConnHost(Region rAddr, Ns_Conn *c)
{
  return convertStringToML(rAddr, Ns_ConnHost(c));
}

// ML: conn -> string ptr_option
String
nssml_ConnLocation(Region rAddr, Ns_Conn *c)
{
  char* s = Ns_ConnLocation(c);
  if ( s == NULL ) return NULL;
  return convertStringToML(rAddr, s);
}

// ML: conn -> string
String
nssml_ConnPeer(Region rAddr, Ns_Conn *c)
{
  return convertStringToML(rAddr, Ns_ConnPeer(c));
}

// ML: conn -> string
String
nssml_ConnServer(Region rAddr, Ns_Conn *c)
{
  return convertStringToML(rAddr, Ns_ConnServer(c));
}

// ML: unit -> string
String
nssml_InfoConfigFile(Region rAddr)
{
  return convertStringToML(rAddr, Ns_InfoConfigFile());
}

// ML: unit -> string
String
nssml_InfoErrorLog(Region rAddr)
{
  return convertStringToML(rAddr, Ns_InfoErrorLog());
}

// ML: unit -> string
String
nssml_InfoHomePath(Region rAddr)
{
  return convertStringToML(rAddr, Ns_InfoHomePath());
}

// ML: unit -> string
String
nssml_InfoHostname(Region rAddr)
{
  return convertStringToML(rAddr, Ns_InfoHostname());
}

// ML: unit -> string
String
nssml_InfoServerVersion(Region rAddr)
{
  return convertStringToML(rAddr, Ns_InfoServerVersion());
}

// ML: string * string -> status
int
nssml_returnFile(Ns_Conn* conn, String mimetype, String file)
{
  int status = 200;
  return Ns_ConnReturnFile(conn, status, &(mimetype->data), &(file->data));
}

// ML: string -> string
String
nssml_GetMimeType(Region rAddr, String s)
{
  char* res_c = Ns_GetMimeType(&(s->data));
  if ( res_c == NULL ) 
    {
      Ns_Log(Warning, "nssml_GetMimeType problem - returning empty string");
      res_c = "";
    }
  return convertStringToML(rAddr, res_c);
}

// ML: string -> string ptr_option
String
nssml_GetHostByAddr(Region rAddr, String s)
{
  Ns_DString ds;
  Ns_DStringInit(&ds);
  if ( Ns_GetHostByAddr(&ds, &(s->data)) == NS_FALSE ) 
    {
      Ns_DStringFree(&ds);
      return (String)NULL;
    }
  s = convertStringToML(rAddr, Ns_DStringValue(&ds));
  Ns_DStringFree(&ds);
  return s;
}

// ML: string -> string
String
nssml_EncodeUrl(Region rAddr, String s)
{
  Ns_DString ds;
  Ns_DStringInit(&ds);
  s = convertStringToML(rAddr, Ns_EncodeUrl(&ds, &(s->data)));
  Ns_DStringFree(&ds);
  return s;
}

// ML: string -> string
String
nssml_DecodeUrl(Region rAddr, String s)
{
  Ns_DString ds;
  Ns_DStringInit(&ds);
  s = convertStringToML(rAddr, Ns_DecodeUrl(&ds, &(s->data)));
  Ns_DStringFree(&ds);
  return s;
}

// ML: {sectionName: string, key: string} -> string ptr_option
String
nssml_configGetValue(Region rAddr, String section, String key)
{
  char* res = Ns_ConfigGetValue(&(section->data), &(key->data));
  if ( res == NULL ) return NULL;
  return convertStringToML(rAddr, res);
}

// ML: {sectionName: string, key: string} -> string ptr_option
String
nssml_configGetValueExact(Region rAddr, String section, String key)
{
  char* res = Ns_ConfigGetValueExact(&(section->data), &(key->data));
  if ( res == NULL ) return NULL;
  return convertStringToML(rAddr, res);
}

// ML: conn -> string
String
nssml_ConnUrl(Region rAddr, Ns_Conn* conn)
{
  return convertStringToML(rAddr, conn->request->url);
}

// ML: string -> string ptr_option
String
nssml_FetchUrl(Region rAddr, String url)
{
  Ns_DString ds;
  String res;
  Ns_DStringInit(&ds);
  if ( Ns_FetchURL(&ds, &(url->data), NULL) != NS_OK ) 
    {
      Ns_DStringFree(&ds);
      return NULL;
    }
  res = convertStringToML(rAddr, Ns_DStringValue(&ds));
  Ns_DStringFree(&ds);
  return res;
}

// Function for freeing cached values
static void
nssml_CacheValueFree(void *p)
{
  Ns_Free(p);
}

// ML: string * int -> cache ptr_option
Ns_Cache*
nssml_CacheCreate(String cacheName, int timeout)
{
  return Ns_CacheCreate(&(cacheName->data), TCL_STRING_KEYS, 
			timeout, nssml_CacheValueFree);
}
  
// ML: string * int -> cache ptr_option
Ns_Cache*
nssml_CacheCreateSz(String cacheName, int maxSize)
{
  return Ns_CacheCreateSz(&(cacheName->data), TCL_STRING_KEYS, 
			  maxSize, nssml_CacheValueFree);
}

// ML: cache * string * string -> int
int
nssml_CacheSet(Ns_Cache* cache, String key, String value)
{
  char *key_c;
  char *value_c;
  int sz;
  int new;
  Ns_Entry *ePtr;
  sz = sizeStringDefine(key) + 1;         // copying strings is essential!!
  key_c = (char*)Ns_Malloc(sz);
  strcpy(key_c,&(key->data));
  sz = sizeStringDefine(value) + 1;
  value_c = (char*)Ns_Malloc(sz);
  strcpy(value_c,&(value->data));
  Ns_CacheLock(cache);
  ePtr = Ns_CacheCreateEntry(cache, key_c, &new);
  Ns_CacheSetValueSz(ePtr, value_c, sz);
  Ns_CacheUnlock(cache);
  return new;
}

// ML: cache * string -> string ptr_option
String
nssml_CacheGet(Region rAddr, Ns_Cache* cache, String key)
{
  int sz;
  char *value_c;
  Ns_Entry *ePtr;
  StringDesc* res;
  Ns_CacheLock(cache);
  ePtr = Ns_CacheFindEntry(cache, &(key->data));
  if ( ePtr == NULL ) {
    goto none;
  }
  value_c = (char*)Ns_CacheGetValue(ePtr);
  if ( value_c == NULL ) {
    goto none;
  }
  res = convertStringToML(rAddr, value_c);
  Ns_CacheUnlock(cache);
  return res;
 none:
  Ns_CacheUnlock(cache);
  return NULL;
}

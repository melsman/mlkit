/* ----------------------------------------------------------
 *
 *  AOLserver API for the ML Kit
 *
 *  The name of each function is prepended with nssml_
 *
 * ---------------------------------------------------------- */

#include "ns.h"
#include "../RuntimeWithGC/String.h"
#include "../RuntimeWithGC/Exception.h"
#include "../RuntimeWithGC/Tagging.h"

// ML: severity * string -> unit
void
nssml_log(Ns_LogSeverity ls, StringDesc* s_ml) 
{
  char* s_c;
  int size = 1 + sizeStringDefine(s_ml);
  s_c = (char*)Ns_Malloc(size);
  convertStringToC(s_ml, s_c, size, (int)&exn_OVERFLOW);
  Ns_Log(ls, s_c); 
  Ns_Free(s_c);
  return;
}

// ML: conn * int * string -> status
int
nssml_ConnReturnHtml(Ns_Conn* c, int status, StringDesc* s_ml) 
{
  int res;
  char* s_c;
  int size = 1 + sizeStringDefine(s_ml);
  s_c = (char*)Ns_Malloc(size);
  convertStringToC(s_ml, s_c, size, (int)&exn_OVERFLOW);
  res = Ns_ConnReturnHtml(c, status, s_c, strlen(s_c));
  Ns_Free(s_c);
  return res;
}

// ML: unit -> conn
Ns_Conn* 
nssml_GetConn () 
{
  return (Ns_TclGetConn (NULL));
}

// ML: set * string -> string ptr_option
StringDesc *
nssml_SetGet(int rAddr, Ns_Set* set, StringDesc* key) 
{
  int sz;
  char* s_c;
  char* res_c;
  sz = sizeStringDefine(key) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(key, s_c, sz, (int)&exn_OVERFLOW);
  res_c = Ns_SetGet(set, s_c);
  Ns_Free(s_c);
  if ( res_c == NULL ) 
    return (StringDesc*)NULL;
  return(convertStringToML(rAddr, res_c));
}

// ML: string ptr_option -> bool
int
nssml_isNullString(StringDesc* s) 
{
  if ( s == NULL ) return mlTRUE;
  else return mlFALSE;
}

// ML: set * string * string -> int
int
nssml_SetPut(Ns_Set* set, StringDesc* key, StringDesc* value)
{
  int index;
  int key_sz;
  int value_sz;
  char* key_c;
  char* value_c;
  key_sz = sizeStringDefine(key) + 1;
  value_sz = sizeStringDefine(value) + 1;
  key_c = (char*)Ns_Malloc(key_sz);
  value_c = (char*)Ns_Malloc(value_sz);
  convertStringToC(key, key_c, key_sz, (int)&exn_OVERFLOW);
  convertStringToC(value, value_c, value_sz, (int)&exn_OVERFLOW);
  index = Ns_SetPut(set, key_c, value_c);
  Ns_Free(key_c);  
  Ns_Free(value_c);
  return index;
}

// ML: string -> set
Ns_Set*
nssml_SetCreate(StringDesc* name)
{ 
  Ns_Set* set;
  int sz;
  char* name_c;
  sz = sizeStringDefine(name) + 1;
  name_c = (char*)Ns_Malloc(sz);
  convertStringToC(name, name_c, sz, (int)&exn_OVERFLOW);
  set = Ns_SetCreate(name_c);
  Ns_Free(name_c);
  return set;
}

int
nssml_SetUnique(Ns_Set* set, StringDesc* key)
{
  int res_c;
  int sz;
  char* key_c;
  sz = sizeStringDefine(key) + 1;
  key_c = (char*)Ns_Malloc(sz);
  convertStringToC(key, key_c, sz, (int)&exn_OVERFLOW);
  res_c = Ns_SetUnique(set, key_c);
  Ns_Free(key_c);
  return convertBoolToML(res_c);
}

int
nssml_SetSize(Ns_Set* set)
{
  return (Ns_SetSize(set));
}

// ML: set * int -> string ptr_option
StringDesc*
nssml_SetKey(int rAddr, Ns_Set* set, int i)
{
  char* res_c = Ns_SetKey(set,i);
  if ( res_c == NULL ) {
    return (StringDesc*)NULL;
  }
  return convertStringToML(rAddr, res_c);
}

// ML: set * int -> string ptr_option
StringDesc*
nssml_SetValue(int rAddr, Ns_Set* set, int i)
{
  char* res_c = Ns_SetValue(set,i);
  if ( res_c == NULL ) {
    return (StringDesc*)NULL;
  }
  return convertStringToML(rAddr, res_c);
}
  
int
nssml_ConnPuts(Ns_Conn* c, StringDesc* s)
{
  int sz;
  int res;
  char* s_c;
  sz = sizeStringDefine(s) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(s, s_c, sz, (int)&exn_OVERFLOW);
  res = Ns_ConnPuts(c, s_c);
  Ns_Free(s_c);
  return res;
}

void
nssml_ConnSetRequiredHeaders(Ns_Conn* c, StringDesc* contentType, int contentLength)
{
  int sz;
  char* contentType_c;
  sz = sizeStringDefine(contentType) + 1;
  contentType_c = (char*)Ns_Malloc(sz);
  convertStringToC(contentType, contentType_c, sz, (int)&exn_OVERFLOW);
  Ns_ConnSetRequiredHeaders(c, contentType_c, contentLength);
  Ns_Free(contentType_c);
  return;
}

// ML: unit -> string
StringDesc *
nssml_PageRoot(int rAddr) 
{
  char* s_c = Ns_PageRoot(Ns_ConnServer(Ns_TclGetConn(NULL)));
  return convertStringToML(rAddr, s_c);
}

// ML: conn * string -> status
int
nssml_ConnReturnRedirect(Ns_Conn* c, StringDesc* s)
{
  int res;
  int sz;
  char* s_c;
  sz = sizeStringDefine(s) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(s, s_c, sz, (int)&exn_OVERFLOW);
  res = Ns_ConnReturnRedirect(c, s_c);
  Ns_Free(s_c);
  return res;
}

// ML: poolname -> db
Ns_DbHandle* 
nssml_DbPoolGetHandle(StringDesc* poolname)
{
  char* poolname_c;
  Ns_DbHandle* res;
  int sz;
  sz = sizeStringDefine(poolname) + 1;
  poolname_c = (char*)Ns_Malloc(sz);
  convertStringToC(poolname, poolname_c, sz, (int)&exn_OVERFLOW);
  res = Ns_DbPoolGetHandle(poolname_c);
  Ns_Free(poolname_c);
  return res;
}

// ML: db -> unit
void
nssml_DbPoolPutHandle(Ns_DbHandle* db)
{
  Ns_DbPoolPutHandle(db);
  return;
}  

// ML: db * string -> status
int
nssml_DbDML(Ns_DbHandle* db, StringDesc* s)
{
  char* s_c;
  int res;
  int sz;
  sz = sizeStringDefine(s) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(s, s_c, sz, (int)&exn_OVERFLOW);
  res = Ns_DbDML(db, s_c);
  Ns_Free(s_c);
  return res;
}

Ns_Set *
nssml_DbSelect(Ns_DbHandle *db, StringDesc* s)
{
  char* s_c;
  Ns_Set* res;
  int sz;
  sz = sizeStringDefine(s) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(s, s_c, sz, (int)&exn_OVERFLOW);
  res = Ns_DbSelect(db, s_c);
  Ns_Free(s_c);
  return res;
}

int
nssml_DbGetRow(Ns_DbHandle *db, Ns_Set* s)
{
  return (Ns_DbGetRow(db, s));
}

// ML: conn -> string
StringDesc*
nssml_ConnHost(int rAddr, Ns_Conn *c)
{
  return convertStringToML(rAddr, Ns_ConnHost(c));
}

// ML: conn -> string ptr_option
StringDesc*
nssml_ConnLocation(int rAddr, Ns_Conn *c)
{
  char* s = Ns_ConnLocation(c);
  if ( s == NULL ) return NULL;
  return convertStringToML(rAddr, s);
}

// ML: conn -> string
StringDesc*
nssml_ConnPeer(int rAddr, Ns_Conn *c)
{
  return convertStringToML(rAddr, Ns_ConnPeer(c));
}

// ML: conn * string -> status
int
nssml_ConnRedirect(Ns_Conn *c, StringDesc* url)
{
  char* url_c;
  int sz;
  int res;
  sz = sizeStringDefine(url) + 1;
  url_c = (char*)Ns_Malloc(sz);
  convertStringToC(url, url_c, sz, (int)&exn_OVERFLOW);
  res = Ns_ConnRedirect(c, url_c);
  Ns_Free(url_c);
  return res;  
}

// ML: conn -> string
StringDesc*
nssml_ConnServer(int rAddr, Ns_Conn *c)
{
  return convertStringToML(rAddr, Ns_ConnServer(c));
}

// ML: unit -> string
StringDesc*
nssml_InfoConfigFile(int rAddr)
{
  return convertStringToML(rAddr, Ns_InfoConfigFile());
}

// ML: unit -> string
StringDesc*
nssml_InfoErrorLog(int rAddr)
{
  return convertStringToML(rAddr, Ns_InfoErrorLog());
}

// ML: unit -> string
StringDesc*
nssml_InfoHomePath(int rAddr)
{
  return convertStringToML(rAddr, Ns_InfoHomePath());
}

// ML: unit -> string
StringDesc*
nssml_InfoHostname(int rAddr)
{
  return convertStringToML(rAddr, Ns_InfoHostname());
}

// ML: unit -> string
StringDesc*
nssml_InfoServerVersion(int rAddr)
{
  return convertStringToML(rAddr, Ns_InfoServerVersion());
}

// ML: string -> string
StringDesc*
nssml_GetMimeType(int rAddr, StringDesc* s)
{
  int sz;
  char* s_c;
  char* res_c;
  sz = sizeStringDefine(s) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(s, s_c, sz, (int)&exn_OVERFLOW);
  res_c = Ns_GetMimeType(s_c);
  Ns_Free(s_c);
  if ( res_c == NULL ) {
    Ns_Log(Warning, "nssml_GetMimeType problem - returning empty string");
    res_c = "";
  }
  s = convertStringToML(rAddr, res_c);
  return s;
}

// ML: string -> string ptr_option
StringDesc*
nssml_GetHostByAddr(int rAddr, StringDesc *s)
{
  int sz;
  char* s_c;
  Ns_DString ds;
  sz = sizeStringDefine(s) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(s, s_c, sz, (int)&exn_OVERFLOW);
  Ns_DStringInit(&ds);
  if ( Ns_GetHostByAddr(&ds, s_c) == NS_FALSE ) {
    Ns_DStringFree(&ds);
    return (StringDesc*)NULL;
  }
  s = convertStringToML(rAddr, Ns_DStringValue(&ds));
  Ns_DStringFree(&ds);
  return s;
}

// ML: string -> string
StringDesc*
nssml_EncodeUrl(int rAddr, StringDesc* s)
{
  int sz;
  char* s_c;
  Ns_DString ds;
  sz = sizeStringDefine(s) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(s, s_c, sz, (int)&exn_OVERFLOW);
  Ns_DStringInit(&ds);
  s = convertStringToML(rAddr, Ns_EncodeUrl(&ds, s_c));
  Ns_DStringFree(&ds);
  return s;
}

// ML: string -> string
StringDesc*
nssml_DecodeUrl(int rAddr, StringDesc* s)
{
  int sz;
  char* s_c;
  Ns_DString ds;
  sz = sizeStringDefine(s) + 1;
  s_c = (char*)Ns_Malloc(sz);
  convertStringToC(s, s_c, sz, (int)&exn_OVERFLOW);
  Ns_DStringInit(&ds);
  s = convertStringToML(rAddr, Ns_DecodeUrl(&ds, s_c));
  Ns_DStringFree(&ds);
  return s;
}

// ML: {sectionName: string, key: string} -> string ptr_option
StringDesc*
nssml_configGetValue(int rAddr, StringDesc *section_ml, StringDesc *key_ml)
{
  int section_sz, key_sz;
  char* section_c;
  char* key_c;
  char* res;
  StringDesc* res_ml;
  section_sz = sizeStringDefine(section_ml) + 1;
  key_sz = sizeStringDefine(key_ml) + 1;
  section_c = (char*)Ns_Malloc(section_sz);
  key_c = (char*)Ns_Malloc(key_sz);
  convertStringToC(section_ml, section_c, section_sz, (int)&exn_OVERFLOW);
  convertStringToC(key_ml, key_c, key_sz, (int)&exn_OVERFLOW);
  res = Ns_ConfigGetValue(section_c, key_c);
  if ( res == NULL ) {
    Ns_Free(section_c);
    Ns_Free(key_c);
    return (StringDesc*)NULL;
  }
  res_ml = convertStringToML(rAddr, res);
  return res_ml;
}

// ML: {sectionName: string, key: string} -> string ptr_option
StringDesc*
nssml_configGetValueExact(int rAddr, StringDesc *section_ml, StringDesc *key_ml)
{
  int section_sz, key_sz;
  char* section_c;
  char* key_c;
  char* res;
  section_sz = sizeStringDefine(section_ml) + 1;
  key_sz = sizeStringDefine(key_ml) + 1;
  section_c = (char*)Ns_Malloc(section_sz);
  key_c = (char*)Ns_Malloc(key_sz);
  convertStringToC(section_ml, section_c, section_sz, (int)&exn_OVERFLOW);
  convertStringToC(key_ml, key_c, key_sz, (int)&exn_OVERFLOW);
  res = Ns_ConfigGetValueExact(section_c, key_c);
  if ( res == NULL ) {
    Ns_Free(section_c);
    Ns_Free(key_c);
    return (StringDesc*)NULL;
  }
  return convertStringToML(rAddr, res);
}

// ML: conn -> string
StringDesc* 
nssml_ConnUrl(int rAddr, Ns_Conn* conn)
{
  return convertStringToML(rAddr, conn->request->url);
}

// ML: string -> string ptr_option
StringDesc*
nssml_FetchUrl(int rAddr, StringDesc *url)
{
  int sz;
  char* url_c;
  Ns_DString ds;
  StringDesc* res;
  sz = sizeStringDefine(url) + 1;
  url_c = (char*)Ns_Malloc(sz);
  convertStringToC(url, url_c, sz, (int)&exn_OVERFLOW);
  Ns_DStringInit(&ds);
  if ( Ns_FetchURL(&ds, url_c, NULL) != NS_OK ) {
    Ns_DStringFree(&ds);
    Ns_Free(url_c);
    return (StringDesc*)NULL;
  }
  res = convertStringToML(rAddr, Ns_DStringValue(&ds));
  Ns_DStringFree(&ds);
  Ns_Free(url_c);
  return res;
}

// Mutex symbol to protect caching operations
static Ns_Mutex cacheLock;

// Function for freeing cached values
static void
nssml_CacheValueFree(void *p)
{
  Ns_Free(p);
}

// ML: string -> cache ptr_option
Ns_Cache*
nssml_CacheFind(StringDesc* cacheName)
{
  char* cacheName_c;
  int sz;
  Ns_Cache* res; 
  sz = sizeStringDefine(cacheName) + 1;
  cacheName_c = (char*)Ns_Malloc(sz);
  convertStringToC(cacheName, cacheName_c, sz, (int)&exn_OVERFLOW);
  res = Ns_CacheFind(cacheName_c);
  Ns_Free(cacheName_c);
  return res;
}

// ML: string * int -> cache ptr_option
Ns_Cache*
nssml_CacheCreate(StringDesc* cacheName, int timeout)
{
  char* cacheName_c;
  int sz;
  Ns_Cache* res; 
  sz = sizeStringDefine(cacheName) + 1;
  cacheName_c = (char*)Ns_Malloc(sz);
  convertStringToC(cacheName, cacheName_c, sz, (int)&exn_OVERFLOW);
  res = Ns_CacheCreate(cacheName_c, TCL_STRING_KEYS, timeout, nssml_CacheValueFree);
  Ns_Free(cacheName_c);
  return res;
}
  
// ML: string * int -> cache ptr_option
Ns_Cache*
nssml_CacheCreateSz(StringDesc* cacheName, int maxSize)
{
  char* cacheName_c;
  int sz;
  Ns_Cache* res; 
  sz = sizeStringDefine(cacheName) + 1;
  cacheName_c = (char*)Ns_Malloc(sz);
  convertStringToC(cacheName, cacheName_c, sz, (int)&exn_OVERFLOW);
  res = Ns_CacheCreateSz(cacheName_c, TCL_STRING_KEYS, maxSize, nssml_CacheValueFree);
  Ns_Free(cacheName_c);
  return res;
}

// ML: cache * string * string -> int
int
nssml_CacheSet(Ns_Cache* cache, StringDesc* key, StringDesc* value)
{
  char *key_c;
  char *value_c;
  int sz;
  int new;
  Ns_Entry *ePtr;
  sz = sizeStringDefine(key) + 1;
  key_c = (char*)Ns_Malloc(sz);
  convertStringToC(key, key_c, sz, (int)&exn_OVERFLOW);
  sz = sizeStringDefine(value) + 1;
  value_c = (char*)Ns_Malloc(sz);
  convertStringToC(value, value_c, sz, (int)&exn_OVERFLOW);
  Ns_CacheLock(cache);
  ePtr = Ns_CacheCreateEntry(cache, key_c, &new);
  Ns_CacheSetValueSz(ePtr, value_c, sz);
  Ns_CacheUnlock(cache);
  return new;
}

// ML: cache * string -> string ptr_option
StringDesc*
nssml_CacheGet(int rAddr, Ns_Cache* cache, StringDesc* key)
{
  char *key_c;
  int sz;
  char *value_c;
  Ns_Entry *ePtr;
  StringDesc* res;
  sz = sizeStringDefine(key) + 1;
  key_c = (char*)Ns_Malloc(sz);
  convertStringToC(key, key_c, sz, (int)&exn_OVERFLOW);
  Ns_CacheLock(cache);
  ePtr = Ns_CacheFindEntry(cache, key_c);
  if ( ePtr == NULL ) {
    goto none;
  }
  value_c = (char*)Ns_CacheGetValue(ePtr);
  if ( value_c == NULL ) {
    goto none;
  }
  res = convertStringToML(rAddr, value_c);
  Ns_CacheUnlock(cache);
  Ns_Free(key_c);
  return res;
 none:
  Ns_CacheUnlock(cache);
  Ns_Free(key_c);
  return NULL;
}

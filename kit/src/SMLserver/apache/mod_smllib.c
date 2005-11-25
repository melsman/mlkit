
// Includes /*{{{*/
#include "../../Runtime/String.h"
#include "../../Runtime/List.h"
#include "httpd.h"
#include "http_log.h"
#include "http_protocol.h"
#include "http_request.h"
#include "http_core.h"
#include "apr_file_io.h"
#include "apr_file_info.h"
#include "apr_uri.h"
#include "mod_sml.h"
#include "plog.h"
#include "../../Runtime/Exception.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include "sched.h"
#include <netdb.h>  /*}}} */

// Defines /*{{{*/
#define BUFFERSIZE 10	/*}}} */
#define FORM 1
#define GET 0

// Structs /*{{{*/
struct stringbuffer
{
  struct stringbuffer *next;
  int size;
  char stringbuf[BUFFERSIZE];
};			/*}}} */


// ML: int * string * int * request_rec -> status
int
apsml_returnHtml (int status, char *s, int len, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
  {
    ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
	    "apsml_returnHtml called without a connection");
    raise_exn ((int) &exn_OVERFLOW);
    return 0;
  }
  request_rec *r = rd->request;
  if (!(status == -1))
  {
    r->status_line = ap_get_status_line (status);
    r->status = status;
  }
  apr_off_t content_length = len;
  ap_set_content_length (r, content_length);
  r->content_type = "text/html";
//      ap_log_rerror(__FILE__,__LINE__, LOG_DEBUG, 0, r, 
//                      "apsml_returnHtml C: status == %i, len == %i, data: %s", status, len, s);
  ap_rputs (s, r);
  return 0;
}				/*}}} */

int
apsml_returnFile (int status, char *mt, char *file, request_data * rd)	//{{{
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_returnFile called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  request_rec *r = rd->request;
  apr_size_t amountsend;
  if (status != -1)
    {
      r->status_line = ap_get_status_line (status);
      r->status = status;
    }
  apr_file_t *fd;
  apr_finfo_t finfo;
  apr_status_t s = apr_file_open (&fd, file, APR_READ | APR_SENDFILE_ENABLED,
				  0, r->pool);
  if (APR_STATUS_IS_SUCCESS (s))
    {
      r->content_type = mt;
      apr_file_info_get (&finfo, APR_FINFO_MTIME | APR_FINFO_SIZE, fd);
      ap_set_content_length (r, finfo.size);
      ap_update_mtime (r, finfo.mtime);
//              ap_send_http_header(r);
      ap_send_fd (fd, r, 0, finfo.size, &amountsend);
      return 0;
    }
  ap_log_rerror (__FILE__, __LINE__, LOG_ERR, 0, r,
		 "SEND_FILE went wrong: %s", file);
  return 1;
}				//}}}

int
apsml_returnRedirect (int i, char *url, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_returnRedirect called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  request_rec *r = rd->request;
  apr_table_set (r->headers_out, "Location", url);
  int status = i;
  if (status == -1)
    {
      status = HTTP_TEMPORARY_REDIRECT;
    }
  r->status_line = ap_get_status_line (status);
  r->status = status;
//      ap_send_http_header(r);
  return 0;
}				/*}}} */

void
apsml_add_headers_out (char *key, int key_size, char *value, int value_size, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_add_headers_out called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return;
    }
  request_rec *r = rd->request;
//      ap_log_rerror(__FILE__,__LINE__, LOG_NOTICE, 0, r, "key,value, key_size, value_size: %s, %s, %i, %i", key, value, key_size, value_size);
  char *kv = (char *) apr_palloc (r->pool, key_size + 1 + value_size + 1);
  strncpy (kv, key, key_size + 1);
  strncpy (kv + key_size + 1, value, value_size);
  kv[key_size] = 0;
  kv[key_size + value_size + 1] = 0;
//      ap_log_rerror(__FILE__,__LINE__, LOG_NOTICE, 0, r, "key,value: %s, %s", kv, kv+key_size+1);
  apr_table_add (r->headers_out, kv, kv + key_size + 1);
  return;
}				/*}}} */

String
apsml_location (Region rAddr, int rd1)	/*{{{ */
{
  request_data *rd = (request_data *) rd1;
  request_rec *r = rd->request;
  ap_log_rerror (__FILE__, __LINE__, LOG_EMERG, 0, r,
		 "apsml_location does not work");
  int length_scheme = strlen (r->parsed_uri.scheme);
  int length_hostinfo = strlen (r->parsed_uri.hostinfo);
  char *location = (char *) apr_pcalloc (r->pool,
					 length_hostinfo + length_scheme + 1);
  strcpy (location, r->parsed_uri.scheme);
  strcpy (location, r->parsed_uri.hostinfo);
  return convertStringToML (rAddr, location);
}				/*}}} */

void
apsml_log (int logSeverity, StringDesc * str, request_data * rd, int exn)	/*{{{ */
{
//              request_data *rd = (request_data *) rd1;
//              ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, "Logging");
  int logS = convertIntToC (logSeverity);
  int stringSize = sizeStringDefine (str) + 1;
  char *s = (char *) apr_palloc (rd->pool, stringSize);
  if (s == NULL)
    raise_exn (exn);
  convertStringToC (str, s, stringSize, exn);
  if (rd->request)
    {
      ap_log_rerror (__FILE__, __LINE__, logS, 0, rd->request, "%s", s);
    }
  else
    {
      ap_log_error (__FILE__, __LINE__, logS, 0, rd->server, "%s", s);
    }
  return;
}				/*}}} */

// ML: request_rec -> int
int
apsml_getport (request_data * rd)	/*{{{ */
{
  // request_rec
  return convertIntToML (ap_get_server_port (rd->request));
  //return convertIntToML(rd->server->addrs->host_port);
}				/*}}} */

// ML: request_rec -> string
String
apsml_gethost (Region rAddr, int rd1)	/*{{{ */
{
  // request_rec
  return convertStringToML (rAddr,
			    (char *)
			    ap_get_local_host (((request_data *) rd1)->pool));
}				/*}}} */

// ML: request_rec -> string
String
apsml_getserver (Region rAddr, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_getserver called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  // request_rec
  return convertStringToML (rAddr, (char *) ap_get_server_name (rd->request));
}				/*}}} */

// ML: request_rec -> string list
int
apsml_geturl (Region rListAddr, Region rStringAddr, request_data * rd)	/*{{{ */
{
  request_rec *tmp;
  int *pair, *cons, *temp_pair, res;
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_geturl called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  allocRecordML(rListAddr, 2, pair);
  first(pair) = (int) convertStringToML (rStringAddr, rd->request->uri);
  makeCONS(pair,cons);
  res = (int) cons;
  tmp = rd->request;
  while (tmp->prev) 
  {
    tmp = tmp->prev;
    allocRecordML(rListAddr,2,temp_pair);
    first(temp_pair) = (int) convertStringToML(rStringAddr, tmp->uri);
    makeCONS(temp_pair,cons);
    second(pair) = (int) cons;
    pair = temp_pair;
  }
  makeNIL(cons);
  second(pair) = (int) cons;
  // request_rec
  return res;
}				/*}}} */

// ML: request_rec -> string
String
apsml_getpeer (Region rAddr, request_data * rd)	/*{{{ */
{
  // request_rec
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_getpeer called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  return convertStringToML (rAddr, rd->request->connection->remote_ip);
}				/*}}} */

// ML: request_rec -> string
String
apsml_PageRoot (Region rAddr, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_PageRoot called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  return convertStringToML (rAddr, (char *) ap_document_root (rd->request));
}				/*}}} */

// ML: () -> string ptr_option
String
apsml_getQueryData (Region rAddr, int maxsize, int type, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_getQueryData called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  request_rec *r = rd->request;
//      ap_log_rerror(__FILE__, __LINE__, LOG_NOTICE, 0, rd->request, "apsml: getFormData C init");
  if (type == GET)
    {
//      ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, r, "apsml get: %s",
//		    r->args);
      if (r->args)
	return convertStringToML (rAddr, r->args);
//              ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, r, "apsml: getFormData C get NULL");
      return (String) NULL;
    }
  // else (type == FORM)
  struct stringbuffer *buf;
  struct stringbuffer *startbuf;
  struct stringbuffer *nextbuf;
  char *finalstring;
  int rds;
  int totalsize = 0;
  maxsize = (maxsize == -1) ? 0x7FFFFFFF : maxsize;
//      ap_log_rerror(__FILE__, __LINE__, LOG_NOTICE, 0, rd->pool, "setup client form date retrievel");
  if (ap_setup_client_block (r, REQUEST_CHUNKED_DECHUNK) == OK)
    {
      ap_should_client_block (r);
      buf = (struct stringbuffer *) apr_palloc (rd->pool, sizeof (struct stringbuffer));
      buf->next = NULL;
      startbuf = buf;
      while ((rds =
	      ap_get_client_block (rd->request, buf->stringbuf,
				   BUFFERSIZE)) != 0)
	{
	  if (rds == -1)
	    return (String) NULL;
	  buf->size = rds;
	  totalsize += rds;
	  nextbuf =
	    (struct stringbuffer *) apr_palloc (rd->pool, sizeof (struct stringbuffer));
	  nextbuf->next = NULL;
	  buf->next = nextbuf;
	  if (totalsize > maxsize)
	    {
	      buf->size = rds - (totalsize - maxsize);
	      totalsize = maxsize;
	      ap_log_rerror (__FILE__, __LINE__, LOG_WARNING, 0, rd->request,
			     "apsml: User tried to send file larger than maxfilesize");
	      break;
	    }
	  buf = nextbuf;
	}
      buf = startbuf;
      finalstring = (char *) apr_palloc (rd->pool, totalsize + 1);
      int count = 0;
      while (buf->next != NULL)
	{
	  memmove (finalstring + count, buf->stringbuf, buf->size);
	  count += buf->size;
	  buf = buf->next;
	}
      *(finalstring + totalsize) = 0;
//      ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, rd->request,
//		     "apsml formdata: size==%i", totalsize);
//      ap_log_rerror (__FILE__, __LINE__, LOG_DEBUG, 0, rd->request,
//		     "apsml formdata: size==%i, data: %s", totalsize, finalstring);
      return convertBinStringToML (rAddr, totalsize, finalstring);
    }
  else
    return (String) NULL;
}				/*}}} */

// ML () -> set
int
apsml_setCreate (request_data * rd)	/*{{{ */
{
//      ap_log_error(__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "apsml_setCreate");
  apr_table_t *t = apr_table_make (rd->pool, 5);
  return (int) t;
}				/*}}} */

// ML: set * string -> string ptr_option
String
apsml_setGet (Region rAddr, apr_table_t * set, String key)	/*{{{ */
{
  char *res_c = (char *) apr_table_get (set, &(key->data));
  if (res_c == NULL)
    {
      return (String) NULL;
    }
  return convertStringToML (rAddr, res_c);
}				/*}}} */

// ML: string ptr_option -> bool
int
apsml_isNullString (String s)	/*{{{ */
{
  if (s == NULL)
    return mlTRUE;
  else
    return mlFALSE;
}				/*}}} */

int
apsml_setcount (void *c, const char *a, const char *b)	/*{{{ */
{
  int *count = (int *) c;
  (*count)++;
  return 1;
}				/*}}} */

// ML: set -> int
int
apsml_SetSize (apr_table_t * set)	/*{{{ */
{
  apr_array_header_t *ah = (apr_array_header_t *) apr_table_elts (set);
  return ah->nelts;
}				/*}}} */

int
apsml_setUnique (apr_table_t * set, char *key)	/*{{{ */
{
  int count = 0;
  apr_table_do (apsml_setcount, &count, set, key);
  return count;
}				/*}}} */

// ML: set * int -> string ptr_option
String
apsml_SetKey (Region rAddr, apr_table_t * set, int i)	/*{{{ */
{
  apr_array_header_t *ah = (apr_array_header_t *) apr_table_elts (set);
  if (ah->nelts > i)
    {
      apr_table_entry_t *fields = (apr_table_entry_t *) ah->elts;
      return convertStringToML (rAddr, (fields + i)->key);
    }
  return (String) NULL;
}				/*}}} */

// ML: set * int -> string ptr_option
String
apsml_SetValue (Region rAddr, apr_table_t * set, int i)	/*{{{ */
{
  apr_array_header_t *ah = (apr_array_header_t *) apr_table_elts (set);
  if (ah->nelts > i)
    {
      apr_table_entry_t *fields = (apr_table_entry_t *) ah->elts;
      return convertStringToML (rAddr, (fields + i)->val);
    }
  return (String) NULL;
}				/*}}} */

// 
void 
apsml_reg_schedule(int first_time, int interval, int type, int pair, request_data *rd)/*{{{*/
{
  ssize_t tmp, packagelength;
  int e, l, serversize;
  String tmpstr;
  char *c, *tmpptr, *server = NULL;
  tmpstr = (String) elemRecordML(pair, 0);
  c = &(tmpstr->data);
  l = sizeStringDefine (tmpstr);
  tmpstr = (String) elemRecordML(pair,1);
  server = &(tmpstr->data);
  serversize = sizeStringDefine(tmpstr);
  l += serversize+1;
  schedHeader *sh = (schedHeader *) malloc(sizeof(schedHeader) + l+1);
  sh->first = first_time;
  sh->port = elemRecordML(pair,2);
  sh->interval = interval;
  sh->type = type;
  sh->serverlength = serversize;
  tmpptr = (char *) (sh+1);
  if (server)
  {
    strcpy(tmpptr, server);
  }
  tmpptr[serversize] = 0;
  tmpptr += serversize + 1;
  strcpy (tmpptr, c);
  sh->length = l;
  packagelength = sh->length + sizeof(schedHeader);
  apr_global_mutex_lock(rd->ctx->sched.lock);
  tmp = 0;
  e = 0;
  ap_log_error(__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, 
      "schedule : %s, first: %d, interval: %d, type %d", c, first_time, interval, type);
  while (e < packagelength)
  {
    ap_log_error(__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "schedule: e:%d", e);
    tmp = write(rd->ctx->sched.input, sh+e, packagelength-e);
    if (tmp == -1)
    {
      e = errno;
      ap_log_error(__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, 
          "schedule reg went bad: %s", strerror(e));
      apr_global_mutex_unlock(rd->ctx->sched.lock);
      free(sh);
      return;
    }
    e += tmp;
    packagelength -= tmp;
  }
  ap_log_error(__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "sent this %s", c);
  apr_global_mutex_unlock(rd->ctx->sched.lock);
  free (sh);
  return;
}/*}}}*/

// ML: unit -> set
int
apsml_headers (int rd1)		/*{{{ */
{
  request_data *rd = (request_data *) rd1;
  request_rec *r = rd->request;
//  ap_log_rerror(__FILE__, __LINE__, LOG_DEBUG, 0, rd->request, "apsml_headers");
  if (r->headers_in)
  {
//                ap_log_error(__FILE__, __LINE__, LOG_DEBUG, 0, rd->request, "apsml_headers in exists");
    return (int) r->headers_in;
  }
  else
  {
//                ap_log_error(__FILE__, __LINE__, LOG_DEBUG, 0, rd->request, "apsml_headers in creating");
    return apsml_setCreate (rd);
  }
}				/*}}} */

/* 
 * Encoding and decoding of application/x-www-form-urlencoded data follow the recipe 
 * in http://www.w3.org/MarkUp/html-spec/html-spec_8.html
 *
 * Encoding:
 *
 *    1. The form field names and values are escaped: space characters are
 *    replaced by `+', and then reserved characters are escaped as per [URL];
 *    that is, non-alphanumeric characters are replaced by `%HH', a percent
 *    sign and two hexadecimal digits representing the ASCII code of the
 *    character. Line breaks, as in multi-line text field values, are
 *    represented as CR LF pairs, i.e. `%0D%0A'.  
 *
 *    2. The fields are listed in the order they appear in the document with
 *    the name separated from the value by `=' and the pairs separated from
 *    each other by `&'. Fields with null values may be omitted. In particular,
 *    unselected radio buttons and checkboxes should not appear in the encoded
 *    data, but hidden fields with VALUE attributes present should. (26) 
 *
 * URL encoding described in RFC 1630
 *
 * */

// ML: string -> string
String
apsml_encodeUrl (Region rAddr, String str, request_data * rd)	/*{{{ */
{
// It seems like ap_escape_url cannot handle + 
  char *src = &(str->data);
//      ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, "apsml: apsml_encodeUrl C1: %s", src);
  int i;
  char *d = (char *) apr_palloc (rd->pool, sizeStringDefine(str) + 1);
  for (i = 0; i < sizeStringDefine(str); i++)
  {
    switch (src[i])
    {
    case ' ':
      d[i] = '+';
      break;
    case '+':
      d[i] = ' ';
      break;
    default:
      d[i] = src[i];
      break;
    }
  }
  d[sizeStringDefine(str)] = 0;
//      ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, "apsml: apsml_encodeUrl C2: %s", src);
  char *dst = ap_escape_uri (rd->pool, d);
  int a = 0;
  for (i = 0; dst[i]; i++, a++)
  {
    if (a >= 2 && dst[i - 2] == '%' && dst[i - 1] == '2' && dst[i] == '0')
    {
      dst[i] = 'B';
      a = -1;
    }
  }
//      ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, "apsml: apsml_encodeUrl C4: %s", dst);
  return convertStringToML (rAddr, dst);
}				/*}}} */

// ML: string -> string
String
apsml_decodeUrl (Region rAddr, String str, request_data * rd)	/*{{{ */
{
  char *s = &(str->data);
//      ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, "apsml: apsml_decodeUrl C: %s", s);
// It seems like ap_unescape_url cannot handle spaces
  char *to = (char *) apr_palloc (rd->pool, sizeStringDefine(str) + 1);
  char p1;
  int i;
  for (i = 0; i < sizeStringDefine(str); i++)
  {
    p1 = s[i];
    if (p1 == '+')
    {
      to[i] = ' ';
    }
    else
    {
      to[i] = p1;
    }
  }
  to[sizeStringDefine(str)] = 0;
//      ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, "apsml: apsml_decodeUrl C1: %s", to);
  ap_unescape_url (to);
//      ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, "apsml: apsml_decodeUrl C2: %s", to);
  return convertStringToML (rAddr, to);
}				/*}}} */

// ML: request_rec -> string
String
apsml_method (Region rAddr, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_method called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  return convertStringToML (rAddr, (char *) rd->request->method);
}				/*}}} */

// ML: request_rec -> string
String
apsml_scheme (Region rAddr, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_method called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  return convertStringToML (rAddr, (char *) ap_run_http_method(rd->request));
}				/*}}} */

// ML: request_rec -> int 
long
apsml_contentlength (request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_method called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return 0;
    }
  return rd->request->clength;
}				/*}}} */

// ML: request_rec -> bool
int
apsml_hasconnection (int rd1)	/*{{{ */
{
  request_data *rd = (request_data *) rd;
  if (rd->request)
    return 1;
  return 0;
}				/*}}} */

// ML: string * request_rec -> unit
void
apsml_setMimeType (char *s, int s_size, request_data * rd)	/*{{{ */
{
  if (rd->request == 0)
    {
      ap_log_error (__FILE__, __LINE__, LOG_WARNING, 0, rd->server,
		    "apsml_method called without a connection");
      raise_exn ((int) &exn_OVERFLOW);
      return;
    }
  char *m = (char *) apr_palloc (rd->pool, s_size + 1);
  strncpy (m, s, s_size);
  m[s_size] = 0;
  rd->request->content_type = m;
  return;
}				/*}}} */

request_rec *
apsml_GetReqRec (int rd1)	/*{{{ */
{
  request_data *rd = (request_data *) rd1;
  return rd->request;
}				/*}}} */

#define SPECIALKEYCONF(name, variable) \
  if (!strcmp(key, name)) \
  { \
    extraString = (String) extraval; \
    if (variable) free(variable); \
    variable = (char *) malloc(sizeStringDefine(extraString)+1); \
    if (!variable) \
    { \
      variable = NULL; \
      apr_thread_rwlock_unlock (rd->ctx->conftable->rwlock); \
      return; \
    } \
    strcpy(variable, &(extraString->data)); \
  }

// ML: String * String -> unit
void
apsml_confinsert (String k, String v, int extraval, request_data * rd)	/*{{{ */
{
//  String extraString;
  if (rd->ctx->initDone) raise_exn ((int) &exn_OVERFLOW);
  keyNhash *kn = (keyNhash *) apr_palloc (rd->ctx->conftable->pool,
					  sizeof (keyNhash) + sizeStringDefine(v) +
					  sizeStringDefine(k) + 2);
  char *key = (char *) (kn + 1);
  char *value = key + (sizeStringDefine(k) + 1);
  kn->key = key;
  strncpy (key, &(k->data), sizeStringDefine(k));
  key[sizeStringDefine(k)] = 0;
  kn->hash = charhashfunction (key);
  strncpy (value, &(v->data), sizeStringDefine(v));
  value[sizeStringDefine(v)] = 0;
  apr_thread_rwlock_wrlock (rd->ctx->conftable->rwlock);
  if (hashupdate (rd->ctx->conftable->ht, kn, value) != hash_OK)
  {
    ap_log_error (__FILE__, __LINE__, LOG_EMERG, 0, rd->server,
	   "apsml_confinsert error, Out of memory");
  }
  apr_thread_rwlock_unlock (rd->ctx->conftable->rwlock);
  return;
}				/*}}} */

// ML: String -> string_pty
String
apsml_conflookup (Region rAddr, String k, request_data * rd)	/*{{{ */
{
  char *value;
  String s;
  apr_thread_rwlock_rdlock (rd->ctx->conftable->rwlock);
  keyNhash kn;
  kn.key = &(k->data);
  kn.hash = charhashfunction (kn.key);
//      ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, rd->server,
//		    "apsml_conflookup key:%s",kn.key);
  if (hashfind (rd->ctx->conftable->ht, &kn, (void **) &value) != hash_OK)
    {
      s = (String) NULL;
    }
  else
    {
      s = convertStringToML (rAddr, value);
    }
  apr_thread_rwlock_unlock (rd->ctx->conftable->rwlock);
  return s;
}				/*}}} */

// ML: int -> string
String
apsml_errnoToString(Region rAddrString, int i)/*{{{*/
{
  return convertStringToML(rAddrString, strerror(i));
}/*}}}*/

// ML: unit -> int
int
apsml_getuptime(request_data *rd)/*{{{*/
{
  time_t n = time(NULL);
  return convertIntToML(n - rd->ctx->starttime);
}/*}}}*/

int
apsml_getpid(request_data *rd)/*{{{*/
{
  return convertIntToML(rd->ctx->pid);
}/*}}}*/

static char *
tail (char *p)/*{{{*/
{
  while (*p) p++;
  return p;
}/*}}}*/

static String 
collectbuf(Region sAddr, struct stringbuffer *buf, request_data *rd)/*{{{*/
{
  struct stringbuffer *startbuf;
  struct stringbuffer *nextbuf;
  char *finalstring;
  unsigned long size;
  String s;
  int i;
  size = 0;
  startbuf = buf;
  if (buf == NULL) return NULL;
  while (buf) 
  {
    size += buf->size;
    buf = buf->next;
  }
  finalstring = (char *) malloc(size+1);
  buf = startbuf;
  if (finalstring == NULL)
  {
    for (buf = startbuf; buf; ) 
    {
      nextbuf = buf->next;
      free(buf);
      buf = nextbuf;
    }
    return NULL;
  }
  for (buf = startbuf, i = 0; buf;)
  {
    memcpy(finalstring+i, buf->stringbuf, buf->size);
    i += buf->size;
    nextbuf = buf->next;
    free(buf);
    buf = nextbuf;
  }
  finalstring[i] = 0;
  s = convertStringToML(sAddr, finalstring);
  free(finalstring);
  return s;
}/*}}}*/

// ML: string * string * int -> string_ptr
String 
apsml_getpage(Region sAddr, String server1, String page1, int pair)/*{{{*/
{
  char *server, *page;
  unsigned short port;
  struct addrinfo addr_hint, *addr, *myaddr;
  struct stringbuffer *buf, *startbuf, *nextbuf;
  char *head, *ptr;
  fd_set writeset, readset, *wset;
  struct timeval tv;
  char sport[20];
  int sock, c;
  time_t now, curtime;
  int tmp, size, left, right;
  int timeout;
  request_data *rd;
  server = &(server1->data);
  page = &(page1->data);
  port = elemRecordML(pair,0);
  rd = (request_data *) elemRecordML(pair,2);
  timeout = elemRecordML(pair,1);
  size = sizeStringDefine(server) + sizeStringDefine(page) + 200;
  snprintf(sport, 19, "%d", port);
  sport[19] = 0;
  head = (char *) malloc (size);
  if (head == NULL) return NULL;
  memset(&addr_hint, 0, sizeof(struct addrinfo));
  ptr = head;
  strcpy (ptr, "GET ");
  ptr = tail (ptr);
  strcpy (ptr, page);
  ptr = tail (ptr);
  strcpy (ptr, " HTTP/1.1\r\nHost: ");
  ptr = tail (ptr);
  strcpy (ptr, server);
  ptr = tail (ptr);
  strcpy (ptr, "\r\nConnection: close\r\n\r\n");
  startbuf = (struct stringbuffer *) malloc(sizeof(struct stringbuffer));
  if (startbuf == NULL) 
  {
    free(head);
    return NULL;
  }
  startbuf->next = NULL;
  startbuf->size = 0;
  buf = startbuf;
  addr_hint.ai_family = PF_UNSPEC;
  addr_hint.ai_socktype = SOCK_STREAM;
  if (getaddrinfo (server, sport, &addr_hint, &addr) != 0) 
  {
      ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
		    "getaddrinfo failed on server: %s", server);
    free(head);
    free(startbuf);
    return NULL;
  }
  myaddr = addr;
  sock = -1;
  c = -1;
  while (myaddr)
  {
    sock = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (sock == -1)
    {
      myaddr = myaddr->ai_next;
    }
    else 
    {
      c = connect(sock, myaddr->ai_addr, myaddr->ai_addrlen);
      if (c != 0)
      {
        close(sock);
        myaddr = myaddr->ai_next;
      }
      else break;
    }
  }
  freeaddrinfo (addr);
  if (sock == -1 || c == -1)
  {
      ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
		    "socket or connect failed on server: %s", server);
    free(head);
    free(startbuf);
    if (sock != -1) close(sock);
    return NULL;
  }
  ptr = head + size;
  left = strlen(head);
  right = 0;
  curtime = time(NULL) + timeout;
  wset = &writeset;
  while (1)
  {
    now = time(NULL);
    if (now > curtime)
    {
      ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
		    "connection timeout on server: %s", server);
      free(head);
      close(sock);
      return collectbuf(sAddr, startbuf,rd);
    }
    FD_ZERO(&readset);
    if (wset && left > 0)
    {
      FD_ZERO(wset);
      FD_SET(sock, wset);
    }
    else 
    {
      wset = NULL;
    }
    FD_SET(sock, &readset);
    tv.tv_sec = 60;
    tv.tv_usec = 0;
    tmp = select(sock + 1, &readset, wset, NULL, &tv);
    if (tmp == -1)
    {
      ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
		    "select failed: %s", server);
      free(head);
      close(sock);
      return collectbuf(sAddr, startbuf,rd);
    }
    if (FD_ISSET(sock, &readset))
    {
      if (BUFFERSIZE <= buf->size)
      {
        nextbuf = (struct stringbuffer *) malloc(sizeof(struct stringbuffer));
        if (nextbuf == NULL)
        {
          free(head);
          close(sock);
          return collectbuf(sAddr, startbuf,rd);
        }
        nextbuf->size = 0;
        nextbuf->next = NULL;
        buf->next = nextbuf;
        buf = nextbuf;
      }
      tmp = recv(sock, buf->stringbuf+buf->size, BUFFERSIZE - buf->size, MSG_DONTWAIT);
      if (tmp == -1)
      {
        tmp = errno;
        if (tmp == EAGAIN) continue;
        ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
            "socket recv error: %s", strerror(tmp));
        free(head);
        close(sock);
        return collectbuf(sAddr, startbuf,rd);
      }
      if (tmp == 0)
      {
        free (head);
        close(sock);
        return collectbuf(sAddr, startbuf,rd);
      }
      buf->size += tmp;
    }
    if (wset && FD_ISSET(sock, wset))
    {
      tmp = send(sock, head+right, left, MSG_DONTWAIT);
      if (tmp == -1)
      {
        tmp = errno;
        if (tmp == EAGAIN || tmp == EWOULDBLOCK) continue;
        ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server,
            "socket send error: %s", strerror(tmp));
        free(head);
        close(sock);
        return collectbuf(sAddr, startbuf,rd);
      }
      left -= tmp;
      right += tmp;
    }
  }
  return collectbuf(sAddr, startbuf, rd);
}/*}}}*/

void *
apsmlGetDBData (int i, request_data *rd)/*{{{*/
{
  struct db_t *tmp = rd->ctx->db;
  apr_thread_mutex_lock(rd->ctx->dblock);
  for (;tmp; tmp = tmp->next)
  {
    if (i == tmp->num)
    {
      apr_thread_mutex_unlock(rd->ctx->dblock);
      return tmp->dbspec;
    }
  }
  apr_thread_mutex_unlock(rd->ctx->dblock);
  return NULL;
}/*}}}*/

int
apsmlPutDBData (int i, void *data, void child_init(void *, int, apr_pool_t *, server_rec *), /*{{{*/
                                   void tmp_shutdown(void *, server_rec *),
                                   void req_cleanup(void *, void *),
                                   request_data *rd)
{
  struct db_t *tmp, *prev_tmp;
//  void *tmp_data;
  apr_thread_mutex_lock(rd->ctx->dblock);
  tmp = rd->ctx->db;
  prev_tmp = NULL;
  for (;tmp;tmp->next)
  {
    if (tmp->num == i)
    {
      apr_thread_mutex_unlock(rd->ctx->dblock);
      return 1;
    }
    prev_tmp = tmp;
  }
  tmp = (struct db_t *) malloc (sizeof (struct db_t));
  tmp->next = NULL;
  tmp->dbspec = data;
  tmp->num = i;
  tmp->child_init = child_init;
  tmp->tmp_shutdown = tmp_shutdown;
  tmp->req_cleanup = req_cleanup;
  if (prev_tmp) 
  {
    prev_tmp->next = tmp;
  }
  else 
  {
    rd->ctx->db = tmp;
  }
  apr_thread_mutex_unlock(rd->ctx->dblock);
  return 0;
}/*}}}*/

void 
dblog1 (const request_data *rd, const char *data)/*{{{*/
{
  ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, data);
}/*}}}*/

void 
dblog2 (const request_data *rd, const char *data, const int num)/*{{{*/
{
  ap_log_error(__FILE__, __LINE__, LOG_NOTICE, 0, rd->server, "%s %d", data, num);
}/*}}}*/

void *
getDbData(int num, request_data *rd)/*{{{*/
{
  struct request_db *tmp = rd->dbdata;
  while (tmp)
  {
    if (num == tmp->num) return tmp->dbdata;
    tmp = tmp->next;
  }
  return NULL;
}/*}}}*/

void
removeDbData(int num, request_data *rd)/*{{{*/
{
  struct request_db *tmp = rd->dbdata, *tmp2 = NULL;
  while (tmp)
  {
    if (num == tmp->num)
    {
      tmp = tmp->next;
      break;
    }
    tmp2 = tmp;
    tmp = tmp->next;
  }
  if (tmp2)
  {
    tmp2->next = tmp;
  }
  else 
  {
    rd->dbdata = tmp;
  }
  return;
}/*}}}*/

int 
putDbData(int num, void *dbdata, request_data *rd)/*{{{*/
{
  struct request_db *tmp = rd->dbdata, *tmp2;
  for(tmp2 = NULL; tmp; tmp2 = tmp, tmp = tmp->next)
  {
    if (tmp->num == num) return 1;
  }
  tmp = (struct request_db *) malloc(sizeof (struct request_db));
  if (!tmp) return 2;
  tmp->next = 0;
  tmp->num = num;
  tmp->dbdata = dbdata;
  if (tmp2) 
  {
    tmp2->next = tmp;
  }
  else
  {
    rd->dbdata = tmp;
  }
  return 0;
}/*}}}*/

/*
String
fromCtoMLstring(Region r, char *c)
{
  if (c) return convertStringToML(r,c);
  return NULL;
}
*/
/*
static int find_ct(request_rec *r);


// ML: string -> string
String
apsml_GetMimeType(Region rAddr, String s, int rr)
{
  apr_pool_t *p = ((request_rec *) rr)->pool;
  request_rec *r = (request_rec *) apr_pcalloc (p,sizeof(request_rec));
  r->filename = &(s->data);
  apr_status_t status = 
	  apr_stat(&(r->finfo), r->filename, APR_FINFO_PROT | APR_FINFO_OWNER 
			  					| APR_FINFO_IDENT | APR_FINFO_MIN, p);
  find_ct(r);
  if ( r->content_type == NULL ) 
    {
	ap_log_rerror(__FILE__, __LINE__, LOG_NOTICE, 0, ((request_rec *) rr), 
			"nssml_GetMimeType problem - returning empty string");
      r->content_type = "";
    }
  return convertStringToML(rAddr, (char *) r->content_type);


}
*/

void
plog1s(char *s, void *ctx)/*{{{*/
{
  request_data *rd = (request_data *) ctx;
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "%s", s);
  return;
}/*}}}*/

void
plog2s(char *s, char *t, void *ctx)/*{{{*/
{
  request_data *rd = (request_data *) ctx;
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "%s%s", s, t);
  return;
}/*}}}*/

void
plog3s(char *s, char *t, char *r, void *ctx)/*{{{*/
{
  request_data *rd = (request_data *) ctx;
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "%s%s%s", s, t, r);
  return;
}/*}}}*/

void
plog4s(char *s, char *t, char *r, char *v, void *ctx)/*{{{*/
{
  request_data *rd = (request_data *) ctx;
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "%s%s%s%s", s, t, r, v);
  return;
}/*}}}*/

void
plog5s(char *s, char *t, char *r, char *v, char *w, void *ctx)/*{{{*/
{
  request_data *rd = (request_data *) ctx;
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "%s%s%s%s%s", s, t, r, v, w);
  return;
}/*}}}*/

void
plog4s1i(char *s, char *t, char *r, char *v, unsigned long w, void *ctx)/*{{{*/
{
  request_data *rd = (request_data *) ctx;
  ap_log_error (__FILE__, __LINE__, LOG_DEBUG, 0, rd->server, "%s%s%s%s%ld", s, t, r, v, w);
  return;
}/*}}}*/


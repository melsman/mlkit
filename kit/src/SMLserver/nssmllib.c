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

void
nssml_log(Ns_LogSeverity ls, StringDesc* s_ml) 
{
  char* s_c;
  int size = 1 + sizeString(s_ml);
  s_c = (char*)malloc(size);
  convertStringToC(s_ml, s_c, size, (int)&exn_OVERFLOW);
  Ns_Log(ls, s_c); 
  free(s_c);
  return;
}

int
nssml_ConnReturnHtml(Ns_Conn * c, int status, StringDesc* s_ml) 
{
  int res;
  char* s_c;
  int size = 1 + sizeString(s_ml);
  s_c = (char*)malloc(size);
  convertStringToC(s_ml, s_c, size, (int)&exn_OVERFLOW);
  res = Ns_ConnReturnHtml(c, status, s_c, strlen(s_c));
  free(s_c);
  return res;
}

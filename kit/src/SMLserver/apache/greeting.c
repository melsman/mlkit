
#include "greeting.h"
#include <ap_release.h>
#include <http_log.h>

void
sml_greeting(server_rec *s)
{
#if (AP_SERVER_MINORVERSION_NUMBER == 0)
  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s, "apsml: module compiled for " 
                                                      AP_SERVER_BASEPRODUCT " " AP_SERVER_MAJORVERSION "." AP_SERVER_MINORVERSION);
#else
  ap_log_error (__FILE__, __LINE__, LOG_NOTICE, 0, s, "apsml: module compiled for " 
                                                      AP_SERVER_BASEPRODUCT "/%d.%d",
                                                      AP_SERVER_MAJORVERSION_NUMBER, AP_SERVER_MINORVERSION_NUMBER);
#endif
}

/* ml-options.h
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Command-line argument processing.
 */

#ifndef _ML_OPTIONS_
#define _ML_OPTIONS_

/* maximum length of option and argument parts of command-line options */
#define MAX_OPT_LEN	64

extern bool_t isRuntimeOption (char *cmdLineArg, char *option, char **arg);
extern int GetSzOption (int scale, char *sz);

#endif /* !_ML_OPTIONS_ */


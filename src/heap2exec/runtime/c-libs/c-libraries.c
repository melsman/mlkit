/* c-libraries.c
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This is the home of the CLibrary table, C library initialization code,
 * and C function lookup code.  It is part of the run-time proper (not part
 * of libcfuns.a).
 */

#ifdef OPSYS_UNIX
#  include "ml-unixdep.h"	/* for the HAS_POSIX_LIBRARIES option flag */
#endif
#include "ml-base.h"
#include "ml-values.h"
#include "c-library.h"

#define C_LIBRARY(lib)  extern c_library_t lib;
#include "clib-list.h"
#undef C_LIBRARY

PVT c_library_t	*CLibs[] = {
#define C_LIBRARY(lib)	&lib,
#	include "clib-list.h"
#undef C_LIBRARY
	NIL(c_library_t *)
    };

/* InitCFunList:
 * Initialize the list of C functions callable from ML.
 */
void InitCFunList ()
{
    int             i, j, libNameLen;
    char	    *nameBuf;

    for (i = 0;  CLibs[i] != NIL(c_library_t *);  i++) {
	c_library_t	*clib = CLibs[i];
	cfunc_binding_t	*cfuns = CLibs[i]->cfuns;

	if (clib->initFn != NIL(clib_init_fn_t)) {
	  /* call the libraries initialization function */
	    (*(clib->initFn)) (0, 0/** argc, argv **/);
	}

      /* register the C functions in the C symbol table */
	libNameLen = strlen(clib->libName) + 2; /* incl "." and "\0" */
	for (j = 0;  cfuns[j].name != NIL(char *);  j++) {
	    nameBuf = NEW_VEC(char, strlen(cfuns[j].name) + libNameLen);
	    sprintf (nameBuf, "%s.%s", clib->libName, cfuns[j].name);
#ifdef INDIRECT_CFUNC
	    RecordCSymbol (nameBuf, PTR_CtoML(&(cfuns[j])));
#else
	    RecordCSymbol (nameBuf, PTR_CtoML(cfuns[j].cfunc));
#endif
	}
    }

} /* end of InitCFunList */

/* BindCFun:
 *
 * Search the C function table for the given function; return ML_unit, if
 * not found.
 * NOTE: eventually, we will raise an exception when the function isn't found.
 */
ml_val_t BindCFun (char *moduleName, char *funName)
{
    int		i, j;

/* SayDebug("BIND: %s.%s\n", moduleName, funName); */
    for (i = 0;  CLibs[i] != NIL(c_library_t *);  i++) {
	if (strcmp(CLibs[i]->libName, moduleName) == 0) {
	    cfunc_binding_t	*cfuns = CLibs[i]->cfuns;
	    for (j = 0;  cfuns[j].name != NIL(char *);  j++) {
		if (strcmp(cfuns[j].name, funName) == 0)
#ifdef INDIRECT_CFUNC
		    return PTR_CtoML(&(cfuns[j]));
#else
		    return PTR_CtoML(cfuns[j].cfunc);
#endif
	    }
	  /* here, we didn't find the library so we return ML_unit */
	    return ML_unit;
	}
    }

  /* here, we didn't find the library so we return ML_unit */
    return ML_unit;

} /* end of BindCFun */


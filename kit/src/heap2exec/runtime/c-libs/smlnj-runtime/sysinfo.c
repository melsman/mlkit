/* sysinfo.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 * General interface to query system properties.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

#if defined(OPSYS_UNIX)
#  include "ml-unixdep.h"  /* for OS_NAME */
#elif defined(OPSYS_WIN32)
#  define OS_NAME "Win32"
#endif

#define STREQ(s1, s2)	(strcmp((s1), (s2)) == 0)


#define FALSE_VALUE	"NO"
#define TRUE_VALUE	"YES"


/* _ml_RunT_sysinfo : string -> string option
 *
 * Current queries:
 *   "OS_NAME"
 *   "OS_VERSION"
 *   "HOST_ARCH"   
 *   "TARGET_ARCH"
 *   "HAS_SOFT_POLL"
 *   "HAS_MP"
 */
ml_val_t _ml_RunT_sysinfo (ml_state_t *msp, ml_val_t arg)
{
    char	*name = PTR_MLtoC(char, arg);
    ml_val_t	res;

    if (STREQ("OS_NAME", name))
	res = ML_CString(msp, OS_NAME);
    else if (STREQ("OS_VERSION", name))
	res = ML_CString(msp, "<unknown>");
    else if (STREQ("HOST_ARCH", name))
#if   defined(HOST_ALPHA32)
	res = ML_CString(msp, "ALPHA32");
#elif defined(HOST_HPPA)
	res = ML_CString(msp, "HPPA");
#elif defined(HOST_MIPS)
	res = ML_CString(msp, "MIPS");
#elif defined(HOST_M68)
	res = ML_CString(msp, "M68");
#elif defined(HOST_PPC)
	res = ML_CString(msp, "PPC");
#elif defined(HOST_RS6000)
	res = ML_CString(msp, "RS6000");
#elif defined(HOST_SPARC)
	res = ML_CString(msp, "SPARC");
#elif defined(HOST_X86)
	res = ML_CString(msp, "X86");
#else
	res = ML_CString(msp, "<unknown>");
#endif
    else if (STREQ("TARGET_ARCH", name))
#if   defined(TARGET_ALPHA32)
	res = ML_CString(msp, "ALPHA32");
#elif defined(TARGET_HPPA)
	res = ML_CString(msp, "HPPA");
#elif defined(TARGET_MIPS)
	res = ML_CString(msp, "MIPS");
#elif defined(TARGET_M68)
	res = ML_CString(msp, "M68");
#elif defined(TARGET_PPC)
	res = ML_CString(msp, "PPC");
#elif defined(TARGET_RS6000)
	res = ML_CString(msp, "RS6000");
#elif defined(TARGET_SPARC)
	res = ML_CString(msp, "SPARC");
#elif defined(TARGET_X86)
	res = ML_CString(msp, "X86");
#elif defined(TARGET_C)
	res = ML_CString(msp, "C");
#elif defined(TARGET_BYTECODE)
	res = ML_CString(msp, "BYTECODE");
#else
	res = ML_CString(msp, "<unknown>");
#endif
    else if (STREQ("HAS_SOFT_POLL", name))
#ifdef SOFT_POLL
	res = ML_CString(msp, TRUE_VALUE);
#else
	res = ML_CString(msp, FALSE_VALUE);
#endif
    else if (STREQ("HAS_MP", name))
#ifdef MP_SUPPORT
	res = ML_CString(msp, TRUE_VALUE);
#else
	res = ML_CString(msp, FALSE_VALUE);
#endif
    else
	return OPTION_NONE;

    OPTION_SOME(msp, res, res);

    return res;

} /* end of _ml_RunT_sysinfo */


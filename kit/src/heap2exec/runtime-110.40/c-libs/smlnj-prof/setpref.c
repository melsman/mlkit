/* setpref.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "cfun-proto-list.h"
#include "profile.h"

extern void EnableProfSignals (void);
extern void DisableProfSignals (void);

/* _ml_Prof_setpref : word array option -> unit
 *
 * Set the profile array reference; NONE means that there is no array.
 */
ml_val_t _ml_Prof_setpref (ml_state_t *msp, ml_val_t arg)
{
#ifdef OPSYS_UNIX
    bool_t	enabled = (ProfCntArray != ML_unit);
    int		i;

    if (arg != OPTION_NONE) {
	ProfCntArray = OPTION_get(arg);
	if (! enabled) {
	  /* add ProfCntArray to the C roots */
	    CRoots[NumCRoots++] = &ProfCntArray;
	  /* enable profiling signals */
	    EnableProfSignals ();
	}
    }
    else if (enabled) {
      /* remove ProfCntArray from the C roots */
	for (i = 0;  i < NumCRoots;  i++) {
	    if (CRoots[i] == &ProfCntArray) {
		CRoots[i] = CRoots[--NumCRoots];
		break;
	    }
	}
      /* disable profiling signals */
	DisableProfSignals ();
	ProfCntArray = ML_unit;
    }

    return ML_unit;
#else
    return RAISE_ERROR(msp, "time profiling not supported");
#endif

} /* end of _ml_Prof_setpref */


/* unix-prof.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * SML Profiling support for Unix.
 */
#include "ml-unixdep.h"
#include "signal-sysdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "profile.h"


/* The pointer to the heap allocated array of call counts.
 * When this pointer is ML_unit, then profiling is disabled.
 */
ml_val_t	ProfCntArray = ML_unit;

/* local routines */
PVT SigReturn_t ProfSigHandler ();


/* EnableProfSignals:
 */
void EnableProfSignals ()
{
    SIG_SetHandler (SIGVTALRM, ProfSigHandler);

} /* end of EnableProfSignals */

/* DisableProfSignals:
 */
void DisableProfSignals ()
{
    SIG_SetHandler (SIGVTALRM, SIG_DFL);

} /* end of DisableProfSignals */

/* ProfSigHandler:
 *
 * The handler for SIGVTALRM signals.
 */
PVT SigReturn_t ProfSigHandler ()
{
    Word_t	*arr = PTR_MLtoC(Word_t, ProfCntArray);
    int		indx = INT_MLtoC(DEREF(ProfCurrent));

    arr[indx]++;

} /* end of ProfSigHandler */


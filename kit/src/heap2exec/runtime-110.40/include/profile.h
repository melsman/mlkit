/* profile.h
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#ifndef _PROFILE_
#define _PROFILE_

#ifndef PROFILE_QUANTUM_US
#  define PROFILE_QUANTUM_US	10000		/* profile timer quantum in uS */
#endif

extern ml_val_t	ProfCntArray;

/* Indices into the ProfCntArray for the run-time and GC; these need to
 * track the definitions in sml-nj/boot/NJ/prof-control.sml.
 */
#define PROF_RUNTIME	INT_CtoML(0)
#define PROF_MINOR_GC	INT_CtoML(1)
#define PROF_MAJOR_GC	INT_CtoML(2)
#define PROF_OTHER	INT_CtoML(3)

#endif /* _PROFILE_ */


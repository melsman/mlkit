/* ml-fp.h
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * NOTE: changes to this file must be tracked in ml-fp.c.
 */


#ifdef TARGET_X86

extern void Save_C_FPState();
extern void Restore_C_FPState();

extern void Save_ML_FPState();
extern void Restore_ML_FPState();

#else

#define Save_C_FPState()
#define Restore_C_FPState()
#define Save_ML_FPState()
#define Restore_ML_FPState()

#endif

/* end of ml-fp.h */


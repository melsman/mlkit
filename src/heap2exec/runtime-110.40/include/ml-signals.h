/* ml-signals.h
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#ifndef _ML_SIGNALS_
#define _ML_SIGNALS_

typedef struct {		/* an item in the pending signal queue */
    int		sigNum;		    /* the ID number of the pending signal */
    int		count;		    /* the count of how many of this kind */
				    /* of signal are pending. */
} pending_sig_t;

/* The state of ML signal handlers; these definitions must agree with
 * the values used in src/sml-nj/boot/smlnj/signals.sml.
 */
#define ML_SIG_IGNORE		0
#define ML_SIG_DEFAULT		1
#define ML_SIG_ENABLED		2

/** Utility functions **/
extern void ChooseSignal (vproc_state_t *vsp);
extern void EnqueueSignal (vproc_state_t *vsp, int sigCode);
extern ml_val_t MakeResumeCont (ml_state_t *msp, ml_val_t resume[]);
extern ml_val_t MakeHandlerArg (ml_state_t *msp, ml_val_t resume[]);
extern void LoadResumeState (ml_state_t *msp);
extern bool_t GCSignal (vproc_state_t *vsp);

/* OS dependent implementations of signal operations. */
extern ml_val_t ListSignals (ml_state_t *msp);
extern void PauseUntilSignal (vproc_state_t *vsp);
extern void SetSignalState (vproc_state_t *vsp, int sigNum, int sigState);
extern int GetSignalState (vproc_state_t *vsp, int sigNum);
extern void SetSignalMask (ml_val_t sigList);
extern ml_val_t GetSignalMask (ml_state_t *msp);

#endif /* !_ML_SIGNALS_ */

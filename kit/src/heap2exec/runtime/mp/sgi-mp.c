/* sgi-mp.c
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * MP support for SGI Challenge machines (Irix 5.x).
 */

#include <sys/types.h>
#include <sys/prctl.h>
#include <unistd.h>
#include <ulocks.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "tags.h"
#include "ml-mp.h"
#include "ml-state.h"
#include "ml-globals.h"
#include "vproc-state.h"

/* #define ARENA_FNAME  tmpnam(0) */
#define ARENA_FNAME  "/tmp/sml-mp.lock-arena"

#define INT_MLinc(n,i)  ((ml_val_t)INT_CtoML(INT_MLtoC(n) + (i)))
#define INT_MLdec(n,i)  (INT_MLinc(n,(-i)))

/* forwards */
PVT mp_lock_t AllocLock ();        
PVT mp_barrier_t *AllocBarrier();

/* locals */
PVT usptr_t	*arena;		/* arena for shared sync objects */
PVT ulock_t	MP_ArenaLock;	/* must be held to alloc/free a lock */
PVT ulock_t	MP_ProcLock;	/* must be held to acquire/release procs */

/* globals */
mp_lock_t	MP_GCLock;
mp_lock_t	MP_GCGenLock;
mp_barrier_t	*MP_GCBarrier;
mp_lock_t	MP_TimerLock;


/* MP_Init:
 */
void MP_Init ()
{
  /* set '_utrace = 1;' to debug shared arenas */
    if (usconfig(CONF_LOCKTYPE, US_NODEBUG) == -1) {
	Die ("usconfig failed in MP_Init");
    }
    usconfig(CONF_AUTOGROW, 0);
    if (usconfig(CONF_INITSIZE, 65536) == -1) {
	Die ("usconfig failed in MP_Init");
    }
    if ((arena = usinit(ARENA_FNAME)) == NIL(usptr_t *)) {
	Die ("usinit failed in MP_Init");
    }

    MP_ArenaLock	= AllocLock();
    MP_ProcLock		= AllocLock();
    MP_GCLock		= AllocLock();
    MP_GCGenLock	= AllocLock();
    MP_TimerLock	= AllocLock();
    MP_GCBarrier	= AllocBarrier();
    ASSIGN(ActiveProcs, INT_CtoML(1));

} /* end of MP_Init */


/* MP_ProcId:
 */
mp_pid_t MP_ProcId ()
{

  return getpid ();

} /* end of MP_ProcId */


/* AllocLock:
 *
 * Allocate and initialize a system lock.
 */
PVT mp_lock_t AllocLock ()
{
    ulock_t	lock;

    if ((lock = usnewlock(arena)) == NIL(ulock_t)) {
	Die ("AllocLock: cannot get lock with usnewlock\n");
    }
    usinitlock(lock);
    usunsetlock(lock);

    return lock;

} /* end of AllocLock */
 

/* MP_SetLock:
 */
void MP_SetLock (mp_lock_t lock)
{
    ussetlock(lock);

} /* end of MP_SetLock */


/* MP_UnsetLock:
 */
void MP_UnsetLock (mp_lock_t lock)
{
    usunsetlock(lock);

} /* end of MP_UnsetLock */


/* MP_TryLock:
 */
bool_t MP_TryLock (mp_lock_t lock)
{
    return ((bool_t) uscsetlock(lock, 1));  /* try once */

} /* end of MP_TryLock */


/* MP_AllocLock:
 */
mp_lock_t MP_AllocLock ()
{
    ulock_t lock;

    ussetlock(MP_ArenaLock);
	lock = AllocLock ();
    usunsetlock(MP_ArenaLock);

    return lock;

} /* end of MP_AllocLock */


/*  MP_FreeLock:
 */
void MP_FreeLock (mp_lock_t lock)
{
    ussetlock(MP_ArenaLock);
	usfreelock(lock,arena);
    usunsetlock(MP_ArenaLock);

} /* end of MP_FreeLock */


/* AllocBarrier:
 *
 * Allocate and initialize a system barrier.
 */
PVT mp_barrier_t *AllocBarrier ()
{
    barrier_t *barrierp;

    if ((barrierp = new_barrier(arena)) == NIL(barrier_t *)) {
	Die ("cannot get barrier with new_barrier");
    }
    init_barrier(barrierp);

    return barrierp;

} /* end of AllocBarrier */
  
/* MP_AllocBarrier:
 */
mp_barrier_t *MP_AllocBarrier ()
{
    barrier_t *barrierp;

    ussetlock(MP_ArenaLock);
	barrierp = AllocBarrier ();
    usunsetlock(MP_ArenaLock);

    return barrierp;

} /* end of MP_AllocBarrier */

/* MP_FreeBarrier:
 */
void MP_FreeBarrier (mp_barrier_t *barrierp)
{
    ussetlock(MP_ArenaLock);
	free_barrier(barrierp);
    usunsetlock(MP_ArenaLock);

} /* end of MP_FreeBarrier */

/* MP_Barrier:
 */
void MP_Barrier (mp_barrier_t *barrierp, unsigned n)
{
    barrier(barrierp, n);

} /* end of MP_Barrier */

/* MP_ResetBarrier:
 */
void MP_ResetBarrier (mp_barrier_t *barrierp)
{
    init_barrier(barrierp);

} /* end of MP_ResetBarrier */

/* ??? */
PVT void fixPnum (int n)
{
  /* dummy for now */
}
 

/* MP_MaxProcs:
 */
int MP_MaxProcs ()
{
    return MAX_NUM_PROCS;

} /* end of MP_MaxProcs */


/* ProcMain:
 */
PVT void ProcMain (void *vmsp)
{
    ml_state_t *msp = (ml_state_t *) vmsp;

  /* needs to be done  
    fixPnum(msp->pnum);
    setup_signals(msp, TRUE);
   */
  /* spin until we get our id (from return of call to NewProc) */
    while (msp->ml_vproc->vp_mpSelf == NIL(mp_pid_t)) {
#ifdef MP_DEBUG
	SayDebug("[waiting for self]\n");
#endif
	continue;
    }
#ifdef MP_DEBUG
    SayDebug ("[new proc main: releasing lock]\n");
#endif
    MP_UnsetLock (MP_ProcLock); /* implicitly handed to us by the parent */
    RunML (msp);                 /* should never return */
    Die ("proc returned after run_ml() in ProcMain().\n");

} /* end of ProcMain */


/* NewProc:
 */
PVT int NewProc (ml_state_t *state)
{
    int ret, error;

    ret = sproc(ProcMain, PR_SALL, (void *)state);
    if (ret == -1) {
	extern int errno;

	error = oserror();	/* this is potentially a problem since */
				/* each thread should have its own errno. */
				/* see sgi man pages for sproc */
	Error ("error=%d,errno=%d\n", error, errno);
	Error ("[warning NewProc: %s]\n",strerror(error));
    } 

    return ret;
}


/* MP_AcquireProc:
 */
ml_val_t MP_AcquireProc (ml_state_t *msp, ml_val_t arg)
{
    ml_state_t *p;
    vproc_state_t *vsp;
    ml_val_t v = REC_SEL(arg, 0);
    ml_val_t f = REC_SEL(arg, 1);
    int i;

#ifdef MP_DEBUG
    SayDebug("[acquiring proc]\n");
#endif
    MP_SetLock(MP_ProcLock);
  /* search for a suspended proc to reuse */
    for (i = 0;
	(i < NumVProcs) && (VProc[i]->vp_mpState != MP_PROC_SUSPENDED);
	i++
    )
	continue;
#ifdef MP_DEBUG
    SayDebug("[checking for suspended processor]\n");
#endif
    if (i == NumVProcs) {
	if (DEREF(ActiveProcs) == INT_CtoML(MAX_NUM_PROCS)) {
	    MP_UnsetLock(MP_ProcLock);
	    Error("[processors maxed]\n");
	    return ML_false;
	}
#ifdef MP_DEBUG
	SayDebug("[checking for NO_PROC]\n");
#endif
      /* search for a slot in which to put a new proc */
	for (i = 0;
	    (i < NumVProcs) && (VProc[i]->vp_mpState != MP_PROC_NO_PROC);
	    i++
	)
	    continue;
	if (i == NumVProcs) {
	    MP_UnsetLock(MP_ProcLock);
	    Error("[no processor to allocate]\n");
	    return ML_false;
	}
    }
#ifdef MP_DEBUG
    SayDebug("[using processor at index %d]\n", i);
#endif
  /* use processor at index i */
    vsp = VProc[i];
    p = vsp->vp_state;

    p->ml_exnCont	= PTR_CtoML(handle_v+1);
    p->ml_arg		= ML_unit;
    p->ml_cont		= PTR_CtoML(return_c);
    p->ml_closure	= f;
    p->ml_pc		= 
    p->ml_linkReg	= GET_CODE_ADDR(f);
    p->ml_varReg	= v;
  
    if (vsp->vp_mpState == MP_PROC_NO_PROC) {
      /* assume we get one */
	ASSIGN(ActiveProcs, INT_MLinc(DEREF(ActiveProcs), 1));
	if ((vsp->vp_mpSelf = NewProc(p)) != -1) {
#ifdef MP_DEBUG
	    SayDebug ("[got a processor]\n");
#endif
	    vsp->vp_mpState = MP_PROC_RUNNING;
	  /* NewProc will release MP_ProcLock */
	    return ML_true;
	}
	else {
	    ASSIGN(ActiveProcs, INT_MLdec(DEREF(ActiveProcs), 1));
	    MP_UnsetLock(MP_ProcLock);
	    return ML_false;
	}      
    }
    else {
	vsp->vp_mpState = MP_PROC_RUNNING;
#ifdef MP_DEBUG
	SayDebug ("[reusing a processor]\n");
#endif
	MP_UnsetLock(MP_ProcLock);
	return ML_true;
    }

} /* end of MP_AcquireProc */

/* MP_ReleaseProc:
 */
void MP_ReleaseProc (ml_state_t *msp)
{
#ifdef MP_DEBUG
    SayDebug("[release_proc: suspending]\n");
#endif
    InvokeGC(msp,1);
    MP_SetLock(MP_ProcLock);
    msp->ml_vproc->vp_mpState = MP_PROC_SUSPENDED;
    MP_UnsetLock(MP_ProcLock);
    while (msp->ml_vproc->vp_mpState == MP_PROC_SUSPENDED) {
      /* need to be continually available for gc */
	InvokeGC(msp,1);
    }
#ifdef MP_DEBUG
    SayDebug("[release_proc: resuming]\n");
#endif
    RunML(msp);
    Die ("return after RunML(msp) in mp_release_proc\n");

} /* end of MP_ReleaseProc */


/* MP_ActiveProcs:
 */
int MP_ActiveProcs ()
{
    int ap;

    MP_SetLock(MP_ProcLock);
    ap = INT_MLtoC(DEREF(ActiveProcs));
    MP_UnsetLock(MP_ProcLock);

    return ap;

} /* end of MP_ActiveProcs */


/* MP_Shutdown:
 */
void MP_Shutdown ()
{
    usdetach(arena);

} /* end of MP_Shutdown */


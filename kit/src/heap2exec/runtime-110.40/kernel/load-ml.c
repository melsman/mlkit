/* load-ml.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 */

#include <stdio.h>
#include "ml-base.h"
#include "ml-state.h"
#include "gc.h"
#include "heap-io.h"


/* LoadML:
 *
 * Load a heap image from a file and resume execution.  The arguments allocSz,
 * numGens and cacheGen are possible command-line overrides of the heap parameters
 * specified in the image being imported (non-negative values signify override).
 */
void LoadML (const char *loadImage, heap_params_t *heapParams)
{
    ml_state_t		*msp;

    msp = ImportHeapImage (loadImage, heapParams);

#ifdef HEAP_MONITOR
    if (HeapMon_Init(msp->ml_heap) == FAILURE)
	Die("unable to start heap monitor");
#endif

    InitFaultHandlers ();

#ifdef SIZES_C64_ML32
  /* patch the 32-bit addresses */
    PatchAddrs ();
#endif

    RunML (msp);

} /* end of LoadML */

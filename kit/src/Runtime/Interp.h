
#ifndef INTERP_H
#define INTERP_H

#include "LoadKAM.h"

/* Interpret code; assumes that code is already resolved; i.e., that
 * instruction numbers are turned into instruction addresses. */
ssize_t 
interpCode(Interp* interpreter,          // Interpreter
	   register uintptr_t * sp,  // Stack pointer
	   uintptr_t * ds,           // Data segment pointer
	   uintptr_t * exnPtr,       // Pointer to next exn-handler on stack
	   Ro** topRegionCell,           // Cell for holding a pointer to the top-most region
	   char** errorStr,              // Cell to store error-string in case of an uncaught exception
	   uintptr_t *exnCnt,        // Exception name counter
	   bytecode_t b_prog,            // The actual code
	   void *serverCtx);             // Apache request_rec pointer


/* Resolve code; i.e., turn instruction numbers into instruction
 * addresses. */
void 
resolveCode(bytecode_t b_prog,              // Code to resolve
	    size_t sizeW);                     // Size of code in words

#ifndef APACHE
ssize_t 
main_interp(int argc, char * argv[]);
#endif

#endif /* INTERP_H */

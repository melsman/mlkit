
/* Interpret code; assumes that code is already resolved; i.e., that
 * instruction numbers are turned into instruction addresses. */
int 
interpCode(Interp* interpreter,          // Interpreter
	   register unsigned long * sp,  // Stack pointer
	   unsigned long * ds,           // Data segment pointer
	   unsigned long * exnPtr,       // Pointer to next exn-handler on stack
	   Ro** topRegionCell,           // Cell for holding a pointer to the top-most region
	   char** errorStr,              // Cell to store error-string in case of an uncaught exception
	   unsigned long *exnCnt,        // Exception name counter
	   bytecode_t b_prog);           // The actual code


/* Resolve code; i.e., turn instruction numbers into instruction
 * addresses. */
void 
resolveCode(bytecode_t b_prog,              // Code to resolve
	    int sizeW);                     // Size of code in words
	    

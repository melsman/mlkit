/* What about the flags:                                                       */
/*   PROFILING: If the flag is set, the runtime system automatically collects  */
/*              statistics on the regions. It only works when all region       */
/*              manipulations are done by the runtime system.                  */
/*              A datafile is created which can be used to output the memory   */
/*              usage graphically (see program rp).                            */
/* There are two important constants:                                          */
/*   ALLOCATABLE_WORDS_IN_REGION_PAGE: This constant holds the number of words */
/*                     in each region page.                                    */
/*   BYTES_ALLOC_BY_SBRK: This constant holds the number of bytes that are     */
/*                        allocated by sbrk. It has to be equivalent to a      */
/*                        number of region pages.                              */
/*                                                                             */
/*   TAG_VALUES: Tag values to admit for polymorphic equality.                 */

#ifndef FLAGS
#define FLAGS

#ifndef NULL
#define NULL 0
#endif

/* The flag PROFILING has to be set if using the new profiling tool. */
/* Flags when debugging the profiler.                                */
#define VERBOSE_OUTPUT_PROFILE 0

/* Tagging values. If elimination of polymorphic equality is disabled
   tagging must be enabled here. */

/* Use boxed or unboxed representation of lists. If elimination of
   polymorphic equality is disabled boxed representation must be
   used. */

#define REGION_PAGE_BAG_SIZE 30
#define HEAP_TO_LIVE_RATIO 3.0

// Simple memory profiling - remember to enable the flag
// simple_memprof_p in X86/CodeGenX86.sml if you enable this flag.
// #define SIMPLE_MEMPROF 1

#ifdef DEBUG
#define debug(Arg) Arg
#else
#define debug(Arg) {}
#endif

#endif /*FLAGS*/


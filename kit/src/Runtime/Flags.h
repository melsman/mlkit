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
/*   TAG_INTEGERS: Tag integers to admit for polymorphic equality.             */
/*   UNBOX_LISTS: Represent lists unboxed.                                     */

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
#define TAG_VALUES 0
#define TAG_INTEGERS 0

/* Use boxed or unboxed representation of lists. If elimination of
   polymorphic equality is disabled boxed representation must be
   used. */
#define UNBOX_LISTS 1

#endif /*FLAGS*/

/* What about the flags:                                                       */
/*   CHECKREGION: The flag is used by alloc, and if set alloc will make sure   */
/*                that there are room in the current region page for the words */
/*                that have to be allocated. We have always the flag set.      */
/*   PROFILING: If the flag is set, the runtime system automatically collects  */
/*              statistics on the regions. It only works when all region       */
/*              manipulations are done by the runtime system.                  */
/*              A datafile is created which can be used to output the memory   */
/*              usage graphically (see program rp).                            */
/*   FREESTAT: This flag only works when PROFILING is set, and when set the    */
/*             frelist is printed each time the collected statistics are       */
/*             printed.                                                        */
/*   DEBUG_STRING: Used when debugging skrings.                                */
/*   DEBUG_ALL_CALL: Writes whenever a procedure is entered and leaved.        */
/*                   Except region procedures, equalpoly and a few others.     */
/*   DEBUG_EQUAL_POLY:  Used when debugging polymorph equality.                */
/*   DEBUG_ALLOCATE_REGION:  Used when debugging regions.                      */
/*   DEBUG_DEALLOCATE_REGION: Used when debugging regions.                     */
/*   DEBUG_ALLOC:  Used when debugging regions.                                */
/*   DEBUG_RESET_REGION:  Used when debugging regions.                         */ 
/*   DEBUG_DEALLOCATE_REGIONS_UNTIL: Used when debugging regions.              */
/*   DEBUG_EXN: Used when debugging operations that can raise exceptions.      */
/*   PRINT_REGION_STACK_WHEN_DEBUG: When debugging regions the region          */
/*                                  stack will be printed if this flag is on.  */
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

#define CHECKREGION 1
/*#define PROFILING 1*/
#define FREESTAT 0       /* PROFILING has to be 1, for this to be set. */
#define DEBUG_STRING 0
#define DEBUG_ALL_CALL 0
#define DEBUG_EQUAL_POLY 0
#define DEBUG_ALLOCATE_REGION 0
#define DEBUG_DEALLOCATE_REGION 0
#define DEBUG_ALLOC 0
#define DEBUG_RESET_REGION 0
#define DEBUG_DEALLOCATE_REGIONS_UNTIL 0
#define DEBUG_EXN 0
#define DEBUG_SBRK 0
#define DEBUG_PRINT_STRING 0
#define PRINT_REGION_STACK_WHEN_DEBUG 0


/* The flag PROFILING has to be set if using the new profiling tool. */
/* Flags when debugging the profiler.                                */
#define DEBUG_ALARM_HANDLER_PROFILING 0
#define DEBUG_ALLOC_REGION_FINITE_PROFILING 0
#define DEBUG_DEALLOC_REGION_FINITE_PROFILING 0
#define DEBUG_DEALLOCATE_REGIONS_UNTIL_PROFILING 0
#define DEBUG_STORE_PRG_POINT 0
#define DEBUG_UPDATE_SIZE_FOR_DOUBLES 0
#define VERBOSE_OUTPUT_PROFILE 0
#define PRINT_WARNINGS_PROFILE_TICK 0
#define DEBUG_PROFILE_TICK 0


/* Tagging values. If elimination of polymorphic equality is disabled
   tagging must be enabled here. */
#define TAG_VALUES 0
#define TAG_INTEGERS 0

/* Use boxed or unboxed representation of lists. If elimination of
   polymorphic equality is disabled boxed representation must be
   used. */
#define UNBOX_LISTS 1

#endif /*FLAGS*/

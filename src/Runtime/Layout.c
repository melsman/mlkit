/* Compile-time checks for layouts shared with BackendInfo and code generation.
 * These checks cover the C data model, not the future ARM GC/frame ABI.
 */
#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>
#include "Region.h"
#include "String.h"
#include "Exception.h"
#ifdef PARALLEL
#include "Spawn.h"
#endif

#define CHECK(name, condition) typedef char layout_##name[(condition) ? 1 : -1]
#define WORDS(type, n) CHECK(type##_size, sizeof(type) == (n) * 8)
CHECK(byte_size, __CHAR_BIT__ == 8);
CHECK(pointer_size, sizeof(void *) == 8);
WORDS(long, 1);
WORDS(uintptr_t, 1);
WORDS(size_t, 1);
WORDS(ssize_t, 1);
WORDS(double, 1);
CHECK(int_size, sizeof(int) == 4);
WORDS(Gen, 2);
CHECK(gen_a, offsetof(Gen, a) == 0);
CHECK(gen_fp, offsetof(Gen, fp) == 8);
CHECK(page_size, sizeof(Rp) == REGION_PAGE_SIZE_BYTES);
CHECK(ro_g0, offsetof(Ro, g0) == 0);
#ifdef ENABLE_GEN_GC
# define GENERATIONS 2
CHECK(ro_g1, offsetof(Ro, g1) == 16);
#else
# define GENERATIONS 1
#endif
#ifdef PROFILING
# define PROFILE_WORDS 3
CHECK(ro_alloc, offsetof(Ro, allocNow) == (2 * GENERATIONS + 1) * 8);
CHECK(ro_alloc_prof, offsetof(Ro, allocProfNow) == (2 * GENERATIONS + 2) * 8);
CHECK(ro_id, offsetof(Ro, regionId) == (2 * GENERATIONS + 3) * 8);
WORDS(FiniteRegionDesc, 2);
WORDS(ObjectDesc, 2);
#else
# define PROFILE_WORDS 0
#endif
CHECK(ro_previous, offsetof(Ro, p) == 2 * GENERATIONS * 8);
CHECK(ro_lobjs, offsetof(Ro, lobjs) == (2 * GENERATIONS + 1 + PROFILE_WORDS) * 8);
#ifdef PARALLEL
# define PAR_WORDS 1
CHECK(ro_mutex, offsetof(Ro, mutex) == (2 * GENERATIONS + 2 + PROFILE_WORDS) * 8);
#else
# define PAR_WORDS 0
#endif
CHECK(ro_size, sizeof(Ro) == (2 * GENERATIONS + 2 + PROFILE_WORDS + PAR_WORDS) * 8);
CHECK(context_top, offsetof(context, topregion) == 0);
CHECK(context_exception, offsetof(context, exnptr) == 8);
CHECK(context_uncaught, offsetof(context, uncaught_exnname) == 16);
WORDS(Exception, 2);
CHECK(exception_string, offsetof(Exception, string) == 8);

CHECK(string_data, offsetof(StringDesc, data) == 8);
CHECK(string_alignment, __alignof__(StringDesc) == 8);
CHECK(word_alignment, __alignof__(uintptr_t) == 8);
CHECK(double_alignment, __alignof__(double) == 8);

#ifdef PARALLEL
CHECK(thread_arg, offsetof(ThreadInfo, arg) == 0);
CHECK(thread_context, offsetof(ThreadInfo, ctx) == 8);
#endif

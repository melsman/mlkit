#include <assert.h>
#include <stdint.h>
extern uintptr_t mlkit_regions_may_spawn;
void mlkit_spawn_unit_keep(void) {}
void mlkit_assert_may_spawn(void) { assert(mlkit_regions_may_spawn == 1); }

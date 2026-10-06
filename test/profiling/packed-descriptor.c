#include <assert.h>
#include <sys/types.h>
#include "Region.h"

int main(void)
{
  _Static_assert(sizeof(ObjectDesc) == sizeof(uintptr_t), "one-word descriptor");
  _Static_assert(OBJECT_DESC_SIZE_BITS == 16, "16 size bits");
  ObjectDesc d;
  const size_t points[] = {1, 2, 65536, OBJECT_DESC_MAX_POINT};
  const size_t sizes[] = {0, 1, ALLOCATABLE_WORDS_IN_REGION_PAGE - 1, 65534};
  for (size_t i = 0; i < sizeof(points)/sizeof(points[0]); i++)
    for (size_t j = 0; j < sizeof(sizes)/sizeof(sizes[0]); j++) {
      objectDescInit(&d, sizes[j], points[i]);
      assert(objectDescPoint(&d) == points[i]);
      assert(objectDescSize(&d) == sizes[j]);
      assert(d.packed == ((uintptr_t)points[i] << 16 | sizes[j]));
    }

  /* The escape must retain full sizes, including values beyond 16 bits. */
  Lobjs large;
  const size_t largeSizes[] = {65535, 65536, 1000000};
  for (size_t i = 0; i < sizeof(largeSizes)/sizeof(largeSizes[0]); i++) {
    large.profSize = largeSizes[i];
    objectDescInit((ObjectDesc *)&large.value, large.profSize, OBJECT_DESC_MAX_POINT);
    assert(objectDescSize((ObjectDesc *)&large.value) == large.profSize);
    assert(objectDescPoint((ObjectDesc *)&large.value) == OBJECT_DESC_MAX_POINT);
  }

  /* Page traversal includes zero-word objects and a zero end sentinel. */
  uintptr_t page[16] = {0};
  ObjectDesc *a = (ObjectDesc *)page;
  objectDescInit(a, 0, 2);
  ObjectDesc *b = a + 1;
  objectDescInit(b, 3, 70000);
  ObjectDesc *end = (ObjectDesc *)((uintptr_t *)b + 1 + objectDescSize(b));
  assert(objectDescPoint(a) == 2);
  assert(objectDescPoint(b) == 70000);
  assert(end->packed == notPP);
  return 0;
}

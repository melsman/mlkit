#include <stdint.h>
#include <stdlib.h>

static uintptr_t encode(uintptr_t x)
{
#ifdef TAG_VALUES
  return (x << 1) | 1;
#else
  return x;
#endif
}

/* Raw ML calls deliberately bypass auto-conversion to check the encoding. */
uintptr_t mlkit_check_word32(uintptr_t x)
{
  if (x != encode(UINT32_MAX)) abort();
  return encode(1);
}
uintptr_t mlkit_check_int32(uintptr_t x)
{
  if (x != encode((uintptr_t)(uint32_t)INT32_MIN)) abort();
  return encode(1);
}
uint32_t mlkit_word32_identity(uint32_t x) { return x; }
int32_t mlkit_int32_identity(int32_t x) { return x; }

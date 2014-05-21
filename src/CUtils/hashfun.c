#include "hashfun.h"

// char * -> unsigned long 
unsigned long
charhashfunction (char *key)
{
  unsigned long result = 0;
  unsigned char *k = (unsigned char *) key;
  while (*k)
    {
      // Will not overflow as the type is unsigned
      result = 31 * result + *k++;
    }
  return result;
}

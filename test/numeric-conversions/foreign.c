#include <stdint.h>
#include <stdio.h>
uintptr_t wordcode_to_word(uintptr_t value)
{
    puts("foreign conversion called");
    fflush(stdout);
    return value;
}

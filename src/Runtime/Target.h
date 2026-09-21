/* Included by every runtime translation unit, including CUtils. */
#ifndef MLKIT_TARGET_H
#define MLKIT_TARGET_H
#include "config.h"
#if defined(__APPLE__)
# if DARWIN_NATIVE
#  if !defined(__aarch64__) && !defined(__arm64__)
#   error "DARWIN_NATIVE=1 requires an arm64 C compiler target"
#  endif
# elif !defined(__x86_64__)
#  error "DARWIN_NATIVE=0 requires an x86_64 C compiler target"
# endif
#elif DARWIN_NATIVE
# error "DARWIN_NATIVE=1 requires macOS"
#endif
#endif

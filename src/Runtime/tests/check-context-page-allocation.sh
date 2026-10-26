#!/bin/sh
# Run after building the native parallel runtime. No performance measurement.
set -eu
cd "$(dirname "$0")/../../.."
work=$(mktemp -d /tmp/mlkit-context-page-check.XXXXXX)
case $(uname -s)-$(uname -m) in
  Darwin-arm64) target=darwin-arm64; arch='-arch arm64'; dead_strip='-Wl,-dead_strip';;
  Linux-x86_64) target=linux-x86_64; arch=; dead_strip='-Wl,--gc-sections';;
  *) echo 'Unsupported native target' >&2; exit 1;;
esac
${CC:-cc} $arch -O2 -ffunction-sections -fdata-sections -std=gnu11 -DPARALLEL -iquote src/Runtime \
  -I "src/Runtime/build/$target" -include src/Runtime/Target.h \
  -Dthread_info=context_page_thread_info -c src/Runtime/Region.c \
  -o "$work/Region.o"
${CC:-cc} $arch -O2 -ffunction-sections -fdata-sections -std=gnu11 -DPARALLEL -iquote src/Runtime \
  src/Runtime/tests/context-page-allocation.c "$work/Region.o" \
  "lib/$target/runtimeSystemPar.a" $dead_strip -pthread -lm -o "$work/check"
"$work/check"

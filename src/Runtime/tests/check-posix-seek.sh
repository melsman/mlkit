#!/bin/sh
# Boundary checks through the real wrapper, independent of filesystem limits.
set -eu
cd "$(dirname "$0")/../../.."
case $(uname -s)-$(uname -m) in
  Darwin-arm64) target=darwin-arm64; arch='-arch arm64'; dead_strip='-Wl,-dead_strip';;
  Darwin-x86_64) target=darwin-x86_64; arch='-arch x86_64'; dead_strip='-Wl,-dead_strip';;
  Linux-x86_64) target=linux-x86_64; arch=; dead_strip='-Wl,--gc-sections';;
  *) echo 'POSIX seek checks require a supported 64-bit native target' >&2; exit 1;;
esac
work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-posix-seek.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
for mode in plain tagged; do
  flags=
  if [ "$mode" = tagged ]; then flags=-DTAG_VALUES; fi
  # Dead stripping avoids pulling in unrelated runtime functions from Posix.c.
  ${CC:-cc} $arch -O2 -std=gnu11 -ffunction-sections -fdata-sections \
    $flags -Dlseek=mlkit_test_lseek -iquote src/Runtime \
    -I"src/Runtime/build/$target" -include src/Runtime/Target.h \
    -c src/Runtime/Posix.c -o "$work/posix.o"
  ${CC:-cc} $arch -O2 -std=gnu11 -Wall -Wextra -Werror $flags \
    -iquote src/Runtime -include src/Runtime/Target.h \
    src/Runtime/tests/posix-seek.c "$work/posix.o" $dead_strip -o "$work/check"
  "$work/check"
done

#!/bin/sh
# Exhaustive unary/conversion checks and sampled binary arithmetic, no MPFR needed.
set -eu
cd "$(dirname "$0")/../../.."
case $(uname -s)-$(uname -m) in
  Darwin-arm64) arch='-arch arm64';;
  Darwin-x86_64) arch='-arch x86_64';;
  Linux-x86_64) arch=;;
  *) echo 'Binary16 checks require a 64-bit native target' >&2; exit 1;;
esac
work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-real16-check.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
for mode in plain tagged; do
  flags=
  if [ "$mode" = tagged ]; then flags=-DTAG_VALUES; fi
  ${CC:-cc} $arch -O2 -std=gnu11 -frounding-math -Wall -Wextra -Werror \
    $flags -iquote src/Runtime -include src/Runtime/Target.h \
    src/Runtime/Real16.c src/Runtime/tests/real16.c -lm -o "$work/check"
  "$work/check"
done

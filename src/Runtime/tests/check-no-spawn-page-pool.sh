#!/bin/sh
# Check the installed no-spawn archive through its page-management behavior.
set -eu
cd "$(dirname "$0")/../../.."
case $(uname -s)-$(uname -m) in
  Darwin-arm64) target=darwin-arm64; arch='-arch arm64'; dead_strip='-Wl,-dead_strip';;
  Linux-x86_64) target=linux-x86_64; arch=; dead_strip='-Wl,--gc-sections';;
  *) echo 'Unsupported native target' >&2; exit 1;;
esac
work=$(mktemp -d /tmp/mlkit-no-spawn-pool-check.XXXXXX)
trap 'rm -rf "$work"' EXIT HUP INT TERM
${CC:-cc} $arch -O2 -std=gnu11 -DPARALLEL -iquote src/Runtime \
  src/Runtime/tests/no-spawn-page-pool.c "lib/$target/runtimeSystemParNoSpawn.a" \
  $dead_strip -pthread -lm -o "$work/check"
"$work/check"

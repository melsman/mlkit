#!/bin/sh
# Additional native archive/object correctness checks; no timing collection.
set -eu
cd "$(dirname "$0")/../../.."
case $(uname -s)-$(uname -m) in
  Darwin-arm64) target=darwin-arm64; arch='-arch arm64'; dead_strip='-Wl,-dead_strip';;
  Linux-x86_64) target=linux-x86_64; arch=; dead_strip='-Wl,--gc-sections';;
  *) echo 'Unsupported native target' >&2; exit 1;;
esac
work=$(mktemp -d /tmp/mlkit-no-spawn-pool-check.XXXXXX)
base=${BASE_RUNTIME:-lib/$target/runtimeSystemPar.a}
${CC:-cc} $arch -O2 -g -std=gnu99 -fPIC -DPARALLEL \
  -DMLKIT_NO_SPAWN_PAGE_POOL -iquote src/Runtime \
  -I "src/Runtime/build/$target" -include src/Runtime/Target.h \
  -c src/Runtime/Region.c -o "$work/Region.o"
cp "$base" "$work/runtime.a"
ar r "$work/runtime.a" "$work/Region.o"
ranlib "$work/runtime.a"
nm -u "$work/Region.o" > "$work/undefined-symbols.txt"
if grep -E ' U _?(thread_info|mutex_lock|mutex_unlock)$' "$work/undefined-symbols.txt"; then
  echo 'Unexpected context lookup or freelist synchronization reference.' >&2; exit 1
fi
${CC:-cc} $arch -O2 -std=gnu11 -DPARALLEL -iquote src/Runtime \
  src/Runtime/tests/no-spawn-page-pool.c "$work/runtime.a" \
  $dead_strip -pthread -lm -o "$work/check"
"$work/check"
if ${CC:-cc} $arch -std=gnu99 -DPARALLEL -DPROFILING \
  -DMLKIT_NO_SPAWN_PAGE_POOL -iquote src/Runtime \
  -I "src/Runtime/build/$target" -include src/Runtime/Target.h \
  -c src/Runtime/Region.c -o "$work/unsupported.o" > "$work/unsupported.log" 2>&1; then
  echo 'Unsupported profiling variant should have been rejected.' >&2; exit 1
fi
grep -q 'no-spawn page pool requires plain POSIX parallel runtime' "$work/unsupported.log"
echo "Archive and validation files: $work"

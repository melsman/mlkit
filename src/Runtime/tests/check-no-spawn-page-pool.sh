#!/bin/sh
# Manual ARM64 prototype check; deliberately not a general runtime selector.
set -eu
cd "$(dirname "$0")/../../.."
[ "$(uname -s)-$(uname -m)" = Darwin-arm64 ] || {
  echo 'This experiment currently targets Darwin ARM64 only.' >&2; exit 1;
}
work=$(mktemp -d /tmp/mlkit-no-spawn-pool-check.XXXXXX)
base=${BASE_RUNTIME:-lib/darwin-arm64/runtimeSystemPar.a}
${CC:-cc} -arch arm64 -O2 -g -std=gnu99 -fPIC -DPARALLEL \
  -DMLKIT_NO_SPAWN_PAGE_POOL -iquote src/Runtime \
  -I src/Runtime/build/darwin-arm64 -include src/Runtime/Target.h \
  -c src/Runtime/Region.c -o "$work/Region.o"
cp "$base" "$work/runtime.a"
ar r "$work/runtime.a" "$work/Region.o"
ranlib "$work/runtime.a"
nm -u "$work/Region.o" > "$work/undefined-symbols.txt"
if grep -E ' U _(thread_info|mutex_lock|mutex_unlock)$' "$work/undefined-symbols.txt"; then
  echo 'Unexpected context lookup or freelist synchronization reference.' >&2; exit 1
fi
${CC:-cc} -arch arm64 -O2 -std=gnu11 -DPARALLEL -iquote src/Runtime \
  src/Runtime/tests/no-spawn-page-pool.c "$work/runtime.a" \
  -Wl,-dead_strip -pthread -lm -o "$work/check"
"$work/check"
if ${CC:-cc} -arch arm64 -std=gnu99 -DPARALLEL -DPROFILING \
  -DMLKIT_NO_SPAWN_PAGE_POOL -iquote src/Runtime \
  -I src/Runtime/build/darwin-arm64 -include src/Runtime/Target.h \
  -c src/Runtime/Region.c -o "$work/unsupported.o" > "$work/unsupported.log" 2>&1; then
  echo 'Unsupported profiling variant should have been rejected.' >&2; exit 1
fi
grep -q 'no-spawn page-pool experiment requires plain POSIX parallel runtime' "$work/unsupported.log"
echo "Experimental archive and validation files: $work"

#!/bin/sh
# Compare the fast helper with the same generated image's original C-only path.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${ARM64_EMITTER:?Set ARM64_EMITTER}"
fixtures=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-allocation-benchmark.XXXXXX")
echo "Allocation benchmark outputs: $scratch"
cd "$scratch"
"$ARM64_EMITTER" > emitter.log 2>&1
# Remove only the assembly fast prefixes, leaving the old preserving C stubs
# at the same private ABI entry points. This controls for all other code.
awk '
  /^_mlkit_arm64_allocate(_untagged)?_preserving:$/ {skip=1; print; next}
  skip && /^_mlkit_arm64_allocate(_untagged)?_preserving_slow:$/ {skip=0}
  !skip {print}
' allocation-concurrent-link.s > baseline-link.s
for variant in baseline fast; do
  link=allocation-concurrent-link.s
  [ "$variant" != baseline ] || link=baseline-link.s
  gcc -arch arm64 -O2 -Wall -Wextra -Werror -DPARALLEL -DALLOCS=131072 \
    -iquote "$SML_LIB/src/Runtime" "$fixtures/allocation-concurrent.c" \
    "$fixtures/allocation-bridge.s" "$link" \
    "$SML_LIB/lib/darwin-arm64/runtimeSystemPar.a" -lm -pthread -o "$variant"
done
printf 'scenario,run,variant,seconds\n' > samples.csv
for scenario in single shared mixed expansion private; do
  for variant in baseline fast; do
    ALLOCATION_SCENARIO=$scenario "./$variant" > actual
    grep -qx 'generated/C allocation passed' actual
  done
  for run in 1 2 3 4 5 6 7 8 9; do
    order='baseline fast'
    [ $((run % 2)) -ne 0 ] || order='fast baseline'
    for variant in $order; do
      ALLOCATION_SCENARIO=$scenario ALLOCATION_TIMING=1 "./$variant" > actual 2> seconds
      grep -qx 'generated/C allocation passed' actual
      printf '%s,%s,%s,%s\n' "$scenario" "$run" "$variant" "$(cat seconds)" >> samples.csv
    done
  done
done
cat samples.csv

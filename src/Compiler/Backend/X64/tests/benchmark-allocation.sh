#!/bin/sh
# Manual only: compare shared allocator helpers, using identical C/runtime code.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT:?Set MLKIT}"
: "${MLKIT_BASELINE:?Set MLKIT_BASELINE}"
fixtures=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-x64-allocation-benchmark.XXXXXX")
echo "X64 allocator benchmark outputs: $scratch"
cd "$scratch"
printf 'val _:unit = prim("putchar",0w65)\n' > helper.sml
prefix=
case $(uname -s) in Darwin) prefix=_;; esac
for variant in baseline fast; do
  compiler=$MLKIT
  [ "$variant" != baseline ] || compiler=$MLKIT_BASELINE
  "$compiler" -no_gc -par --no_basislib --no_delete_target_files \
    --mlb-subdir "Helper$variant" --assembler "${CC:-cc} -c" -ldexe "${CC:-cc}" \
    -o "$variant-helper" helper.sml > "$variant-compile.log" 2>&1
  "./$variant-helper" > actual
  printf A > expected
  cmp expected actual
  awk -v p="$prefix" '
    $0 == ".globl " p "__allocate" {emit=1; print ".text"}
    $0 == ".globl " p "__raise_overflow" {exit}
    emit {print}
  ' "MLB/RI_PAR_Helper$variant/base-link_objects.s" > "$variant-link.s"
  grep -q 'lock cmpxchgq' "$variant-link.s"
  ${CC:-cc} -O2 -Wall -Wextra -Werror -DPARALLEL -DALLOCS=131072 \
    -Darm64_generated_alloc=x64_generated_alloc -iquote "$SML_LIB/src/Runtime" \
    "$fixtures/../../Arm64/tests/allocation-concurrent.c" \
    "$fixtures/allocation-bridge.S" "$variant-link.s" \
    "${ALLOCATION_RUNTIME:-$SML_LIB/lib/runtimeSystemPar.a}" -lm -pthread -o "$variant"
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

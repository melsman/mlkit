#!/bin/sh
# Manual bridge measurements for the pointer-saving assembly experiment.
set -eu
: "${M8_ROOT:?Set M8_ROOT to the milestone-8 source/benchmark directory}"
: "${POINTER_BENCHMARK:?Set POINTER_BENCHMARK to the pointer experiment directory}"
root=$M8_ROOT
forced=$(sed -n '1s/.*: //p' "$root/forced.log")
cd "$POINTER_BENCHMARK"
fixtures=$root/fast/src/Compiler/Backend
for variant in stack previous pointer; do
  case $variant in
    stack) link=$forced/MLB/RI_PAR_Localbaseline/base-link_objects.s;;
    previous) link=$forced/MLB/RI_PAR_Localfast/base-link_objects.s;;
    pointer) link=pointer-link.s;;
  esac
  awk '
    $0 == ".globl __allocate" {emit=1;print ".text"}
    $0 == ".globl __raise_overflow" {exit}
    emit {print}
  ' "$link" > "$variant-helpers.s"
  cc -O2 -Wall -Wextra -Werror -DPARALLEL -DALLOCS=131072 \
    -Darm64_generated_alloc=x64_generated_alloc -iquote "$root/fast/src/Runtime" \
    "$fixtures/Arm64/tests/allocation-concurrent.c" "$fixtures/X64/tests/allocation-bridge.S" \
    "$variant-helpers.s" "$root/fast/lib/runtimeSystemPar.a" -lm -pthread -o "$variant-bridge"
done
printf 'scenario,run,variant,seconds\n' > bridge-samples.csv
for scenario in single shared mixed expansion private; do
  for variant in stack previous pointer; do
    ALLOCATION_SCENARIO=$scenario "./$variant-bridge" > actual
    grep -qx 'generated/C allocation passed' actual
  done
  for run in 1 2 3 4 5 6 7 8 9; do
    order='stack previous pointer'
    [ $((run % 2)) -ne 0 ] || order='pointer previous stack'
    for variant in $order; do
      ALLOCATION_SCENARIO=$scenario ALLOCATION_TIMING=1 "./$variant-bridge" > actual 2> seconds
      grep -qx 'generated/C allocation passed' actual
      printf '%s,%s,%s,%s\n' "$scenario" "$run" "$variant" "$(cat seconds)" >> bridge-samples.csv
    done
  done
done
cat bridge-samples.csv

#!/bin/sh
# Manual experiment: eliminate the result-pointer dependency on CAS's rax output.
set -eu
: "${M8_ROOT:?Set M8_ROOT}"
: "${REVERIFY_BENCHMARK:?Set REVERIFY_BENCHMARK}"
root=$M8_ROOT
source=$REVERIFY_BENCHMARK/short/MLB/RI_PAR_Verifycurrent
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-cas-result.XXXXXX")
echo "CAS-result experiment outputs: $scratch"
cd "$scratch"
cp "$REVERIFY_BENCHMARK/short/current" current
cp "$REVERIFY_BENCHMARK/short/baseline" baseline
cp "$source/main.sml.o" main.o
# Variant 1: retain the existing size save/pre-CAS arithmetic; derive the result
# again from proposed pointer and saved size instead of consuming CAS's rax.
awk '
  /^allocinreg_protected:$/ {protected=1}
  protected && /^.text$/ {protected=0}
  protected && /movq %rax,%r11/ {
    print "\tmovq %xmm15,%r11"; print "\tsalq $3,%r11"
    print "\tnegq %r11"; print "\taddq %r10,%r11";next
  }
  {print}
' "$source/base-link_objects.s" > recompute.s
# Variant 2 control: save the old allocation pointer instead of the size.
awk '
  /^allocinreg:$/ {helper=1}
  /^.globl __raise_overflow$/ {helper=0}
  helper && /movq %r10,%xmm15/ {next}
  helper && /movq \(%r11\),%rax/ {print;print "\tmovq %rax,%xmm15";next}
  helper && (/salq \$3,%rax/ || /negq %rax/ || /addq %r10,%rax/) {next}
  helper && /movq %xmm15,%r10/ {
    print "\tmovq %xmm15,%rax";print "\tsubq %rax,%r10";print "\tshrq $3,%r10";next
  }
  {print}
' "$source/base-link_objects.s" > pointer-control.s
# Change only the protected result move: xmm15 was set before CAS, whereas rax
# is an output of CAS even when the reservation succeeds.
awk '
  /^allocinreg_protected:$/ {protected=1}
  protected && /^.text$/ {protected=0}
  protected && /movq %rax,%r11/ {print "\tmovq %xmm15,%r11";next}
  {print}
' pointer-control.s > pointer-result.s
fixtures=$root/fast/src/Compiler/Backend
for variant in recompute pointer-control pointer-result; do
  cc -c "$variant.s" -o "$variant.o"
  cc -o "$variant" main.o "$variant.o" "$root/fast/lib/runtimeSystemPar.a" -lm -ldl -pthread
  "./$variant" > actual
  grep -qx ok actual
  awk '
    $0==".globl __allocate" {emit=1;print ".text"}
    $0==".globl __raise_overflow" {exit}
    emit {print}
  ' "$variant.s" > helpers.s
  for failure in normal forced; do
    input=helpers.s
    if [ "$failure" = forced ]; then
      awk '/lock cmpxchgq/ {print "\txorq %rax,%rax"} {print}' helpers.s > failure.s
      input=failure.s
    fi
    cc -O2 -Wall -Wextra -Werror -DPARALLEL -Darm64_generated_alloc=x64_generated_alloc \
      -iquote "$root/fast/src/Runtime" "$fixtures/Arm64/tests/allocation-concurrent.c" \
      "$fixtures/X64/tests/allocation-bridge.S" "$input" \
      "$root/fast/lib/runtimeSystemPar.a" -lm -pthread -o check
    for scenario in single shared mixed expansion private; do
      ALLOCATION_SCENARIO=$scenario taskset -c 0,2,4,6 ./check > actual
      grep -qx 'generated/C allocation passed' actual
    done
  done
  size "$variant" "$variant.o" > "$variant-size.txt"
  printf 'Correctness passed: %s\n' "$variant"
done
printf 'run,variant,real,user,sys\n' > samples.csv
for run in 1 2 3 4 5 6 7 8 9; do
  order='baseline current recompute pointer-control pointer-result'
  [ $((run % 2)) -ne 0 ] || order='pointer-result pointer-control recompute current baseline'
  for variant in $order; do
    /usr/bin/time -p "./$variant" > actual 2> timing
    grep -qx ok actual
    awk -v r="$run" -v v="$variant" '
      $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
      END {printf "%s,%s,%s,%s,%s\n",r,v,real,user,sys}
    ' timing >> samples.csv
  done
  printf 'Timing round %s passed.\n' "$run"
done

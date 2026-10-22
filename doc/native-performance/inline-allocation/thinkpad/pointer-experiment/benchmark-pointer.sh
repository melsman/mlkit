#!/bin/sh
# Manual assembly experiment; do not use these transformations in production.
set -eu
: "${M8_ROOT:?Set M8_ROOT to the milestone-8 source/benchmark directory}"
root=$M8_ROOT
forced=$(sed -n '1s/.*: //p' "$root/forced.log")
scratch=$(mktemp -d /tmp/mlkit-pointer-helper.XXXXXX)
echo "Pointer helper outputs: $scratch"
cd "$scratch"
awk '
  /^allocinreg:$/ {helper=1}
  /^.globl __raise_overflow$/ {helper=0}
  helper && /movq %r10,%xmm15/ {next}
  helper && /movq \(%r11\),%rax/ {print; print "\tmovq %rax,%xmm15"; next}
  helper && (/salq \$3,%rax/ || /negq %rax/ || /addq %r10,%rax/) {next}
  helper && /movq %xmm15,%r10/ {
    print "\tmovq %xmm15,%rax"; print "\tsubq %rax,%r10"; print "\tshrq $3,%r10"; next
  }
  {print}
' "$forced/MLB/RI_PAR_Localfast/base-link_objects.s" > pointer-link.s
cc -c pointer-link.s -o pointer-link.o
cc -o pointer "$forced/MLB/RI_PAR_Localfast/main.sml.o" pointer-link.o "$root/fast/lib/runtimeSystemPar.a" -lm -ldl -pthread
cp "$forced/baseline" stack
cp "$forced/fast" previous
fixtures=$root/fast/src/Compiler/Backend
awk '
  $0 == ".globl __allocate" {emit=1;print ".text"}
  $0 == ".globl __raise_overflow" {exit}
  emit {print}
' pointer-link.s > helpers.s
cc -O2 -Wall -Wextra -Werror -DPARALLEL -Darm64_generated_alloc=x64_generated_alloc \
  -iquote "$root/fast/src/Runtime" "$fixtures/Arm64/tests/allocation-concurrent.c" \
  "$fixtures/X64/tests/allocation-bridge.S" helpers.s "$root/fast/lib/runtimeSystemPar.a" -lm -pthread -o correctness
for scenario in single shared mixed expansion private; do
  ALLOCATION_SCENARIO=$scenario ./correctness > actual
  grep -qx 'generated/C allocation passed' actual
done
awk '/lock cmpxchgq/ {print "\txorq %rax,%rax"} {print}' helpers.s > failure.s
cc -O2 -Wall -Wextra -Werror -DPARALLEL -Darm64_generated_alloc=x64_generated_alloc \
  -iquote "$root/fast/src/Runtime" "$fixtures/Arm64/tests/allocation-concurrent.c" \
  "$fixtures/X64/tests/allocation-bridge.S" failure.s "$root/fast/lib/runtimeSystemPar.a" -lm -pthread -o failure
./failure > actual
grep -qx 'generated/C allocation passed' actual
printf 'run,variant,real,user,sys\n' > samples.csv
for run in 1 2 3 4 5 6 7 8 9; do
  order='stack previous pointer'
  [ $((run % 2)) -ne 0 ] || order='pointer previous stack'
  for variant in $order; do
    /usr/bin/time -p "./$variant" > actual 2> timing
    grep -qx ok actual
    awk -v r="$run" -v v="$variant" '
      $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
      END {printf "%s,%s,%s,%s,%s\n",r,v,real,user,sys}
    ' timing >> samples.csv
  done
done
cat samples.csv

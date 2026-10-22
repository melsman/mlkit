#!/bin/sh
# Exercise the emitted helper against the shared generated/C stress fixture.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT:?Set MLKIT}"
fixtures=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-x64-allocation.XXXXXX")
echo "X64 allocation outputs: $scratch"
cd "$scratch"
printf 'val _:unit = prim("putchar",0w65)\n' > helper.sml
"$MLKIT" -no_gc -par --no_basislib --no_delete_target_files \
  --assembler "${CC:-cc} -c" -ldexe "${CC:-cc}" -o helper helper.sml > compile.log 2>&1
./helper > actual
printf A > expected
cmp expected actual
prefix=
case $(uname -s) in Darwin) prefix=_;; esac
awk -v p="$prefix" '
  $0 == ".globl " p "__allocate" {emit=1; print ".text"}
  $0 == ".globl " p "__raise_overflow" {exit}
  emit {print}
' MLB/RI_PAR/base-link_objects.s > allocation-helpers.s
awk -v p="$prefix" '
  $0 == p "allocinreg:" {emit=1}
  emit {print}
' allocation-helpers.s > fast.s
grep -q 'lock cmpxchgq' fast.s
if grep -Eq 'push|pop|%rsp|%rbp' fast.s; then
  echo 'X64 fast allocation helper has explicit stack storage.' >&2; exit 1
fi
${CC:-cc} -O2 -Wall -Wextra -Werror -DPARALLEL \
  -Darm64_generated_alloc=x64_generated_alloc -iquote "$SML_LIB/src/Runtime" \
  "$fixtures/../../Arm64/tests/allocation-concurrent.c" \
  "$fixtures/allocation-bridge.S" allocation-helpers.s \
  "${ALLOCATION_RUNTIME:-$SML_LIB/lib/runtimeSystemPar.a}" -lm -pthread -o allocation
for scenario in single shared mixed expansion private; do
  ALLOCATION_SCENARIO=$scenario ./allocation > actual
  grep -qx 'generated/C allocation passed' actual
done
# Make every CAS fail, then perturb its returned pointer without changing flags
# or the descriptor. Fallback must retain the requested size even when CAS
# overwrites rax with a value different from the pre-attempt allocation pointer.
awk '
  /lock cmpxchgq/ {print "\txorq %rax,%rax"}
  {print}
  /lock cmpxchgq/ {print "\tleaq 8(%rax),%rax"}
' allocation-helpers.s > failure.s
${CC:-cc} -O2 -Wall -Wextra -Werror -DPARALLEL \
  -Darm64_generated_alloc=x64_generated_alloc -iquote "$SML_LIB/src/Runtime" \
  "$fixtures/../../Arm64/tests/allocation-concurrent.c" \
  "$fixtures/allocation-bridge.S" failure.s \
  "${ALLOCATION_RUNTIME:-$SML_LIB/lib/runtimeSystemPar.a}" -lm -pthread -o failure
./failure > actual
grep -qx 'generated/C allocation passed' actual
# A shortened correctness fixture: verify results, without collecting timings.
sed 's/repeat(1000,0)/repeat(10,0)/; s/result = 100000000/result = 1000000/' \
  "$fixtures/../../tests/allocation-local.sml" > local.sml
for mode in inferred forced runtime; do
  flags=
  [ "$mode" != forced ] || flags=--alloc_protect_always
  [ "$mode" != runtime ] || flags=--no_inline_allocation
  "$MLKIT" -no_gc -par $flags --no_basislib --no_delete_target_files \
    --mlb-subdir "Probe$mode" --assembler "${CC:-cc} -c" -ldexe "${CC:-cc}" \
    -o local local.sml > "$mode.log" 2>&1
  ./local > actual
  grep -qx ok actual
  find MLB -name local.sml.s -exec cat {} \; > sites.s
  test -s sites.s
  if grep -q 'cmpxchg' sites.s; then
    echo 'CAS emitted at an allocation site.' >&2; exit 1
  fi
done
awk '
  /cmpq.*32\(%r11\)/ {emit=1}
  emit {print}
  emit && /movq %r10,\(%r11\)/ {emit=0}
' sites.s > local-fast.s
test -s local-fast.s
if grep -Eq 'push|pop|%rsp|%rbp|call ' local-fast.s; then
  echo 'Inline local allocation uses a stack or helper call.' >&2; exit 1
fi
if [ -n "${REML:-}" ]; then
  for flags in '' '-par0' '-no_par0' '-par0 --no_inline_allocation'; do
    "$REML" $flags --no_basislib --assembler "${CC:-cc} -c" -ldexe "${CC:-cc}" \
      -o local local.sml > reml.log 2>&1
    ./local > actual
    grep -qx ok actual
  done
fi
echo 'X64 inline local allocation, helper, forced failure, register preservation and generated/C stress passed.'

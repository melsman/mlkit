#!/bin/sh
# Manual diagnostics for the recorded milestone-8 forced-protection fixture.
set -eu
: "${M8_ROOT:?Set M8_ROOT to the milestone-8 source/benchmark directory}"
root=$M8_ROOT
forced=$(sed -n '1s/.*: //p' "$root/forced.log")
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-layout-saves.XXXXXX")
echo "Layout/save experiment outputs: $scratch"
cd "$scratch"
cp "$forced/MLB/RI_PAR_Localfast/main.sml.o" main.o
cp "$forced/fast" reference-current
cp "$forced/baseline" reference-old
printf 'method,offset,entry,cas,caller,after_helper,text\n' > addresses.csv
make_variant() {
  method=$1; offset=$2; variant=$method-$offset
  awk -v method="$method" -v offset="$offset" '
    $0 == ".globl allocinreg_protected" {
      print ".balign 64"; print ".Lprotected_arena:"; print ".fill " offset ",1,0x90"
    }
    /^allocinreg_protected:$/ {protected=1}
    protected && /^.text$/ {print ".org .Lprotected_arena+256,0x90"; protected=0}
    protected && /movq %rax,%xmm14/ && (method=="stack-rax" || method=="stack-both") {
      print "\tpushq %rax"; next
    }
    protected && /movq %r10,%xmm15/ && (method=="stack-size" || method=="stack-both") {
      print "\tpushq %r10"; next
    }
    protected && /movq %xmm15,%rax/ && (method=="stack-size" || method=="stack-both") {
      print "\tmovq (%rsp),%rax"; next
    }
    protected && /movq %xmm15,%r10/ && (method=="stack-size" || method=="stack-both") {
      print "\tmovq (%rsp),%r10"; next
    }
    protected && /movq %xmm14,%rax/ {
      if (method=="stack-rax") {print "\tpopq %rax"; next}
      if (method=="stack-size") print "\tleaq 8(%rsp),%rsp"
      if (method=="stack-both") {
        print "\tmovq 8(%rsp),%rax"; print "\tleaq 16(%rsp),%rsp"; next
      }
    }
    {print}
  ' "$forced/MLB/RI_PAR_Localfast/base-link_objects.s" > "$variant.s"
  cc -c "$variant.s" -o "$variant.o"
  cc -o "$variant" main.o "$variant.o" "$root/fast/lib/runtimeSystemPar.a" -lm -ldl -pthread
  "./$variant" > actual
  grep -qx ok actual
  entry=$(nm -n "$variant" | awk '$3=="allocinreg_protected" {print $1}')
  cas=$(objdump -d "$variant" | awk '/<allocinreg_protected>:/ {p=1} p && /lock cmpxchg/ {sub(":","",$1);print $1;exit}')
  caller=$(nm -n "$variant" | awk '$3 ~ /^F.build4_/ {print $1}')
  after=$(nm -n "$variant" | awk '$3=="__raise_overflow" {print $1}')
  text=$(size -A "$variant" | awk '$1==".text" {print $2}')
  printf '%s,%s,%s,%s,%s,%s,%s\n' "$method" "$offset" "$entry" "$cas" "$caller" "$after" "$text" >> addresses.csv
}
# The complete helper moves in a fixed-size arena: padding is never executed.
for offset in $(seq 0 63); do make_variant fp "$offset"; done
for method in stack-rax stack-size stack-both; do
  for offset in 0 16 32 48; do make_variant "$method" "$offset"; done
done
# The caller, downstream code and total text size must not move across variants.
awk -F, 'NR==2 {caller=$5;after=$6;text=$7} NR>1 && ($5!=caller || $6!=after || $7!=text) {exit 1}' addresses.csv
printf 'Address controls passed.\n'
# Execute real concurrent and forced-failure checks for every save strategy.
fixtures=$root/fast/src/Compiler/Backend
for method in fp stack-rax stack-size stack-both; do
  awk '
    $0 == ".globl __allocate" {emit=1;print ".text"}
    $0 == ".globl __raise_overflow" {exit}
    emit {print}
  ' "$method-0.s" > helpers.s
  for failure in normal forced; do
    input=helpers.s
    if [ "$failure" = forced ]; then
      awk '/lock cmpxchgq/ {print "\txorq %rax,%rax"} {print}' helpers.s > failure.s
      input=failure.s
    fi
    cc -O2 -Wall -Wextra -Werror -DPARALLEL \
      -Darm64_generated_alloc=x64_generated_alloc -iquote "$root/fast/src/Runtime" \
      "$fixtures/Arm64/tests/allocation-concurrent.c" "$fixtures/X64/tests/allocation-bridge.S" \
      "$input" "$root/fast/lib/runtimeSystemPar.a" -lm -pthread -o check
    for scenario in single shared mixed expansion private; do
      ALLOCATION_SCENARIO=$scenario taskset -c 0,2,4,6 ./check > actual
      grep -qx 'generated/C allocation passed' actual
    done
  done
  printf 'Correctness passed: %s\n' "$method"
done
printf 'phase,run,method,offset,real,user,sys\n' > samples.csv
sample() {
  phase=$1; run=$2; method=$3; offset=$4
  variant=$method-$offset
  case $method in reference-*) variant=$method;; esac
  /usr/bin/time -p "./$variant" > actual 2> timing
  grep -qx ok actual
  awk -v p="$phase" -v r="$run" -v m="$method" -v o="$offset" '
    $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
    END {printf "%s,%s,%s,%s,%s,%s,%s\n",p,r,m,o,real,user,sys}
  ' timing >> samples.csv
}
# Screen every byte offset, three rounds with reversed/rotated ordering.
for run in 1 2 3; do
  case $run in
    1) offsets=$(seq 0 63);;
    2) offsets=$(seq 63 -1 0);;
    3) offsets="$(seq 21 63) $(seq 0 20)";;
  esac
  count=0
  for offset in $offsets; do
    sample alignment "$run" fp "$offset"
    count=$((count+1))
    if [ $((count % 16)) -eq 0 ]; then
      sample alignment "$run" reference-current -1
      sample alignment "$run" reference-old -1
    fi
  done
  printf 'Alignment round %s passed.\n' "$run"
done
# Factorial comparison: identical allocation arithmetic; four save strategies at
# four matched entry alignments, nine rounds with alternating execution order.
for run in 1 2 3 4 5 6 7 8 9; do
  methods='fp stack-rax stack-size stack-both'; offsets='0 16 32 48'
  if [ $((run % 2)) -eq 0 ]; then
    methods='stack-both stack-size stack-rax fp'; offsets='48 32 16 0'
  fi
  for offset in $offsets; do
    for method in $methods; do sample saves "$run" "$method" "$offset"; done
  done
  sample saves "$run" reference-current -1
  sample saves "$run" reference-old -1
  printf 'Save-method round %s passed.\n' "$run"
done
printf 'Experiments passed.\n'

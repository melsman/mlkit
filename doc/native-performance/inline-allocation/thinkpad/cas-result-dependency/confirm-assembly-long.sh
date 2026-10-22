#!/bin/sh
set -eu
: "${M8_ROOT:?Set M8_ROOT}"
: "${REVERIFY_BENCHMARK:?Set REVERIFY_BENCHMARK}"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-cas-result-long.XXXXXX")
echo "Long CAS-result outputs: $scratch"
cd "$scratch"
source=$REVERIFY_BENCHMARK/long/MLB/RI_PAR_Verifycurrent
cp "$REVERIFY_BENCHMARK/long/current" current
cp "$REVERIFY_BENCHMARK/long/baseline" baseline
awk '
  /^allocinreg_protected:$/ {protected=1}
  protected && /^.text$/ {protected=0}
  protected && /movq %rax,%r11/ {
    print "\tmovq %xmm15,%r11";print "\tsalq $3,%r11"
    print "\tnegq %r11";print "\taddq %r10,%r11";next
  }
  {print}
' "$source/base-link_objects.s" > recompute.s
cc -c recompute.s -o recompute.o
cc -o recompute "$source/main.sml.o" recompute.o "$M8_ROOT/fast/lib/runtimeSystemPar.a" -lm -ldl -pthread
./recompute > actual
grep -qx ok actual
printf 'block,position,variant,real,user,sys\n' > samples.csv
for block in 1 2 3 4 5; do
  order='current recompute recompute current baseline'
  [ $((block % 2)) -ne 0 ] || order='baseline recompute current current recompute'
  position=0
  for variant in $order; do
    position=$((position+1))
    /usr/bin/time -p "./$variant" > actual 2> timing
    grep -qx ok actual
    awk -v b="$block" -v p="$position" -v v="$variant" '
      $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
      END {printf "%s,%s,%s,%s,%s,%s\n",b,p,v,real,user,sys}
    ' timing >> samples.csv
  done
  printf 'Long comparison block %s passed.\n' "$block"
done

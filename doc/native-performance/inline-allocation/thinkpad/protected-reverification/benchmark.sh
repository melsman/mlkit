#!/bin/sh
# Manual re-verification: fresh programs, untimed protection audit, ABBA blocks.
set -eu
: "${M8_ROOT:?Set M8_ROOT to the recorded baseline/candidate source directory}"
root=$M8_ROOT
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-protected-reverify.XXXXXX")
echo "Re-verification outputs: $scratch"
cd "$scratch"
export SML_LIB=$root/fast
runtime=$SML_LIB/lib/runtimeSystemPar.a
{ date -Is; uname -a; uptime; powerprofilesctl get; cat /sys/class/power_supply/AC*/online;
  sha256sum "$root/baseline/bin/mlkit" "$root/fast/bin/mlkit" "$runtime";
} > host-before.txt
cat > audit.c <<'C'
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include "Spawn.h"
extern uint64_t mlkit_audit_helper_calls;
extern Region __real_allocateRegion(Context, Region, Protect);
static uint64_t region_calls;
Region __wrap_allocateRegion(Context ctx, Region r, Protect p) {
  assert(p == 1);
  Region result = __real_allocateRegion(ctx,r,p);
  assert(result->mutex != NULL);
  ++region_calls;
  return result;
}
__attribute__((destructor)) static void report(void) {
  assert(mlkit_audit_helper_calls == EXPECTED_CALLS);
  assert(region_calls >= EXPECTED_REGIONS);
  fprintf(stderr,"Protected helper calls: %" PRIu64 "; protected region constructors: %" PRIu64 "\n",
          mlkit_audit_helper_calls,region_calls);
}
C
for workload in short long; do
  mkdir "$workload"
  cp "$SML_LIB/src/Compiler/Backend/tests/allocation-local.sml" "$workload/main.sml"
  calls=100000000; regions=1000
  if [ "$workload" = long ]; then
    sed 's/repeat(1000,0)/repeat(5000,0)/; s/result = 100000000/result = 500000000/' \
      "$workload/main.sml" > "$workload/long.sml"
    mv "$workload/long.sml" "$workload/main.sml"
    calls=500000000; regions=5000
  fi
  cc -O2 -Wall -Wextra -Werror -DPARALLEL -DEXPECTED_CALLS=$calls -DEXPECTED_REGIONS=$regions \
    -iquote "$SML_LIB/src/Runtime" -c audit.c -o "$workload/audit.o"
  for variant in baseline current; do
    compiler=$root/baseline/bin/mlkit
    [ "$variant" != current ] || compiler=$root/fast/bin/mlkit
    (cd "$workload"; "$compiler" -no_gc -par --alloc_protect_always --no_basislib \
      --no_delete_target_files --mlb-subdir "Verify$variant" -o "$variant" main.sml > "$variant-compile.log" 2>&1)
    exe=$workload/$variant
    "$exe" > actual
    grep -qx ok actual
    dir=$workload/MLB/RI_PAR_Verify$variant
    # Instrument only the audit copy; the timed executable/object is untouched.
    awk -v variant="$variant" '
      variant=="baseline" && $1=="jne" && $2 ~ /^L_alloc_protect_/ {label=$2}
      {print}
      (variant=="baseline" && $0==label ":") || (variant=="current" && $0=="allocinreg_protected:") {
        print "\tincq mlkit_audit_helper_calls(%rip)"
      }
      END {print ".comm mlkit_audit_helper_calls,8,8"}
    ' "$dir/base-link_objects.s" > "$workload/$variant-audit.s"
    cc -c "$workload/$variant-audit.s" -o "$workload/$variant-audit.o"
    cc -Wl,--wrap=allocateRegion -o "$workload/$variant-audit" "$dir/main.sml.o" \
      "$workload/$variant-audit.o" "$workload/audit.o" "$runtime" -lm -ldl -pthread
    taskset -c 0 "$workload/$variant-audit" > actual 2> "$workload/$variant-audit.log"
    grep -qx ok actual
    (cd "$workload"; size "$variant"; size "MLB/RI_PAR_Verify$variant/main.sml.o" \
      "MLB/RI_PAR_Verify$variant/base-link_objects.o"; sha256sum "$variant") > "$workload/$variant-size.txt"
    objdump -d "$exe" > "$workload/$variant.dis"
  done
  printf 'Fresh builds and protection audit passed: %s\n' "$workload"
done
printf 'workload,cpu,block,position,variant,real,user,sys\n' > samples.csv
sample() {
  workload=$1; cpu=$2; block=$3; position=$4; variant=$5
  taskset -c "$cpu" /usr/bin/time -p "./$workload/$variant" > actual 2> timing
  grep -qx ok actual
  awk -v w="$workload" -v c="$cpu" -v b="$block" -v p="$position" -v v="$variant" '
    $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
    END {printf "%s,%s,%s,%s,%s,%s,%s,%s\n",w,c,b,p,v,real,user,sys}
  ' timing >> samples.csv
}
# Short workload on two distinct physical performance cores. ABBA blocks balance
# ordering/drift; reversing alternate blocks gives 12 observations per version/core.
for cpu in 0 2; do
  for block in 1 2 3 4 5 6; do
    order='baseline current current baseline'
    [ $((block % 2)) -ne 0 ] || order='current baseline baseline current'
    position=0
    for variant in $order; do
      position=$((position+1)); sample short "$cpu" "$block" "$position" "$variant"
    done
  done
  printf 'Short ABBA runs passed: CPU %s\n' "$cpu"
done
# Five times more allocation per process reduces timer-rounding/startup effects.
# Five ABBA blocks give ten observations per version.
for block in 1 2 3 4 5; do
  order='baseline current current baseline'
  [ $((block % 2)) -ne 0 ] || order='current baseline baseline current'
  position=0
  for variant in $order; do
    position=$((position+1)); sample long 0 "$block" "$position" "$variant"
  done
  printf 'Long ABBA block %s passed.\n' "$block"
done
{ date -Is; uptime; powerprofilesctl get; cat /sys/class/power_supply/AC*/online; } > host-after.txt
printf 'Re-verification passed.\n'

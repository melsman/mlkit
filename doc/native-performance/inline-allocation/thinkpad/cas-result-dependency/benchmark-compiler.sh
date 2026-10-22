#!/bin/sh
# Manual before/after compiler comparison with retained spawning metadata.
set -eu
: "${FIX_ROOT:?Set FIX_ROOT to the current PR build with before-result compilers}"
root=$FIX_ROOT
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-cas-compiler-benchmark.XXXXXX")
echo "Compiler-fix benchmark outputs: $scratch"
cd "$scratch"
export SML_LIB=$root
cat > keep.c <<'C'
#include <assert.h>
#include <stdint.h>
extern uintptr_t mlkit_regions_may_spawn;
void mlkit_spawn_unit_keep(void) {}
void mlkit_assert_may_spawn(void) { assert(mlkit_regions_may_spawn == 1); }
C
cc -O2 -c keep.c -o keep.o
cat > spawner.sml <<'SML'
fun retained_spawn__noinline (f:unit->unit) : foreignptr =
  prim("spawnone",(prim("pointer",f):foreignptr))
val _:unit = prim("mlkit_spawn_unit_keep",())
SML
printf 'spawner.sml\nmain.sml\n' > program.mlb
printf 'val _:unit = prim("mlkit_assert_may_spawn",())\n' > short.sml
cat "$root/src/Compiler/Backend/tests/allocation-local.sml" >> short.sml
sed 's/repeat(1000,0)/repeat(5000,0)/; s/result = 100000000/result = 500000000/' short.sml > long.sml
printf 'compiler,workload,block,position,variant,real,user,sys\n' > samples.csv
for kind in mlkit reml; do
  flags='-no_gc -par --alloc_protect_always'
  [ "$kind" != reml ] || flags='-par -no_par0 --alloc_protect_always'
  for workload in short long; do
    cp "$workload.sml" main.sml
    for variant in before after; do
      compiler=$root/bin/$kind
      [ "$variant" != before ] || compiler=$root/bin/$kind-before-result
      "$compiler" $flags --no_basislib --no_delete_target_files --mlb-subdir "Cas${kind}${variant}" \
        -ldexe "cc $scratch/keep.o" -o "$kind-$workload-$variant" program.mlb \
        > "$kind-$workload-$variant-compile.log" 2>&1
      "./$kind-$workload-$variant" > actual
      grep -qx ok actual
      cp MLB/RI_PAR_Cas${kind}${variant}/base-link_objects.s "$kind-$workload-$variant-link.s"
      size "$kind-$workload-$variant" MLB/RI_PAR_Cas${kind}${variant}/main.sml.o \
        MLB/RI_PAR_Cas${kind}${variant}/base-link_objects.o > "$kind-$workload-$variant-size.txt"
    done
  done
done
# All builds finish before timing to avoid compiler activity perturbing samples.
for kind in mlkit reml; do
  for workload in short long; do
    for block in 1 2 3 4 5; do
      order='before after after before'
      [ $((block % 2)) -ne 0 ] || order='after before before after'
      position=0
      for variant in $order; do
        position=$((position+1))
        taskset -c 0 /usr/bin/time -p "./$kind-$workload-$variant" > actual 2> timing
        grep -qx ok actual
        awk -v k="$kind" -v w="$workload" -v b="$block" -v p="$position" -v v="$variant" '
          $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
          END {printf "%s,%s,%s,%s,%s,%s,%s,%s\n",k,w,b,p,v,real,user,sys}
        ' timing >> samples.csv
      done
    done
    printf 'Compiler comparison passed: %s %s\n' "$kind" "$workload"
  done
done

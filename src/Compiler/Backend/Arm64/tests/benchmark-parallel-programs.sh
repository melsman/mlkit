#!/bin/sh
# Whole-program comparison against an unchanged ARM64 compiler.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT_ARM64:?Set MLKIT_ARM64}"
: "${MLKIT_BASELINE:?Set MLKIT_BASELINE to the baseline compiler}"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-parallel-benchmark.XXXXXX")
echo "Parallel program benchmark outputs: $scratch"
printf 'program,run,variant,real,user,sys\n' > "$scratch/samples.csv"
for sample in spawn_msort spawn_alloccomp; do
  mkdir "$scratch/$sample"
  cd "$scratch/$sample"
  cp "$SML_LIB/test/parallelism/$sample.sml" main.sml
  if [ "$sample" = spawn_alloccomp ]; then
    # Ten computation rounds instead of 100; preserve list length and workers.
    sed 's/compk 100/compk 10/' main.sml > reduced.sml
    mv reduced.sml main.sml
  fi
  for variant in baseline fast; do
    compiler=$MLKIT_ARM64
    [ "$variant" != baseline ] || compiler=$MLKIT_BASELINE
    "$compiler" -no_gc -par --no_basislib --no_delete_target_files \
      --mlb-subdir "Atomic$variant" -o "$variant" main.sml > "$variant-compile.log" 2>&1
  done
  ./baseline > expected
  ./fast > actual
  cmp expected actual
  if [ "$sample" = spawn_msort ]; then grep -qx ok expected; fi
  for run in 1 2 3 4 5 6 7 8 9; do
    order='baseline fast'
    [ $((run % 2)) -ne 0 ] || order='fast baseline'
    for variant in $order; do
      /usr/bin/time -p "./$variant" > actual 2> timing
      cmp expected actual
      awk -v p="$sample" -v r="$run" -v v="$variant" '
        $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
        END {printf "%s,%s,%s,%s,%s,%s\n",p,r,v,real,user,sys}
      ' timing >> "$scratch/samples.csv"
    done
  done
done
cat "$scratch/samples.csv"

#!/bin/sh
# Compare generated allocation-local loops with the previous compiler.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT:?Set MLKIT}"
: "${MLKIT_BASELINE:?Set MLKIT_BASELINE}"
fixtures=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-local-benchmark.XXXXXX")
echo "Allocation-local benchmark outputs: $scratch"
cd "$scratch"
cp "$fixtures/allocation-local.sml" main.sml
for variant in baseline fast; do
  compiler=$MLKIT
  [ "$variant" != baseline ] || compiler=$MLKIT_BASELINE
  # ALLOCATION_FLAGS intentionally supports a list of compiler options.
  "$compiler" ${ALLOCATION_FLAGS:--no_gc -par} --no_basislib --no_delete_target_files \
    --mlb-subdir "Local$variant" -o "$variant" main.sml > "$variant-compile.log" 2>&1
  "./$variant" > actual
  grep -qx ok actual
  size "$variant" > "$variant-size.txt"
done
printf 'run,variant,real,user,sys\n' > samples.csv
for run in 1 2 3 4 5 6 7 8 9; do
  order='baseline fast'
  [ $((run % 2)) -ne 0 ] || order='fast baseline'
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

#!/bin/sh
# Manual only: generated ML reset-site comparison, including ARM64 placement.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${REML_BEFORE:?Set REML_BEFORE}"
: "${REML_AFTER:?Set REML_AFTER}"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-reset-loop.XXXXXX")
echo "Reset loop outputs: $scratch"
cd "$scratch"
for mode in seq par prof; do
  flags=-no_par
  [ "$mode" != par ] || flags=-par
  [ "$mode" != prof ] || flags='-no_par -rp'
  for variant in before after; do
    compiler=$REML_BEFORE; [ "$variant" != after ] || compiler=$REML_AFTER
    mkdir "$mode-$variant"; cd "$mode-$variant"
    cp "$SML_LIB/src/Compiler/Backend/tests/reset-loop.sml" .
    "$compiler" $flags --no_basislib --no_delete_target_files -o reset-loop reset-loop.sml > compile.log 2>&1
    ./reset-loop > actual
    grep -qx 'reset loop passed' actual
    size reset-loop > size.txt
    cd ..
  done
done
printf 'mode,round,variant,user_seconds\n' > samples.csv
for mode in seq par prof; do
  for round in 1 2 3 4 5; do
    order='before after'; [ $((round%2)) != 0 ] || order='after before'
    for variant in $order; do
      set --
      [ "$(uname -s)" != Linux ] || set -- taskset -c "${BENCH_CPU:-0}"
      /usr/bin/time -p "$@" "./$mode-$variant/reset-loop" > actual 2> timing
      grep -qx 'reset loop passed' actual
      printf '%s,%s,%s,%s\n' "$mode" "$round" "$variant" "$(awk '/^user / {print $2}' timing)" >> samples.csv
    done
  done
done
cat samples.csv

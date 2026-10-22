#!/bin/sh
# Manual only: unchanged standard programs, exactly three executions per binary.
set -eu
: "${SML_LIB:?Set SML_LIB to the source/runtime checkout}"
: "${MLKIT_BEFORE:?Set MLKIT_BEFORE}"
: "${MLKIT_AFTER:?Set MLKIT_AFTER}"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-sequential.XXXXXX")
echo "Sequential benchmark outputs: $scratch"
fixtures=$SML_LIB/src/Compiler/Backend/tests
programs='kitfib35 kitlife35u kitqsort kittmergesort kitkbjul9 kitsimple kitmandelbrot fft nucleic ray'
${CC:-cc} -O2 -Wall -Wextra -Werror "$fixtures/benchmark-sequential-time.c" -o "$scratch/timer"
tag=$(basename "$scratch" | tr -cd '[:alnum:]')
for variant in before after; do
  mkdir "$scratch/$variant"
  git -C "$SML_LIB" archive HEAD test | tar -xf - -C "$scratch/$variant"
  cd "$scratch/$variant/test"
  compiler=$MLKIT_BEFORE; [ "$variant" != after ] || compiler=$MLKIT_AFTER
  for name in $programs; do
    project=$name.mlb
    case $name in
      nucleic|ray) expected=$name.mlb.out.ok;;
      *) project=benchmark-$name.mlb; expected=$name.sml.out.ok
         printf '$(SML_LIB)/basis/basis.mlb\n%s.sml\n' "$name" > "$project";;
    esac
    "$compiler" -gc -no_par --no_basislib --no_delete_target_files \
      --mlb-subdir "Seq$tag$variant" -o "$name.exe" "$project" > "$name-compile.log" 2>&1 || {
        cat "$name-compile.log"; exit 1;
      }
    size "$name.exe" > "$name-size.txt"
    cp "$expected" "$name.expected"
  done
done
cd "$scratch"
printf 'program,round,variant,user_seconds,sys_seconds,wall_seconds,max_rss_kib\n' > samples.csv
for name in $programs; do
  for round in 1 2 3; do
    order='before after'; [ "$round" != 2 ] || order='after before'
    for variant in $order; do
      cd "$scratch/$variant/test"
      set --; [ "$(uname -s)" != Linux ] || set -- taskset -c "${BENCH_CPU:-0}"
      "$@" "$scratch/timer" "$name-$round.out" "./$name.exe" > "$name-$round.time" 2> "$name-$round.stderr"
      cmp "$name.expected" "$name-$round.out"
      printf '%s,%s,%s,%s\n' "$name" "$round" "$variant" "$(cat "$name-$round.time")" >> "$scratch/samples.csv"
    done
  done
  echo "Sequential $name: three pairs passed"
done
cat "$scratch/samples.csv"

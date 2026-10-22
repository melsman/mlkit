#!/bin/sh
# Manual only: fifteen unmodified mlkit-bench programs, three runs per binary.
set -eu
: "${SML_LIB:?Set SML_LIB to the common source/runtime checkout}"
: "${MLKIT_BEFORE:?Set MLKIT_BEFORE}"
: "${MLKIT_AFTER:?Set MLKIT_AFTER}"
: "${BENCH_ROOT:?Set BENCH_ROOT to the mlkit-bench checkout}"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-bench-pr253.XXXXXX")
echo "MLKit-bench outputs: $scratch"
fixtures=$SML_LIB/src/Compiler/Backend/tests
programs='barnes-hut fft fib37 kbc lexgen life logic mandelbrot mlyacc msort nucleic ray simple tak vliw'
${CC:-cc} -O2 -Wall -Wextra -Werror "$fixtures/benchmark-sequential-time.c" -o "$scratch/timer"
git -C "$BENCH_ROOT" rev-parse HEAD > "$scratch/benchmark-revision.txt"
for variant in before after; do
  stage=$scratch/$variant
  mkdir "$stage"
  # Isolate prepared libraries; never overwrite the user's installed caches.
  git -C "$SML_LIB" archive HEAD basis kitlib ml-yacc-lib | tar -xf - -C "$stage"
  ln -s "$SML_LIB/lib" "$stage/lib"
  compiler=$MLKIT_BEFORE; [ "$variant" != after ] || compiler=$MLKIT_AFTER
  export SML_LIB_STAGE=$stage
  echo "Preparing Basis ($variant)"
  SML_LIB=$stage "$compiler" -gc -no_par -c "$stage/basis/repl.mlb" > "$stage/basis-build.log" 2>&1 || {
    tail -40 "$stage/basis-build.log"; exit 1;
  }
  git -C "$BENCH_ROOT" archive HEAD benchmarks | tar -xf - -C "$stage"
  cd "$stage/benchmarks"
  for name in $programs; do
    source=$name.sml
    case $name in barnes-hut|logic|mlyacc|nucleic|ray) source=$name.mlb;; esac
    SML_LIB=$stage "$compiler" -gc -no_par --no_delete_target_files \
      -o "$name.exe" "$source" > "$name-compile.log" 2>&1 || {
        tail -40 "$name-compile.log"; exit 1;
      }
    size "$name.exe" > "$name-size.txt"
    cp "$source.out.ok" "$name.expected"
  done
done
cd "$scratch"
printf 'program,round,variant,user_seconds,sys_seconds,wall_seconds,max_rss_kib\n' > samples.csv
for name in $programs; do
  for round in 1 2 3; do
    order='before after'; [ "$round" != 2 ] || order='after before'
    for variant in $order; do
      cd "$scratch/$variant/benchmarks"
      set --; [ "$(uname -s)" != Linux ] || set -- taskset -c "${BENCH_CPU:-0}"
      SML_LIB=$scratch/$variant "$@" "$scratch/timer" "$name-$round.out" "./$name.exe" \
        > "$name-$round.time" 2> "$name-$round.stderr"
      cmp "$name.expected" "$name-$round.out"
      printf '%s,%s,%s,%s\n' "$name" "$round" "$variant" "$(cat "$name-$round.time")" >> "$scratch/samples.csv"
    done
  done
  echo "MLKit-bench $name: three pairs passed"
done
cat "$scratch/samples.csv"

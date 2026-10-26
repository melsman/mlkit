#!/bin/sh
# Manual native assessment only. Never run timing collection in CI.
set -eu
: "${SML_LIB:?Set SML_LIB to the built source/runtime checkout}"
: "${MLKIT:?Set MLKIT to the compiler under test}"
: "${BENCH_ROOT:?Set BENCH_ROOT to mlkit-bench}"
case $(uname -s)-$(uname -m) in
  Darwin-arm64) link_flags='-arch arm64 -Wl,-stack_size,0x10000000';;
  Linux-x86_64) link_flags=;;
  *) echo 'Unsupported native target' >&2; exit 1;;
esac
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-page-pool-integrated.XXXXXX")
echo "Page-pool benchmark outputs: $scratch"
programs='barnes-hut fft fib37 kbc lexgen life logic mandelbrot mlyacc mpuz msort nucleic professor ratio ray simple tak tsp vliw zebra'
fixtures=$SML_LIB/src/Compiler/Backend/tests
${CC:-cc} -O2 -Wall -Wextra -Werror "$fixtures/benchmark-sequential-time.c" -o "$scratch/timer"
git -C "$BENCH_ROOT" rev-parse HEAD > "$scratch/benchmark-revision.txt"
mkdir "$scratch/stage" "$scratch/executables"
git -C "$SML_LIB" archive HEAD basis kitlib ml-yacc-lib | tar -xf - -C "$scratch/stage"
ln -s "$SML_LIB/lib" "$scratch/stage/lib"
stage=$scratch/stage
SML_LIB=$stage "$MLKIT" -ri -par -no_gc -c "$stage/basis/repl.mlb" > "$scratch/basis-build.log" 2>&1
git -C "$BENCH_ROOT" archive HEAD benchmarks | tar -xf - -C "$stage"
# A reference-only linker override uses the ordinary parallel runtime. The
# candidate keeps the compiler's automatic selection; neither needs a new flag.
cat > "$scratch/linker" <<'LINKER'
#!/bin/sh
set -eu
for argument do
  shift
  if [ "$PAGE_POOL_REFERENCE" = 1 ]; then
    case "$argument" in
      */runtimeSystemParNoSpawn.a) argument=${argument%runtimeSystemParNoSpawn.a}runtimeSystemPar.a;;
    esac
  fi
  set -- "$@" "$argument"
done
printf '%s\n' "$@" > "$PAGE_POOL_LINK_LOG"
exec ${CC:-cc} "$@"
LINKER
chmod +x "$scratch/linker"
cd "$stage/benchmarks"
for name in $programs; do
  source=$name.sml
  case $name in barnes-hut|logic|mlyacc|nucleic|ray) source=$name.mlb;; esac
  cp "$source.out.ok" "$scratch/$name.expected"
  for variant in reference automatic; do
    reference=0; [ "$variant" != reference ] || reference=1
    PAGE_POOL_REFERENCE=$reference PAGE_POOL_LINK_LOG=$scratch/$name-$variant-link-args.txt \
      SML_LIB=$stage "$MLKIT" -ri -par -no_gc -debug_linking --no_delete_target_files \
      -ldexe "$scratch/linker $link_flags" \
      -o "$scratch/executables/$name-$variant.exe" "$source" > "$scratch/$name-$variant-build.log" 2>&1
    archive=runtimeSystemParNoSpawn.a
    [ "$variant" != reference ] || archive=runtimeSystemPar.a
    grep -q "/$archive$" "$scratch/$name-$variant-link-args.txt"
    nm -n "$scratch/executables/$name-$variant.exe" | awk 'NF == 3 && /F\./' > "$scratch/$name-$variant-ml-symbols.txt"
  done
  cmp "$scratch/$name-reference-ml-symbols.txt" "$scratch/$name-automatic-ml-symbols.txt"
  echo "$name: both executables built with identical ML function addresses"
done
printf 'program,round,variant,user_seconds,sys_seconds,wall_seconds,max_rss_kib\n' > "$scratch/samples.csv"
for name in $programs; do
  for round in 1 2 3 4 5; do
    case $round in 1|3|5) order='reference automatic';; *) order='automatic reference';; esac
    for variant in $order; do
      set --
      [ "$(uname -s)" != Linux ] || set -- taskset -c "${BENCH_CPU:-0}"
      "$@" "$scratch/timer" "$scratch/$name-$variant-$round.out" "$scratch/executables/$name-$variant.exe" \
        > "$scratch/$name-$variant-$round.time" 2> "$scratch/$name-$variant-$round.err"
      cmp "$scratch/$name.expected" "$scratch/$name-$variant-$round.out"
      printf '%s,%s,%s,%s\n' "$name" "$round" "$variant" "$(cat "$scratch/$name-$variant-$round.time")" >> "$scratch/samples.csv"
    done
  done
  echo "$name: five pairs passed"
done

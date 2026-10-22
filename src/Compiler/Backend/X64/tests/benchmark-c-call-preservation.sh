#!/bin/sh
# Manual native X64 before/after assessment. Never run this from CI.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT_BEFORE:?Set MLKIT_BEFORE}"
: "${MLKIT_AFTER:?Set MLKIT_AFTER}"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-c-preservation-bench.XXXXXX")
echo "C preservation benchmark: $scratch"
cd "$scratch"
cp "$SML_LIB/src/Compiler/Backend/X64/tests/c-call-preservation.S" .
${CC:-cc} -c c-call-preservation.S -o clobber.o
cat > loop.sml <<'SML'
infix 6 + -
infix 4 =
fun op = (x: ''a,y: ''a): bool = prim("=",(x,y))
fun loop__noinline(0,acc:int) = acc
  | loop__noinline(n,acc) =
      let val r:int = prim("preservation_identity",acc)
      in loop__noinline(n-1,r+n) end
val result = loop__noinline(200000000,0)
val () = prim("printStringML",if result = 20000000100000000 then "OK\n" else "BAD\n"):unit
SML
sed 's/preservation_identity/@preservation_identity/' loop.sml > converted.sml
cat > pressure.sml <<'SML'
infix 6 + -
infix 4 =
fun op = (x: ''a,y: ''a): bool = prim("=",(x,y))
fun loop__noinline(0,acc:int,a:int,b:int,c:int,d:int) = acc+a+b+c+d
  | loop__noinline(n,acc,a,b,c,d) =
      let val r:int = prim("preservation_identity",acc)
      in loop__noinline(n-1,r+n,a,b,c,d) end
val result = loop__noinline(200000000,0,1,2,3,4)
val () = prim("printStringML",if result = 20000000100000010 then "OK\n" else "BAD\n"):unit
SML
for workload in loop converted pressure; do
  for variant in before after; do
    compiler=$MLKIT_BEFORE
    [ "$variant" != after ] || compiler=$MLKIT_AFTER
    "$compiler" -no_gc --no_basislib --no_delete_target_files \
      --mlb-subdir "Bench$variant" -ldexe "${CC:-cc} clobber.o" \
      -o "$workload-$variant" "$workload.sml" > "$workload-$variant-compile.log" 2>&1
    size "$workload-$variant" > "$workload-$variant-size.txt"
    ./$workload-$variant > actual
    grep -qx OK actual
  done
done
printf 'workload,round,variant,user_seconds\n' > samples.csv
for workload in loop converted pressure; do
for round in 1 2 3 4 5; do
  order='before after after before'
  [ $((round % 2)) != 0 ] || order='after before before after'
  for variant in $order; do
    /usr/bin/time -f "$workload,$round,$variant,%U" -o sample \
      taskset -c "${BENCH_CPU:-0}" ./$workload-$variant > actual
    grep -qx OK actual
    cat sample >> samples.csv
  done
done
done
cat samples.csv

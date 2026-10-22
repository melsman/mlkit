#!/bin/sh
# Manual only: compare identical global allocation with/without retained spawning.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${MLKIT:?Set MLKIT}"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-no-spawn-benchmark.XXXXXX")
echo "No-spawn benchmark outputs: $scratch"
cd "$scratch"
printf 'void mlkit_spawn_unit_keep(void) {}\n' > keep.c
${CC:-cc} -O2 -c keep.c -o keep.o
cat > spawner.sml <<'SML'
fun retained_spawn__noinline (f:unit->unit) : foreignptr =
  prim("spawnone",(prim("pointer",f):foreignptr))
val _:unit = prim("mlkit_spawn_unit_keep",())
SML
cat > build.sml <<'SML'
infix 6 -
infixr 5 ::
fun build__noinline (0,xs) = xs
  | build__noinline (n:int,xs) = build__noinline(n-1,1::xs)
SML
# Export the list to another unit so its region cannot be reclaimed locally.
printf 'val xs = build__noinline(10000000,nil)\n' > globals.sml
cat > consume.sml <<'SML'
infix 6 +
infix 4 =
fun op = (a:int,b:int) : bool = prim("=",(a,b))
fun sum__noinline ([],a:int) = a
  | sum__noinline (x::xs,a) = sum__noinline(xs,a+x)
val () = if sum__noinline(xs,0) = 10000000 then prim("printStringML","ok\n")
         else prim("printStringML","incorrect result\n")
SML
printf 'build.sml\nglobals.sml\nconsume.sml\n' > plain.mlb
printf 'spawner.sml\nbuild.sml\nglobals.sml\nconsume.sml\n' > protected.mlb
for variant in sequential no-spawn protected; do
  flags=-par; program=plain
  [ "$variant" != sequential ] || flags=-no_par
  [ "$variant" != protected ] || program=protected
  "$MLKIT" -no_gc $flags --no_basislib --no_delete_target_files \
    --mlb-subdir NoSpawn --assembler "${CC:-cc} -c" -ldexe "${CC:-cc} keep.o" \
    -o "$variant" "$program.mlb" > "$variant-compile.log" 2>&1
  "./$variant" > actual
  grep -qx ok actual
  size "$variant" > "$variant-size.txt"
done
printf 'run,variant,real,user,sys\n' > samples.csv
for run in 1 2 3 4 5 6 7 8 9; do
  order='sequential no-spawn protected'
  [ $((run % 2)) -ne 0 ] || order='protected no-spawn sequential'
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

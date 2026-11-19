#!/bin/sh
# Exercise executable/reusable cache separation in both directions, and REPL loads.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
OUT=${FINAL_EXPORT_TEST_OUTPUTS:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-final-exports.XXXXXX")}
export SML_LIB="$ROOT"
mkdir -p "$OUT"
cd "$OUT"
ln -s "$ROOT/basis" basis
cat > producer.sml <<'SML'
val object = (ref 42, 0)
fun inspect 0 (p:int ref * int) = !(#1 p)
  | inspect n p = inspect (n-1) p
val () = print ("producer: " ^ Int.toString(inspect 3 object) ^ "\n")
SML
cat > producer.mlb <<'MLB'
basis/basis.mlb
producer.sml
MLB
cat > client.sml <<'SML'
val () = print ("client: " ^ Int.toString(inspect 2 object) ^ "\n")
SML
printf 'producer.mlb\nclient.sml\n' > client.mlb
cat > split.sml <<'SML'
val early = ref 40
functor F(X:sig val n:int end) = struct val answer = !early + X.n end
structure Result = F(struct val n = 2 end)
val () = print ("split: " ^ Int.toString Result.answer ^ "\n")
SML
"$MLKIT" --no_unbox_funargs --report_data_flattening -o producer "$OUT/producer.mlb" > final.log 2>&1
./producer > final.out
grep -q 'producer: 42' final.out
grep -q 'data flattening: transformed' final.log
"$MLKIT" --no_unbox_funargs --no_prune_final_exports -o reusable "$OUT/producer.mlb" > reusable.log 2>&1
./reusable > reusable.out
cmp final.out reusable.out
"$MLKIT" -o client "$OUT/client.mlb" > client.log 2>&1
./client > client.out
grep -q 'client: 42' client.out
"$MLKIT" --no_unbox_funargs -o producer "$OUT/producer.mlb" > cached.log 2>&1
./producer > cached.out
cmp final.out cached.out
"$MLKIT" -c "$OUT/producer.mlb" > compile-only.log 2>&1
# The root link metadata needs its own directory even when the last source
# lives elsewhere and the root directory contains no source files.
mkdir -p project/nested
cp producer.sml project/nested/main.sml
printf '../basis/basis.mlb\nnested/main.sml\n' > project/nested.mlb
"$MLKIT" -o nested-program "$OUT/project/nested.mlb" > nested.log 2>&1
./nested-program > nested.out
cmp final.out nested.out
for inline in inline_functors separate_functors; do
  cp split.sml "$inline.sml"
  case "$inline" in inline_functors) switches=--inline_functors ;; *) switches= ;; esac
  "$MLKIT" $switches -o split "$inline.sml" > "$inline.log" 2>&1
  ./split > "$inline.out"
  grep -q 'split: 42' "$inline.out"
done
printf ':load %s/producer.mlb;\nval answer = inspect 2 object;\n:quit;\n' "$OUT" |
  "$MLKIT" > repl.log 2>&1
grep -q 'answer.*42' repl.log
if grep -E 'Unbound|uncaught|Type clash|Compile error' repl.log; then exit 1; fi
printf 'Final-export checks passed: %s\n' "$OUT"

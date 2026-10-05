#!/bin/sh
# Run with a freshly built compiler and its matching Basis/runtime libraries.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-ir.XXXXXX")
cleanup () {
  status=$?
  if [ "$status" -eq 0 ]; then rm -rf "$OUT"
  else echo "IR test artifacts retained at $OUT" >&2; cat "$OUT/build.log" >&2
  fi
}
trap cleanup EXIT
cat > "$OUT/main.sml" <<'SML'
fun build 0 = []
  | build n = (n,n+1)::build (n-1)
val pairs = build 20
val box = ref pairs
val _ = print (Int.toString (length (!box)) ^ "\n")
SML
cd "$OUT"
"$MLKIT" -no_gc -rp -Pcee -Pcee_locations -Ppp -Pole -log_to_file -o program main.sml > build.log 2>&1
./program > result
[ "$(cat result)" = 20 ]
set -- MLB/*/main.sml.o.ir
[ "$#" -eq 1 ] && [ -f "$1" ]
ir=$1
# The dedicated artifact contains neither other diagnostic passes nor inline pp IDs.
! grep -q 'Program After\|pp[0-9]' "$ir"
grep -q '^MLKIT-IR 3$' "$ir"
grep -q '^MLKIT-IR-LOCATIONS 1$' "$ir"
# Independently verify absolute byte offsets, one-based line/byte columns,
# and that every span selects a complete allocation specifier.
LC_ALL=C awk '
  { lines[NR]=$0; full=full $0 "\n" }
  /^MLKIT-IR-LOCATIONS 1$/ { table=1; next }
  table && /^[0-9]+\t/ { mark[++n]=$1; start[n]=$2; len[n]=$3; row[n]=$4; col[n]=$5 }
  END {
    if (!n) exit 1;
    for (i=1;i<=n;i++) {
      s=substr(full,start[i]+1,len[i]);
      if ((s !~ /^(attop|atbot|sat) / && s !~ /^\$/ ) || s!=substr(lines[row[i]],col[i],len[i])) exit 2;
    }
  }' "$ir"
cp "$ir" saved.ir
"$MLKIT" -no_gc -rp -o program main.sml > build.log 2>&1
cmp "$ir" saved.ir
! grep -q 'reading source file:.*main.sml' build.log
# Missing/corrupt companion files must invalidate an otherwise reusable object.
rm "$ir"
"$MLKIT" -no_gc -rp -o program main.sml > build.log 2>&1
[ -f "$ir" ]
grep -q 'reading source file:.*main.sml' build.log
printf '\ncorrupt\n' >> "$ir"
"$MLKIT" -no_gc -rp -o program main.sml > build.log 2>&1
grep -q 'reading source file:.*main.sml' build.log
! grep -q '^corrupt$' "$ir"
# Ordinary -Pcee has no table; the table flag alone does not request IR output.
cp main.sml plain.sml
"$MLKIT" -no_gc -Pcee -log_to_file -o plain plain.sml > build.log 2>&1
grep -q 'Program After Application Conversion' plain.sml.log
! grep -q 'MLKIT-IR-LOCATIONS' plain.sml.log
cp main.sml table-only.sml
"$MLKIT" -no_gc -Pcee_locations -log_to_file -o table-only table-only.sml > build.log 2>&1
! grep -q 'MLKIT-IR-BEGIN' table-only.sml.log
# Non-profiling builds do not create automatic IR artifacts.
! test -f MLB/*/plain.sml.o.ir
echo 'IR artifacts: generation, spans, cache reuse/invalidation and printing flags passed'

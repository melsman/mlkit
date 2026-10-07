#!/bin/sh
# Release validation, separate from the small deterministic CI fixtures.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
export SML_LIB=${SML_LIB:-$ROOT}
export RPVIEW
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-ir11-examples.XXXXXX")
echo "IR11 examples: $OUT"
"$MLKIT" -no_gc -rp -o "$OUT/msort" "$ROOT/test/msort.mlb" > "$OUT/msort.build" 2>&1
"$MLKIT" -no_gc -rp -o "$OUT/kkb_eq" "$ROOT/test/kkb_eq.sml" > "$OUT/kkb_eq.build" 2>&1
"$MLKIT" -gc -rp -o "$OUT/mlyacc" "$ROOT/src/Tools/ml-yacc/src/ml-yacc.mlb" > "$OUT/mlyacc.build" 2>&1
cp "$ROOT/src/Parsing/Topdec.grm" "$OUT/Topdec.grm"
for example in msort kkb_eq mlyacc; do
 set --
 if [ "$example" = mlyacc ]; then set -- "$OUT/Topdec.grm"; fi
 "$OUT/$example" "$@" +RTS -rp -rp_interval 1ms -rp_region all -rp_file "$OUT/$example.rp" -RTS > "$OUT/$example.out"
 case "$example" in
  msort) cmp "$OUT/msort.out" "$ROOT/test/msort.mlb.out.ok";;
  kkb_eq) cmp "$OUT/kkb_eq.out" "$ROOT/test/kkb_eq.sml.out.ok";;
  mlyacc) test -s "$OUT/Topdec.grm.sml" && test -s "$OUT/Topdec.grm.sig";;
 esac
 "$RPVIEW" "$OUT/$example.rp" -o "$OUT/$example.html" > /dev/null
 sh "$ROOT/test/region_profile/check-ir11.sh" "$OUT/$example.rp"
done

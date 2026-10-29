#!/bin/sh
# One small shared fixture, plus independent type errors. No exhaustive sweeps.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
SMLTOJS=${SMLTOJS:-$ROOT/bin/smltojs}
export SML_LIB=${SML_LIB:-$ROOT/js}
work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-js-narrow.XXXXXX")
trap 'status=$?; if [ "$status" -ne 0 ]; then cat "$work"/*.log; fi; rm -rf "$work"; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
cp "$ROOT/test/narrow-real-portable.sml" "$work/main.sml"
"$SMLTOJS" -aopt -o "$work/main" "$work/main.sml" > "$work/build.log" 2>&1
node "$ROOT/js/test/run-node.js" "$work/main.html" > "$work/actual"
cmp "$ROOT/test/narrow-real-portable.sml.out.ok" "$work/actual"
if SMLTOJS_NO_BIGINT=1 node "$ROOT/js/test/run-node.js" "$work/main.html" > "$work/capability.log" 2>&1; then
  echo 'Narrow arithmetic unexpectedly succeeded without BigInt' >&2; exit 1
fi
grep -q 'requires BigInt' "$work/capability.log"
for name in narrow-real-equality narrow-real16-equality narrow-real-mixing; do
  cp "$ROOT/test/$name.sml" "$work/$name.sml"
  if "$SMLTOJS" -no_basislib "$work/$name.sml" > "$work/$name.log" 2>&1; then
    echo "Unexpected success: $name" >&2; exit 1
  fi
  grep -q 'Type clash' "$work/$name.log"
done
# Loading the extended Basis must not require BigInt for ordinary programs.
printf 'val () = print (Real.toString (3.0/2.0) ^ "\\n")\n' > "$work/legacy.sml"
"$SMLTOJS" -o "$work/legacy" "$work/legacy.sml" > "$work/legacy.log" 2>&1
SMLTOJS_NO_BIGINT=1 node "$ROOT/js/test/run-node.js" "$work/legacy.html" > "$work/legacy.out"
grep -q '^1.50000000000$' "$work/legacy.out"
echo 'SMLtoJs narrow real checks passed'

#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
export SML_LIB=${SML_LIB:-$ROOT/js}
work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-js-real.XXXXXX")
trap 'status=$?; if [ "$status" -ne 0 ]; then cat "$work"/*.log; fi; rm -rf "$work"; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
cp "$ROOT/js/test/real-basis.sml" "$work/main.sml"
"${SMLTOJS:-$ROOT/bin/smltojs}" -aopt -o "$work/main" "$work/main.sml" > "$work/build.log" 2>&1
node "$ROOT/js/test/run-node.js" "$work/main.html" > "$work/run.log" 2>&1
grep -qx 'JavaScript REAL checks passed' "$work/run.log"
SMLTOJS_NO_BIGINT=1 node "$ROOT/js/test/run-node.js" "$work/main.html" > "$work/legacy.log" 2>&1
cmp "$work/run.log" "$work/legacy.log"
cat "$work/run.log"

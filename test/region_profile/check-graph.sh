#!/bin/sh
# Generate the interactive graph regression page with the shared DOM harness. Open the
# resulting HTML in a browser: its visible result is PASS or an exception.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=${1:-$(mktemp -d "${TMPDIR:-/tmp}/rp-graph.XXXXXX")}
mkdir -p "$OUT"
sh "$ROOT/test/region_profile/encode-fixture.sh" "$ROOT/test/region_profile/graph-fixture.json" "$OUT/profile.rp"
"$RPVIEW" "$OUT/profile.rp" -o "$OUT/profile.html" > /dev/null
{
    printf '%s\n' '<!doctype html><meta charset="utf-8"><title>Region graph regression</title><pre id="result">RUNNING</pre><script>'
    printf '%s\n' 'const resultNode=window.document.getElementById("result");' 'try {'
    sh "$ROOT/test/region_profile/viewer-prelude.sh" "$OUT/profile.html"
    sed -n '/^<script>$/,/^<\/script>/p' "$OUT/profile.html" | sed '1d;$d;s/^const samples=/let samples=/'
    cat "$ROOT/test/region_profile/graph-assertions.js"
    printf '%s\n' 'resultNode.textContent="PASS: exact graph sums, filters, colours, units, labels, export and edge cases";' '} catch(error) { resultNode.textContent="FAIL: "+error.stack; throw error; }' '</script>'
} > "$OUT/check-graph.html"
echo "Open $OUT/check-graph.html in a browser to run the interactive graph checks."

#!/bin/sh
# Exercise generated standalone reports using the real script and DOM harness.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-time-viewer.XXXXXX")
trap 'rm -rf "$OUT"' EXIT
for profile in "$@"; do
 "${RPVIEW:-$ROOT/bin/rpview}" "$profile" -o "$OUT/report.html" >/dev/null
 sh "$ROOT/test/region_profile/viewer-prelude.sh" "$OUT/report.html" > "$OUT/test.js"
 sed -n '/^<script>$/,/^<\/script>/p' "$OUT/report.html" | sed '1d;$d' >> "$OUT/test.js"
 cat "$ROOT/test/time_profile/viewer-assertions.js" >> "$OUT/test.js"
 node "$OUT/test.js"
done

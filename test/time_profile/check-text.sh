#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-text.XXXXXX")}
PVIEW=${PVIEW:-$ROOT/bin/pview}
ENCODER=${RPENCODER:-$ROOT/bin/rpfixture}
mkdir -p "$OUT"
"$ENCODER" "$ROOT/test/time_profile/text-fixture.json" "$OUT/input.prof"
"$PVIEW" "$OUT/input.prof" --format text > "$OUT/stdout.txt"
"$PVIEW" "$OUT/input.prof" -o "$OUT/report.txt" > /dev/null
cmp "$OUT/stdout.txt" "$OUT/report.txt"
grep -q 'Samples: 4 .*complete' "$OUT/report.txt"
grep -q 'buffer 2, routing 1' "$OUT/report.txt"
grep -Eq '2 +50.00% +2.000 ms +compute42' "$OUT/report.txt"
grep -Eq '1 +25.00% +1.000 ms +GC' "$OUT/report.txt"
grep -Eq '1 +25.00% +1.000 ms +Unresolved PC' "$OUT/report.txt"
sed '/"type":"time_sample"/d; s/"recorded":4/"recorded":0/' "$ROOT/test/time_profile/text-fixture.json" > "$OUT/empty.json"
"$ENCODER" "$OUT/empty.json" "$OUT/empty.prof"
"$PVIEW" "$OUT/empty.prof" --format text > "$OUT/empty.txt"
grep -q 'No recorded time samples' "$OUT/empty.txt"
sed '/"type":"time_status"/d; /"type":"session_end"/d' "$ROOT/test/time_profile/text-fixture.json" > "$OUT/incomplete.json"
"$ENCODER" "$OUT/incomplete.json" "$OUT/incomplete.prof"
"$PVIEW" "$OUT/incomplete.prof" --format text > "$OUT/incomplete.txt"
grep -q INCOMPLETE "$OUT/incomplete.txt"
grep -q 'Loss counters unavailable' "$OUT/incomplete.txt"
"$ENCODER" "$ROOT/test/region_profile/graph-fixture.json" "$OUT/region.prof"
if "$PVIEW" "$OUT/region.prof" --format text > "$OUT/region.txt" 2> "$OUT/error"; then exit 1; fi
grep -q 'requires time profiling data' "$OUT/error"
echo 'PASS: textual time overview, exact attribution, percentages, loss, empty/incomplete and output inference'

#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/rp-records.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
# Validate syntax, uint64 ranges, committed snapshots, and accounting in SML;
# then check independent fixture expectations with the scalar-record oracle.
"$RPVIEW" "$2" --format json -o "$OUT/profile.json" > /dev/null
linux=0
[ "$(uname -s)" != Linux ] || linux=1
awk -v mode="$1" -v linux="$linux" -f "$ROOT/test/region_profile/records.awk" -f "$ROOT/test/region_profile/accounting.awk" "$OUT/profile.json"

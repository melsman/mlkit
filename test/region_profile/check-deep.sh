#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
CC=${CC:-cc}
RPVIEW=${RPVIEW:-$ROOT/bin/rpview}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-rp-deep.XXXXXX")
echo "Deep stack artifacts: $OUT"
$CC -O2 -std=gnu99 -Wall -Wextra -Werror -DPROFILING -iquote "$ROOT/src/Runtime" \
  "$ROOT/src/Runtime/RegionProfile.c" "$ROOT/src/Runtime/tests/region-profile-deep.c" -o "$OUT/deep"
"$OUT/deep" "$OUT/deep.rp"
"$RPVIEW" "$OUT/deep.rp" --format json -o "$OUT/deep.json" > /dev/null
grep -q '"frames":1000017' "$OUT/deep.json"
grep -q '"stack_bytes":8000136' "$OUT/deep.json"
if "$OUT/deep" "$OUT/cycle.rp" cycle > "$OUT/cycle.log" 2>&1; then
  echo 'Accepted a non-increasing frame chain' >&2; exit 1
fi
grep -q 'non-increasing ML frame chain' "$OUT/cycle.log"
echo 'Deep stacks: full frame count, stack accounting and cycle rejection passed'

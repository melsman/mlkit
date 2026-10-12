#!/bin/sh
# Opt-in T6c/T6d study; no performance thresholds in CI.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-measure-attribution.XXXXXX")}
export OUT
BUILD_ONLY=1 sh "$ROOT/test/time_profile/measure-ordinary.sh"
node "$ROOT/test/time_profile/measure-ordinary.js" "$OUT" "${RPVIEW:-$ROOT/bin/rpview}" "${REPEATS:-5}" --attribution

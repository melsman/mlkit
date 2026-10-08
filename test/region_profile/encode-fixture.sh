#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
ENCODER=${RPENCODER:-$ROOT/bin/rpfixture}
if [ ! -x "$ENCODER" ] || [ "$ROOT/test/region_profile/encode-fixture.sml" -nt "$ENCODER" ] || [ "$ROOT/src/Tools/RegionProfile/Binary.sml" -nt "$ENCODER" ] || [ "$ROOT/src/Tools/RegionProfile/Reader.sml" -nt "$ENCODER" ] || [ "$ROOT/src/Tools/RegionProfile/Json.sml" -nt "$ENCODER" ]; then
  # These sources are also built by the seed compiler for rpview. Keep the
  # checkout compiler's objects separate from that compiler/Basis combination.
  SML_LIB="$ROOT" "${MLKIT:-$ROOT/bin/mlkit-arm64}" --mlb-subdir RPFixture -gc -o "$ENCODER" "$ROOT/test/region_profile/encode-fixture.mlb" >&2
fi
exec "$ENCODER" "$@"

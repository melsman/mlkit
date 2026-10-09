#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
ENCODER=${RPENCODER:-$ROOT/bin/rpfixture}
if [ ! -x "$ENCODER" ]; then
  echo "Missing fixture encoder: $ENCODER. Run make rpfixture in $ROOT." >&2
  exit 1
fi
exec "$ENCODER" "$@"

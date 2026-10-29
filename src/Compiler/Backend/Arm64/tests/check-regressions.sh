#!/bin/sh
# Compatibility entry point; native suites share one isolated runner.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../../../../.." && pwd)
MLKIT=${MLKIT:-${MLKIT_ARM64:?Set MLKIT_ARM64}}
REML=${REML:-${REML_ARM64:-}}
export MLKIT REML
exec sh "$ROOT/test/check-regressions.sh" "$@"

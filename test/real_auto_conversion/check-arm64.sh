#!/bin/sh
set -eu
export MLKIT_NATIVE=${MLKIT_ARM64:-$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)/bin/mlkit}
export REAL_AUTO_ARCH=arm64
exec sh "$(dirname -- "$0")/check-native.sh"

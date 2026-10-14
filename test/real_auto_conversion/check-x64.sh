#!/bin/sh
set -eu
export MLKIT_NATIVE=${MLKIT_X64:-$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)/bin/mlkit}
export REAL_AUTO_ARCH=x86_64
exec sh "$(dirname -- "$0")/check-native.sh"

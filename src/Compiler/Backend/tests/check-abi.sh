#!/bin/sh
set -eu
cd "$(dirname "$0")"
abi_test_dir=$(mktemp -d /tmp/mlkit-abi.XXXXXX)
trap 'rm -rf "$abi_test_dir"' EXIT HUP INT TERM
# The harness substitutes small support structures for compiler modules.
# Keep its MLKit cache separate from production compilations of CallConv.
mkdir "$abi_test_dir/Arm64" "$abi_test_dir/tests"
cp ../FrameLayout.sml ../CALL_CONV.sml ../CallConv.sml "$abi_test_dir/"
cp ../Arm64/AbiArm64.sml "$abi_test_dir/Arm64/"
cp abi.mlb abi.sml CallConvSupport.sml "$abi_test_dir/tests/"
(cd "$abi_test_dir/tests" &&
  "${MLKIT:-mlkit}" -no_gc -output "$abi_test_dir/layout" abi.mlb)
"$abi_test_dir/layout"
if [ "$(uname -s)" = Darwin ] && [ "$(uname -m)" = arm64 ]; then
  gcc -arch arm64 darwin-c-abi.c darwin-c-abi.s -o "$abi_test_dir/darwin"
  "$abi_test_dir/darwin"
fi

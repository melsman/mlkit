#!/bin/sh
set -eu
cd "$(dirname "$0")"
abi_test_dir=$(mktemp -d /tmp/mlkit-abi.XXXXXX)
trap 'rm -rf "$abi_test_dir"' EXIT HUP INT TERM
mlton -output "$abi_test_dir/layout" abi.mlb
"$abi_test_dir/layout"
if [ "$(uname -s)" = Darwin ] && [ "$(uname -m)" = arm64 ]; then
  gcc -arch arm64 darwin-c-abi.c darwin-c-abi.s -o "$abi_test_dir/darwin"
  "$abi_test_dir/darwin"
fi

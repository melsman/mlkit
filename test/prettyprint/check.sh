#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/prettyprint-spans.XXXXXX")
trap 'rm -rf "$OUT"' EXIT HUP INT TERM
make -C "$ROOT" prettyprint-tools PRETTYPRINT_BUILD="$OUT"
"$OUT/spans"

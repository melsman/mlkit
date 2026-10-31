#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
MLKIT=${MLKIT:-$root/bin/mlkit}
MLLEX=${MLLEX:-$root/bin/mlkit-mllex}
SML_LIB=${SML_LIB:-$root}
export SML_LIB
work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-mllex.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
cp "$root"/test/mllex/*.lex "$root"/test/mllex/check.sml "$root"/test/mllex/check.mlb "$work/"
cd "$work"
"$MLLEX" actions.lex escaping.lex > generate.log 2>&1 || { cat generate.log; exit 1; }
"$MLKIT" -gc -no_par -j 2 -o check check.mlb > build.log 2>&1 || { cat build.log; exit 1; }
./check

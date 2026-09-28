#!/bin/sh
# Linux x86-64 regression for issue #233; MLKIT may name an uninstalled compiler.
set -eu
ulimit -c 0
root=$(CDPATH= cd -- "$(dirname -- "$0")/../../../../.." && pwd)
MLKIT=${MLKIT:-"$root/bin/mlkit"}
export SML_LIB=${SML_LIB:-"$root"}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM
cp "$root/src/Compiler/Backend/X64/tests/"c-call-alignment.* "$work/"
cp "$root/src/Compiler/Backend/X64/tests/c-call-sysdb.sml" "$work/"
cd "$work"
printf '$(SML_LIB)/basis/basis.mlb\nc-call-sysdb.sml\n' > sysdb.mlb
${CC:-cc} -Wall -Wextra -Werror -c c-call-alignment.c -o probe.o
${CC:-cc} -c c-call-alignment.S -o entry.o
for mode in gc no_gc gengc; do
    "$MLKIT" --no_basislib -"$mode" --mlb-subdir "CallAlignment$$" -ldexe "${CC:-cc} probe.o entry.o" \
        -o probe c-call-alignment.sml > "$mode.log" 2>&1 || { cat "$mode.log"; exit 1; }
    ./probe > actual
    awk 'BEGIN { for (i = 0; i < 17; i++) print "OK" }' > expected
    diff -u expected actual
    "$MLKIT" -"$mode" --mlb-subdir "CallAlignment$$" -o sysdb sysdb.mlb > "sysdb-$mode.log" 2>&1 || {
        cat "sysdb-$mode.log"; exit 1;
    }
    printf 'OK\n' > expected
    for lookup in group user; do
        ./sysdb "$lookup" > actual
        diff -u expected actual
    done
    echo "C call alignment and SysDB ($mode): OK"
done

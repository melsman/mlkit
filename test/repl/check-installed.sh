#!/bin/sh
# Exercise the packaged default-GC Basis from a fresh user directory.
set -eu
: "${MLKIT:?Set MLKIT to the installed compiler executable}"
: "${SML_LIB:?Set SML_LIB to the installed library directory}"
installed_repl_work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-installed-repl.XXXXXX")
echo "Installed REPL outputs: $installed_repl_work"
trap 'status=$?; if [ "$status" -ne 0 ]; then
  for log in "$installed_repl_work/output.log" "$installed_repl_work/error.log"; do
    [ ! -f "$log" ] || cat "$log" >&2
  done
fi; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
cd "$installed_repl_work"
printf 'val answer = 10 + 32;\nval pretty = List.tabulate (3, fn i => i + 1);\n:quit;\n' |
  "$MLKIT" > output.log 2> error.log
if grep -Ei 'Failed to|Garbage collection disabled|uncaught|error:|Type clash' output.log error.log; then
  exit 1
fi
grep -Fq 'val answer = 42 : int' output.log
grep -Fq 'val pretty = [1,2,3] : int list' output.log
printf 'Installed default-GC REPL: OK\n'

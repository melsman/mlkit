#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
export SML_LIB=${SML_LIB:-$ROOT}
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-rts-args.XXXXXX")
echo "Runtime argument checks: $OUT"
cat > "$OUT/main.sml" <<'SML'
val () = List.app (fn s => print ("[" ^ s ^ "]\n")) (CommandLine.arguments ())
SML
printf '%s\n' '$(SML_LIB)/basis/basis.mlb' main.sml > "$OUT/main.mlb"
"$MLKIT" -no_gc -o "$OUT/plain" "$OUT/main.mlb" > "$OUT/plain.build" 2>&1
"$MLKIT" -no_gc -rp -o "$OUT/profile" "$OUT/main.mlb" > "$OUT/profile.build" 2>&1
check () {
  expected=$1; shift
  printf '%s\n' "$expected" > "$OUT/expected"
  printf 'Runtime argument case:'
  printf ' <%s>' "$@"
  printf '\n'
  "$@" > "$OUT/actual"
  cmp "$OUT/expected" "$OUT/actual"
}
check '[-help]
[-rp]
[]
[two words]' "$OUT/plain" -help -rp '' 'two words'
check '[before]
[middle]
[after]' "$OUT/plain" before +RTS -command_pipe first -RTS middle +RTS -reply_pipe second -RTS after
check '[--]
[+RTS]
[-help]' "$OUT/plain" -- +RTS -help
check '[+RTS]
[-help]' "$OUT/plain" +RTS --RTS +RTS -help
check '[--]
[-rp]' "$OUT/plain" +RTS -- -rp
check '[one]' "$OUT/plain" one +RTS -repl_logfile log
check '[input]
[-rp]' "$OUT/profile" input +RTS -rp -rp_interval 0 -rp_file "$OUT/test.rp" -RTS -rp
[ -s "$OUT/test.rp" ]
"$OUT/plain" +RTS -help > "$OUT/plain.help" 2>&1
"$OUT/profile" +RTS -help > "$OUT/profile.help" 2>&1
if grep -Fq -- '[-rp' "$OUT/plain.help"; then exit 1; fi
grep -q -- '-rp_region' "$OUT/profile.help"
for option in -unknown -rp_alloc_depth; do
 if "$OUT/profile" +RTS "$option" > "$OUT/error" 2>&1; then exit 1; fi
 grep -q 'unknown .*option' "$OUT/error"
done
for delimiter in +RTS -RTS --RTS --; do
 if "$OUT/profile" +RTS -rp_file "$delimiter" > "$OUT/error" 2>&1; then exit 1; fi
 grep -q 'Missing argument' "$OUT/error"
done
if "$OUT/profile" +RTS -rp_file > "$OUT/error" 2>&1; then exit 1; fi
if "$OUT/plain" +RTS -rp > "$OUT/error" 2>&1; then exit 1; fi
grep -q 'compiled with -rp' "$OUT/error"
echo 'Runtime argument blocks passed'

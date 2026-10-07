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
[after]' "$OUT/profile" before +RTS -rp -rp_interval 0 -RTS middle +RTS -rp_file "$OUT/multiple.rp" -RTS after
check '[--]
[+RTS]
[-help]' "$OUT/plain" -- +RTS -help
check '[+RTS]
[-help]' "$OUT/plain" +RTS --RTS +RTS -help
check '[--]
[-rp]' "$OUT/plain" +RTS -- -rp
check '[one]' "$OUT/profile" one +RTS -rp -rp_interval 0 -rp_file "$OUT/implicit-end.rp"
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

# REPL-only options are neither advertised nor accepted by ordinary programs.
for executable in plain profile; do
  for option in -command_pipe -reply_pipe -repl_logfile; do
    ! grep -q -- "$option" "$OUT/$executable.help"
    if "$OUT/$executable" +RTS "$option" unused > "$OUT/error" 2>&1; then exit 1; fi
    grep -q 'unknown runtime option' "$OUT/error"
  done
  if nm "$OUT/$executable" | grep -q 'repl_interp'; then
    echo 'Ordinary executable linked the REPL interpreter' >&2; exit 1
  fi
done
check '[-command_pipe]
[application-path]' "$OUT/plain" -command_pipe application-path
# Ordinary programs still terminate on an uncaught exception.
printf '%s\n' 'val () = raise Fail "ordinary failure"' > "$OUT/failure.sml"
"$MLKIT" -no_gc -o "$OUT/failure" "$OUT/failure.sml" > "$OUT/failure.build" 2>&1
if "$OUT/failure" > "$OUT/failure.out" 2>&1; then exit 1; fi
grep -q 'uncaught exception' "$OUT/failure.out"
# A real REPL must keep working after an uncaught exception.
cat > "$OUT/input" <<'SML'
(raise Fail "expected REPL failure") : unit;
print "REPL recovered\n";
:quit;
SML
(cd "$OUT" && "$MLKIT" -no_gc < input > repl.out 2>&1)
grep -q 'REPL recovered' "$OUT/repl.out"
grep -q 'uncaught exception' "$OUT/repl.out"
set -- "$OUT"/MLB/*/runtime.exe
[ "$#" -eq 1 ]
(cd "$OUT" && "$1" +RTS -help) > "$OUT/repl.help" 2>&1
for option in -command_pipe -reply_pipe -repl_logfile; do
  grep -q -- "$option PATH" "$OUT/repl.help"
  if (cd "$OUT" && "$1" +RTS "$option") > "$OUT/error" 2>&1; then exit 1; fi
  grep -q 'Missing' "$OUT/error"
  for value in '' +RTS -RTS --RTS --; do
    if (cd "$OUT" && "$1" +RTS "$option" "$value") > "$OUT/error" 2>&1; then exit 1; fi
    grep -q 'Missing' "$OUT/error"
  done
done
echo 'REPL options are confined to REPL executables'

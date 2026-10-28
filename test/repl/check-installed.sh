#!/bin/sh
# Exercise packaged runtime modes, generators, and the default-GC REPL.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
: "${MLKIT:?Set MLKIT to the installed compiler executable}"
: "${SML_LIB:?Set SML_LIB to the installed library directory}"
installed_repl_work=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-installed-repl.XXXXXX")
echo "Installed REPL outputs: $installed_repl_work"
trap 'status=$?; if [ "$status" -ne 0 ]; then
  for log in "$installed_repl_work/output.log" "$installed_repl_work/error.log" "$installed_repl_work/build.log"; do
    [ ! -f "$log" ] || cat "$log" >&2
  done
fi; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
cd "$installed_repl_work"
tools=$(dirname "$MLKIT")
cat > probe.sml <<'SML'
val answer = List.foldl op+ 0 (List.tabulate (7, fn i => i + 3))
val () = if answer = 42 then print "installed compiler passed\n"
         else raise Fail "incorrect result"
SML
printf 'installed compiler passed\n' > expected
# Exercise the six prepared Basis configurations without rebuilding libraries.
for mode in gc par gcprof parprof reml remlprof; do
  compiler=$MLKIT
  case "$mode" in
    gc) flags=-gc ;;
    par) flags=-no_gc ;;
    gcprof) flags='-gc -rp' ;;
    parprof) flags='-no_gc -rp' ;;
    reml) compiler=$tools/reml; flags= ;;
    remlprof) compiler=$tools/reml; flags=-rp ;;
  esac
  "$compiler" $flags -o probe probe.sml > build.log 2>&1
  case "$mode" in
    *prof)
      ./probe +RTS -rp -rp_interval 0 -p_file profile.prof > actual
      "$tools/pview" profile.prof -o profile.html > /dev/null ;;
    *) ./probe > actual ;;
  esac
  cmp expected actual
  echo "Installed runtime mode: $mode passed"
done
# Exercise both native generators and compile their output with the installed
# compiler and parser library, independent of the source checkout's caches.
cp "$ROOT/src/Tools/ml-yacc/examples/calc/calc.lex" \
   "$ROOT/src/Tools/ml-yacc/examples/calc/calc.grm" "$installed_repl_work/"
# The parser library ships as source; compile its installed sources in the
# client's writable directory, leaving the installation read-only.
mkdir parser-lib
cp "$SML_LIB/ml-yacc-lib/"*.sml "$SML_LIB/ml-yacc-lib/"*.sig \
   "$SML_LIB/ml-yacc-lib/"*.mlb parser-lib/
cat > "$installed_repl_work/calc.mlb" <<'EOF'
$(SML_LIB)/basis/basis.mlb
parser-lib/ml-yacc-lib.mlb
calc.grm.sig
calc.grm.sml
calc.lex.sml
driver.sml
EOF
cat > "$installed_repl_work/driver.sml" <<'EOF'
structure Values = CalcLrValsFun(structure Token = LrParser.Token)
structure Lexer = CalcLexFun(structure Tokens = Values.Tokens)
structure Parser = Join(structure LrParser = LrParser
                        structure ParserData = Values.ParserData
                        structure Lex = Lexer)
val input = ref "2+3*4;"
val lexer = Parser.makeLexer (fn _ => !input before input := "")
val (result,_) = Parser.parse(0,lexer,fn (s,_,_) => raise Fail s,())
val () = if result = SOME 14 then print "native generators passed\n"
         else raise Fail "native calculator result"
EOF
(cd "$installed_repl_work" && "$tools/mlkit-mllex" calc.lex &&
  "$tools/mlkit-mlyacc" calc.grm &&
  "$tools/mlkit" -gc -o calc calc.mlb &&
  ./calc > generators.out)
printf 'native generators passed\n' > "$installed_repl_work/generators.expected"
cmp "$installed_repl_work/generators.expected" "$installed_repl_work/generators.out"
printf 'val answer = 10 + 32;\nval pretty = List.tabulate (3, fn i => i + 1);\n:quit;\n' |
  "$MLKIT" > output.log 2> error.log
if grep -Ei 'Failed to|Garbage collection disabled|uncaught|error:|Type clash' output.log error.log; then
  exit 1
fi
grep -Fq 'val answer = 42 : int' output.log
grep -Fq 'val pretty = [1,2,3] : int list' output.log
printf 'Installed default-GC REPL: OK\n'

#!/bin/sh
# A separate, relocatable native prefix. SML_LIB selects it when invoking tools.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${ARM64_NATIVE_BIN:?Set ARM64_NATIVE_BIN}"
: "${ARM64_PREFIX:?Set ARM64_PREFIX}"
case "$ARM64_PREFIX" in
  *[[:space:]]*) echo 'Use a stable, space-free symlink as ARM64_PREFIX (compiler dependency paths cannot contain whitespace).' >&2; exit 1 ;;
esac
native_tools='mlkit reml kittester rp2ps mlkit-mllex mlkit-mlyacc'
for tool in $native_tools; do
  [ "$(lipo -archs "$ARM64_NATIVE_BIN/$tool")" = arm64 ]
done
for archive in "$SML_LIB"/lib/darwin-arm64/runtimeSystem*.a; do
  [ "$(lipo -archs "$archive")" = arm64 ]
done
# Source timestamps alone do not invalidate code emitted by an older backend.
if ! cmp -s "$ARM64_NATIVE_BIN/mlkit" "$ARM64_PREFIX/bin/mlkit" ||
   [ "$(cat "$ARM64_PREFIX/.arm64-install-prefix" 2>/dev/null || :)" != "$ARM64_PREFIX" ]; then
  for dir in basis basis/io kitlib ml-yacc-lib; do
    for cache in "$ARM64_PREFIX/$dir"/MLB/ARM64_*; do
      [ ! -d "$cache" ] || rm -rf "$cache"
    done
  done
fi
mkdir -p "$ARM64_PREFIX/bin" "$ARM64_PREFIX/lib/darwin-arm64" "$ARM64_PREFIX/basis/io"
for tool in $native_tools; do cp "$ARM64_NATIVE_BIN/$tool" "$ARM64_PREFIX/bin/$tool"; done
cp "$SML_LIB"/lib/darwin-arm64/runtimeSystem*.a "$ARM64_PREFIX/lib/darwin-arm64/"
for dir in basis basis/io kitlib ml-yacc-lib; do
  mkdir -p "$ARM64_PREFIX/$dir"
  for source in "$SML_LIB/$dir/"*.sml "$SML_LIB/$dir/"*.sig "$SML_LIB/$dir/"*.mlb; do
    if [ -f "$source" ] && ! cmp -s "$source" "$ARM64_PREFIX/$dir/$(basename "$source")"; then
      cp -p "$source" "$ARM64_PREFIX/$dir/"
    fi
  done
done
# Build caches at their installed location using the installed native compiler.
# This also checks that no source-tree or X64 runtime path leaks into linking.
scratch=$(mktemp -d /tmp/mlkit-install-check.XXXXXX)
trap 'rm -rf "$scratch"' EXIT
printf 'val () = print "native installation passed\\n"\n' > "$scratch/probe.sml"
printf '$(SML_LIB)/basis/basis.mlb\nprobe.sml\n' > "$scratch/probe.mlb"
printf 'native installation passed\n' > "$scratch/expected"
for flags in '-no_gc' '-gc' '-gengc' '-gc -prof' '-no_gc -prof' '-no_gc -par'; do
  printf 'Checking installed configuration: %s\n' "$flags"
  (cd "$ARM64_PREFIX/basis" && SML_LIB="$ARM64_PREFIX" \
    "$ARM64_PREFIX/bin/mlkit" $flags -c basis.mlb)
  (cd "$scratch" && SML_LIB="$ARM64_PREFIX" "$ARM64_PREFIX/bin/mlkit" \
    $flags -o probe probe.mlb)
  [ "$(lipo -archs "$scratch/probe")" = arm64 ]
  case "$flags" in
    *-prof*)
      (cd "$scratch" && ./probe -notimer 1 > actual &&
        "$ARM64_PREFIX/bin/rp2ps" -source profile.rp -name installation-check -region region.ps -stack stack.ps)
      for graph in region stack; do
        (cd "$scratch" && "$ARM64_PREFIX/bin/rp2ps" -source profile.rp -name installation-check "-$graph" single.ps)
        # Ignore the creation time, which is also printed inside the graph.
        for file in "$graph.ps" single.ps; do
          sed '/^%%CreationDate:/d; /^([A-Z][a-z][a-z] [A-Z][a-z][a-z] /d' \
            "$scratch/$file" > "$scratch/$file.normalized"
        done
        grep -q '^%!PS' "$scratch/$graph.ps"
        cmp "$scratch/$graph.ps.normalized" "$scratch/single.ps.normalized"
      done ;;
    *) (cd "$scratch" && ./probe > actual) ;;
  esac
  cmp "$scratch/expected" "$scratch/actual"
done
# Exercise both native generators and compile their output with the installed
# compiler and parser library, independent of the source checkout's caches.
cp "$SML_LIB/src/Tools/ml-yacc/examples/calc/calc.lex" \
   "$SML_LIB/src/Tools/ml-yacc/examples/calc/calc.grm" "$scratch/"
cat > "$scratch/calc.mlb" <<'EOF'
$(SML_LIB)/basis/basis.mlb
$(SML_LIB)/ml-yacc-lib/ml-yacc-lib.mlb
calc.grm.sig
calc.grm.sml
calc.lex.sml
driver.sml
EOF
cat > "$scratch/driver.sml" <<'EOF'
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
(cd "$scratch" && "$ARM64_PREFIX/bin/mlkit-mllex" calc.lex &&
  "$ARM64_PREFIX/bin/mlkit-mlyacc" calc.grm &&
  SML_LIB="$ARM64_PREFIX" "$ARM64_PREFIX/bin/mlkit" -gc -o calc calc.mlb &&
  ./calc > generators.out)
printf 'native generators passed\n' > "$scratch/generators.expected"
cmp "$scratch/generators.expected" "$scratch/generators.out"
printf '%s\n' "$ARM64_PREFIX" > "$ARM64_PREFIX/.arm64-install-prefix"
printf 'Native installation staged at %s\n' "$ARM64_PREFIX"

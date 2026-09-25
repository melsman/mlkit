#!/bin/bash
# Validate the shipped archive, independently of the development installation.
set -euo pipefail
archive=${1:?Specify the ARM64 release archive}
root=$(pwd)
package_name=mlkit-bin-dist-darwin
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-package.XXXXXX")
echo "Package check outputs: $scratch"

# Reject the former flat archive before extracting or installing anything.
tar -tzf "$archive" > "$scratch/contents.txt"
awk -v root="$package_name" '
  $0 != root && index($0, root "/") != 1 { bad = 1 }
  END { exit (NR == 0 || bad) }
' "$scratch/contents.txt" || {
  echo "Expected one top-level $package_name/ directory" >&2
  exit 1
}
tar -xzf "$archive" -C "$scratch"
package=$scratch/$package_name
test -f "$package/Makefile"
test -s "$package/lib/mlkit/basis/MLB/ARM64_FD2_RI_GC/repl.sml.d"

installed=$scratch/installed
make -C "$package" install PREFIX="$installed"
for tool in mlkit reml smltojs kittester mlkit-mllex mlkit-mlyacc rp2ps; do
  test "$(lipo -archs "$installed/bin/$tool")" = arm64
done
for archive in "$installed/lib/mlkit/lib/darwin-arm64/"runtimeSystem*.a; do
  test "$(lipo -archs "$archive")" = arm64
done
export SML_LIB=$installed/lib/mlkit
export MLKIT=$installed/bin/mlkit
"$MLKIT" --version
"$installed/bin/reml" --version
SML_LIB="$installed/lib/smltojs" "$installed/bin/smltojs" --version

# A normal installation may be read-only to its users. It must not need to
# recreate the Basis caches that were tested before packaging.
chmod -R a-w "$installed/lib"
mkdir "$scratch/user"
cd "$scratch/user"
cat > probe.sml <<'SML'
val answer = List.foldl op+ 0 (List.tabulate (7, fn i => i + 3))
val () = if answer = 42 then print "packaged compiler passed\n"
         else raise Fail "incorrect result"
SML
printf 'packaged compiler passed\n' > expected
for mode in gc no_gc; do
  "$MLKIT" "-$mode" -o "probe-$mode" probe.sml
  test "$(lipo -archs "probe-$mode")" = arm64
  "./probe-$mode" > actual
  cmp expected actual
done
SML_LIB="$installed/lib/smltojs" "$installed/bin/smltojs" -o probe-js probe.sml
test -s probe-js.html
sh "$root/test/repl/check-installed.sh"
echo "ARM64 release archive passed"

#!/bin/sh
# Exercise configured seed commands and the bootstrap dependency chain without
# spending CI time rebuilding the compiler a second time.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-build-check.XXXXXX")
cleanup () {
  status=$?
  if [ "$status" -eq 0 ]; then rm -rf "$OUT"
  else
    echo "Build-check artifacts retained at $OUT" >&2
    find "$OUT" -name '*.log' -exec tail -n 20 {} \; >&2
  fi
}
trap cleanup EXIT
trap 'exit 1' HUP INT TERM
export BUILD_CHECK_LOG="$OUT/events"
cat > "$OUT/seed" <<'SH'
#!/bin/sh
set -eu
if [ "${1:-}" = --version ]; then echo 'test compiler'; exit; fi
# Stand in for the generated HTML embedding tool.
if [ "${1:-}" = viewer.html ]; then echo '(* embedded HTML *)' > "$2"; exit; fi
printf '%s|%s|%s\n' "$0" "${SML_LIB:-unset}" "$*" >> "$BUILD_CHECK_LOG"
while [ "$#" -gt 0 ]; do
  case "$1" in -output|-o) shift; output=$1 ;; esac
  shift
done
echo "building $output"
echo "compiler diagnostic" >&2
case "$output" in *"${FAIL_STAGE:-never-match}"*) exit 37 ;; esac
cp "$0" "$output"
chmod +x "$output"
SH
chmod +x "$OUT/seed"
ln -s seed "$OUT/seed-mlkit"
ln -s seed "$OUT/seed-mlton"
for seed in mlkit mlton; do
  dir=$OUT/$seed
  mkdir "$dir"
  (cd "$dir" && "$ROOT/configure" --srcdir="$ROOT" \
    --with-compiler="$OUT/seed-$seed -test-argument" > configure.log 2>&1)
  ln -s "$ROOT/Makefiledefault" "$dir/Makefiledefault"
  mkdir "$dir/bin"
  cat > "$dir/probe.mk" <<'MAKE'
include Makefile
seed-probe:
	$(MLCOMP) -output bin/probe dummy.mlb
MAKE
  (cd "$dir" && SML_LIB=must-not-leak make -s -f probe.mk seed-probe)
  grep -q "seed-$seed|unset|-test-argument" "$BUILD_CHECK_LOG"
  case "$(uname -s):$seed" in
    Darwin:mlkit) grep -q -- '-ldexe gcc -arch arm64' "$BUILD_CHECK_LOG" ;;
    Darwin:mlton) grep -q -- '-link-opt -Wl,-stack_size' "$BUILD_CHECK_LOG" ;;
  esac
  # The viewer and its embedding helper must use the same configured seed.
  cp "$ROOT"/src/Tools/RegionProfile/*.sml "$ROOT"/src/Tools/RegionProfile/*.mlb \
    "$ROOT/src/Tools/RegionProfile/viewer.html" "$dir/src/Tools/RegionProfile/"
  mkdir -p "$dir/test/region_profile" "$dir/src/Kitlib"
  cp "$ROOT"/test/region_profile/encode-fixture.sml "$ROOT"/test/region_profile/encode-fixture.mlb "$dir/test/region_profile/"
  cp "$ROOT/src/Kitlib/BINARYMAP.sig" "$ROOT/src/Kitlib/Binarymap.sml" "$dir/src/Kitlib/"
  (cd "$dir" && SML_LIB=must-not-leak make -s rpview rpfixture > rpview.log 2>&1)
  grep "seed-$seed|unset|" "$BUILD_CHECK_LOG" | grep -q -- '-output embed embed.mlb'
  grep "seed-$seed|unset|" "$BUILD_CHECK_LOG" | grep -q -- '-output .*/bin/rpview rpview.mlb'
  grep "seed-$seed|unset|" "$BUILD_CHECK_LOG" | grep -q -- '-output .*/bin/rpfixture encode-fixture.mlb'
  mkdir -p "$dir/test/prettyprint" "$dir/src/Common"
  cp "$ROOT"/test/prettyprint/*.sml "$ROOT"/test/prettyprint/*.mlb "$dir/test/prettyprint/"
  cp "$ROOT/src/Common/PRETTYPRINT.sig" "$ROOT/src/Common/PrettyPrint.sml" \
    "$ROOT/src/Common/MD5.sml" "$ROOT/src/Common/IRLocations.sml" "$dir/src/Common/"
  (cd "$dir" && SML_LIB=must-not-leak make -s prettyprint-tools > prettyprint.log 2>&1)
  for helper in spans locations ir-report link-map; do
    grep "seed-$seed|unset|" "$BUILD_CHECK_LOG" | grep -q -- "/$helper $helper.mlb"
  done
  if [ "$(uname -s)" = Darwin ]; then
    (cd "$dir" && SML_LIB=must-not-leak make -s emitter > emitter.log 2>&1)
    grep "seed-$seed|unset|" "$BUILD_CHECK_LOG" | grep -q -- '-output bin/arm64-emitter-test'
  fi
  # Both seed selections use the same backend/stage chain. The architecture
  # check and strip are bypassed only because the stand-in outputs are scripts.
  for verbose in 0 1; do
    (cd "$dir" && TMPDIR="$dir" make -s -j6 bootstrap VERBOSE=$verbose \
      BOOTSTRAP_COMPILER="$dir/bin/probe" bootstrap_check=true BOOTSTRAP_STRIP=true > success.log 2>&1)
    grep -q 'stage1/mlkit.*-j 2.*Stage2' "$BUILD_CHECK_LOG"
    grep -q 'stage2/mlkit.*-j 2.*Stage3' "$BUILD_CHECK_LOG"
    output=$(sed -n 's/^Bootstrap outputs: //p' "$dir/success.log")
    for stage in 1 2 3; do
      grep -q 'building ' "$output/stage$stage.log"
      grep -q 'compiler diagnostic' "$output/stage$stage.log"
    done
    if [ "$verbose" = 1 ]; then
      grep -q 'building ' "$dir/success.log"
      grep -q 'compiler diagnostic' "$dir/success.log"
    else
      if grep -Eq 'building |compiler diagnostic' "$dir/success.log"; then
        echo 'Quiet bootstrap leaked compiler output' >&2; exit 1
      fi
    fi
    printf 'previous compiler must survive a failed bootstrap\n' > "$dir/bin/mlkit"
    cp "$dir/bin/mlkit" "$dir/success"
    # In particular, tee must not hide a failed compiler in verbose mode.
    for failure in compile compare; do
      if [ "$failure" = compile ]; then fail_stage=stage2; compare=cmp
      else fail_stage=never-match; compare=false
      fi
      if (cd "$dir" && TMPDIR="$dir" FAIL_STAGE=$fail_stage make -s bootstrap VERBOSE=$verbose \
        BOOTSTRAP_COMPILER="$dir/bin/probe" bootstrap_check=true \
        BOOTSTRAP_STRIP=true BOOTSTRAP_COMPARE=$compare > failure.log 2>&1); then
        echo "Accepted bootstrap $failure failure (VERBOSE=$verbose)" >&2; exit 1
      fi
      cmp "$dir/success" "$dir/bin/mlkit"
    done
  done
done
# An explicit seed library must override an application's inherited SML_LIB.
(cd "$OUT/mlkit" && "$ROOT/configure" --srcdir="$ROOT" \
  --with-compiler="$OUT/seed-mlkit" --with-compiler-lib="$OUT/seed-library" > configure.log 2>&1 &&
  SML_LIB=must-not-leak make -s -f probe.mk seed-probe)
grep -q "seed-mlkit|$OUT/seed-library|" "$BUILD_CHECK_LOG"
echo 'Configured seeds and Makefile bootstrap ordering/failure handling passed'

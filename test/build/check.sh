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
printf '%s|%s|%s\n' "$0" "${SML_LIB:-unset}" "$*" >> "$BUILD_CHECK_LOG"
while [ "$#" -gt 0 ]; do
  case "$1" in -output|-o) shift; output=$1 ;; esac
  shift
done
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
  # Both seed selections use the same backend/stage chain. The architecture
  # check and strip are bypassed only because the stand-in outputs are scripts.
  (cd "$dir" && TMPDIR="$dir" make -s -j6 bootstrap \
    BOOTSTRAP_COMPILER="$dir/bin/probe" bootstrap_check=true BOOTSTRAP_STRIP=true)
  grep -q 'stage1/mlkit.*Stage2' "$BUILD_CHECK_LOG"
  grep -q 'stage2/mlkit.*Stage3' "$BUILD_CHECK_LOG"
  printf 'previous compiler must survive a failed bootstrap\n' > "$dir/bin/mlkit"
  cp "$dir/bin/mlkit" "$dir/success"
  # Failure of compilation or comparison must not publish a new compiler.
  for failure in compile compare; do
    if [ "$failure" = compile ]; then fail_stage=stage2; compare=cmp
    else fail_stage=never-match; compare=false
    fi
    if (cd "$dir" && TMPDIR="$dir" FAIL_STAGE=$fail_stage make -s bootstrap \
      BOOTSTRAP_COMPILER="$dir/bin/probe" bootstrap_check=true \
      BOOTSTRAP_STRIP=true BOOTSTRAP_COMPARE=$compare > failure.log 2>&1); then
      echo "Accepted bootstrap $failure failure" >&2; exit 1
    fi
    cmp "$dir/success" "$dir/bin/mlkit"
  done
done
# An explicit seed library must override an application's inherited SML_LIB.
(cd "$OUT/mlkit" && "$ROOT/configure" --srcdir="$ROOT" \
  --with-compiler="$OUT/seed-mlkit" --with-compiler-lib="$OUT/seed-library" > configure.log 2>&1 &&
  SML_LIB=must-not-leak make -s -f probe.mk seed-probe)
grep -q "seed-mlkit|$OUT/seed-library|" "$BUILD_CHECK_LOG"
echo 'Configured seeds and Makefile bootstrap ordering/failure handling passed'

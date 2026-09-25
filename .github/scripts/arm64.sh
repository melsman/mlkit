#!/bin/bash
# CI phases are separate so failures and logs identify the failing operation.
set -euo pipefail
: "${ARM64_HOST:?Set ARM64_HOST to mlkit or mlton}"
: "${ARM64_CI_ROOT:?Set ARM64_CI_ROOT to a space-free output directory}"
case "$ARM64_HOST" in mlkit|mlton) ;; *) exit 1 ;; esac
case "$(uname -s)/$(uname -m)" in Darwin/arm64) ;; *) exit 1 ;; esac
root=$(pwd)
export SML_LIB=$root
export TMPDIR=$ARM64_CI_ROOT/tmp
mkdir -p "$TMPDIR" "$ARM64_CI_ROOT/logs"
seed=$ARM64_CI_ROOT/seed
native=$ARM64_CI_ROOT/bin
prefix=$ARM64_CI_ROOT/install
cache=CI_${ARM64_HOST}
tests=src/Compiler/Backend/Arm64/tests

case "${1:?Specify a CI phase}" in
  runtime)
    sh autobuild
    DARWIN_NATIVE=1 ./configure CC=/usr/bin/gcc --with-compiler=mlkit
    make -j3 runtime
    # A clean native job must not depend on checkout-built X64 compatibility archives.
    test ! -e lib/runtimeSystemGC.a
    test ! -d lib/darwin-x86_64
    for archive in lib/darwin-arm64/runtimeSystem*.a; do
      test "$(lipo -archs "$archive")" = arm64
    done
    ;;
  seed)
    make -f Makefile.arm64 configuration
    mkdir -p "$seed"
    bootstrap_env=(env -u SML_LIB)
    if [ -n "${MLKIT_BOOTSTRAP_SML_LIB:-}" ]; then
      bootstrap_env=(env "SML_LIB=$MLKIT_BOOTSTRAP_SML_LIB")
    fi
    for compiler in mlkit reml; do
      if [ "$ARM64_HOST" = mlkit ]; then
        # Large compiler units exhaust the former 256 MiB host stack. Match
        # the 1 GiB/classic-linker configuration used for local compiler builds.
        # Use the release's Basis/cache/runtime without adding a cache suffix:
        # an installed seed may be read-only and cannot build new Basis caches.
        "${bootstrap_env[@]}" mlkit -gc \
          -ldexe 'gcc -arch x86_64 -Wl,-ld_classic,-stack_size,0x40000000' \
          -o "$seed/$compiler" "src/Compiler/${compiler}arm64.mlb"
        test "$(lipo -archs "$seed/$compiler")" = x86_64
        stack_size=$(otool -l "$seed/$compiler" | awk '$1 == "stacksize" {print $2}')
        echo "$compiler seed stack size: $stack_size bytes"
        test "$stack_size" = 1073741824
      else
        # Use MLton's own Basis, not the source tree's MLKit-specific Basis.
        env -u SML_LIB mlton @MLton ram-slop 0.7 -- \
          -drop-pass deepFlatten -drop-pass refFlatten -verbose 2 \
          -output "$seed/$compiler" "src/Compiler/${compiler}arm64.mlb"
        test "$(lipo -archs "$seed/$compiler")" = arm64
      fi
      "$seed/$compiler" --version
    done
    ;;
  native)
    make mlkit \
      ARM64_COMPILER="$seed/mlkit" ARM64_CACHE="$cache" \
      ARM64_NATIVE_BIN="$native" ARM64_BUILD="$ARM64_CI_ROOT/rp2ps"
    "$native/mlkit" -gc --mlb-subdir "$cache" \
      -o "$native/emitter" "$tests/emitter.mlb"
    for tool in mlkit reml kittester mlkit-mllex mlkit-mlyacc rp2ps emitter; do
      test "$(lipo -archs "$native/$tool")" = arm64
    done
    ;;
  check)
    export MLKIT_ARM64=$native/mlkit REML_ARM64=$native/reml
    export ARM64_EMITTER=$native/emitter KITTESTER=$native/kittester
    export ARM64_KEEP_TEST_OUTPUTS=1
    sh "$tests/check-native.sh"
    sh "$tests/check-parallel.sh"
    sh "$tests/check-regressions.sh"
    ;;
  bootstrap)
    BOOTSTRAP_COMPILER="$native/mlkit" sh "$tests/check-bootstrap.sh"
    ;;
  install)
    ARM64_NATIVE_BIN="$native" ARM64_PREFIX="$prefix" sh "$tests/install-native.sh"
    ;;
  package)
    make all \
      ARM64_COMPILER="$seed/mlkit" ARM64_CACHE="$cache" \
      ARM64_NATIVE_BIN="$native" ARM64_BUILD="$ARM64_CI_ROOT/rp2ps"
    make mlkit_bin_dist
    bash .github/scripts/check-arm64-package.sh "$root/dist/mlkit-bin-dist-darwin.tgz"
    ;;
  *) echo "Unknown CI phase: $1" >&2; exit 1 ;;
esac

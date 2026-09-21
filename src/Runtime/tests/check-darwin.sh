#!/bin/sh
# Run from any directory. Requires an Apple Silicon Mac and Rosetta 2.
set -eu
cd "$(dirname "$0")/../../.."
[ "$(uname -s)" = Darwin ] && [ "$(uname -m)" = arm64 ] || {
  echo 'Run on an Apple Silicon Mac with Rosetta 2.' >&2; exit 1;
}
logs=$(mktemp -d /tmp/mlkit-runtime-check.XXXXXX)
echo "Validation logs: $logs"
run() {
  name=$1; shift
  if ! "$@" > "$logs/$name.log" 2>&1; then
    cat "$logs/$name.log" >&2; exit 1
  fi
}
run autobuild sh autobuild
for native in 0 1; do
  if [ "$native" = 0 ]; then arch=x86_64; else arch=arm64; fi
  run "configure-$arch" env DARWIN_NATIVE=$native ./configure CC=gcc
  run "build-$arch" make runtime -j4
  count=0
  for archive in lib/darwin-$arch/runtimeSystem*.a; do
    [ "$(lipo -archs "$archive")" = "$arch" ]
    case "$archive" in *runtimeSystemArPar.a) ;; *) count=$((count + 1));; esac
  done
  [ "$count" = 10 ]
  for object in src/Runtime/build/darwin-$arch/*/*.o; do
    [ "$(lipo -archs "$object")" = "$arch" ]
  done
  run "link-$arch" gcc -arch "$arch" -std=gnu99 -iquote src/Runtime \
    src/Runtime/tests/allocation.c lib/darwin-$arch/runtimeSystem.a \
    -Wl,-dead_strip -lm -o "$logs/smoke-$arch"
  run "smoke-$arch" "$logs/smoke-$arch"
  run "link-profiling-$arch" gcc -arch "$arch" -std=gnu99 -DPROFILING -iquote src/Runtime \
    src/Runtime/tests/profiling-stream.c lib/darwin-$arch/runtimeSystemProf.a \
    -Wl,-dead_strip -lm -o "$logs/profiling-$arch"
  (cd "$logs" && run "profiling-$arch" "$logs/profiling-$arch")
  run "link-par-$arch" gcc -arch "$arch" -std=gnu99 -DPARALLEL -iquote src/Runtime \
    src/Runtime/tests/parallel-allocation.c lib/darwin-$arch/runtimeSystemPar.a \
    -Wl,-dead_strip -lm -pthread -o "$logs/parallel-$arch"
  run "parallel-$arch" "$logs/parallel-$arch"
  run "link-publication-$arch" gcc -arch "$arch" -O2 -std=gnu99 -DPARALLEL -iquote src/Runtime \
    src/Runtime/tests/parallel-publication.c lib/darwin-$arch/runtimeSystemPar.a \
    -Wl,-dead_strip -lm -pthread -o "$logs/publication-$arch"
  run "publication-$arch" "$logs/publication-$arch"
  run "install-$arch" make install_runtime LIBDIR="$logs/stage"
  for archive in lib/darwin-$arch/runtimeSystem*.a; do
    cmp "$archive" "$logs/stage/lib/darwin-$arch/$(basename "$archive")"
  done
  if [ "$native" = 0 ]; then
    shasum lib/runtimeSystem*.a > "$logs/legacy.sha"
  else
    shasum -c "$logs/legacy.sha" > "$logs/legacy-check.log"
    if make mlkit > "$logs/native-compiler.log" 2>&1; then
      echo 'Native compiler build should have been rejected' >&2; exit 1
    fi
    if make -C src/Runtime DARWIN_NATIVE=0 > "$logs/make-override.log" 2>&1; then
      echo 'Make target override should have been rejected' >&2; exit 1
    fi
    if gcc -arch x86_64 -iquote src/Runtime -include Target.h \
      -c src/Runtime/Layout.c -o "$logs/wrong.o" > "$logs/wrong-target.log" 2>&1; then
      echo 'Mismatched compiler target should have been rejected' >&2; exit 1
    fi
  fi
done
if DARWIN_NATIVE=2 ./configure CC=gcc > "$logs/invalid-mode.log" 2>&1; then
  echo 'Invalid DARWIN_NATIVE should have been rejected' >&2; exit 1
fi
run restore env DARWIN_NATIVE=0 ./configure CC=gcc
run rebuild make runtime -j4
for archive in lib/darwin-x86_64/runtimeSystem*.a; do
  cmp "$archive" "lib/$(basename "$archive")"
done
printf 'Runtime matrix passed: 10 variants per architecture, allocation, installation, and target guards.\n'

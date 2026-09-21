#!/bin/sh
# Optional Argobots: point ARGOBOTS_ROOT at a configured ARM64 source build.
set -eu
: "${MLKIT_ARM64:?Set MLKIT_ARM64}"
: "${REML_ARM64:?Set REML_ARM64}"
: "${SML_LIB:?Set SML_LIB}"
case "$(uname -s)/$(uname -m)" in Darwin/arm64) ;; *) exit 1;; esac
cd "$(dirname "$0")"
fixtures=$PWD
scratch=$(mktemp -d /tmp/mlkit-parallel-arm64.XXXXXX)
echo "Parallel validation logs: $scratch"
run() {
  run_log=$1; shift
  if ! "$@" > "$scratch/$run_log.log" 2>&1; then
    cat "$scratch/$run_log.log" >&2; exit 1
  fi
}
# Reject options that have no matching runtime before compiling source.
printf 'val x = 1\n' > "$scratch/guard.sml"
for options in '-argo' '-par0' '-par -prof' '-par --tag_values' '-par -gc' '-par -gengc'; do
  if "$MLKIT_ARM64" --no_basislib $options -o "$scratch/rejected" \
       "$scratch/guard.sml" > "$scratch/guard.log" 2>&1; then
    echo "Unsupported options accepted: $options" >&2; exit 1
  fi
  grep -q 'ARM64' "$scratch/guard.log"
  [ ! -e "$scratch/rejected" ]
done
for mode in pthread argobots; do
  flags=""; includes=""; libraries=""; runtime=runtimeSystemPar.a
  if [ "$mode" = argobots ]; then
    if [ -z "${ARGOBOTS_ROOT:-}" ]; then
      echo 'Argobots skipped (set ARGOBOTS_ROOT to test it).'; continue
    fi
    flags=-argo
    includes="-DARGOBOTS -I$ARGOBOTS_ROOT/src/include"
    libraries="$ARGOBOTS_ROOT/src/.libs/libabt.a"
    runtime=runtimeSystemArPar.a
  fi
  for test in parallel-allocation parallel-publication; do
    # Argobots paths must not contain spaces (the runtime build has this limit).
    run "$mode-$test-link" gcc -arch arm64 -O2 -Wall -Wextra -Werror \
      -DPARALLEL $includes -iquote "$SML_LIB/src/Runtime" \
      "$SML_LIB/src/Runtime/tests/$test.c" "$SML_LIB/lib/darwin-arm64/$runtime" \
      $libraries -Wl,-dead_strip -lm -pthread -o "$scratch/$mode-$test"
    for streams in 1 4; do
      run "$mode-$test-$streams" "$scratch/$mode-$test" -p "$streams"
    done
  done
  for compiler in "$MLKIT_ARM64" "$REML_ARM64"; do
    for policy in inferred always private; do
      name="$mode-$(basename "$compiler")-$policy"
      mkdir "$scratch/$name"
      cd "$scratch/$name"
      cp "$fixtures"/parallel*.sml .
      cp "$SML_LIB/basis/THREAD.sig" "$SML_LIB/basis/Thread.sml" .
      sample=parallel; extra=""; callback=callback.o
      case "$policy" in
        always) extra=--alloc_protect_always ;;
        private) extra=-par0; sample=parallel-private; callback="" ;;
      esac
      printf 'parallel-prelude.sml\nTHREAD.sig\nThread.sml\n%s.sml\n' "$sample" > main.mlb
      run "$name-callback" gcc -arch arm64 -DPARALLEL $includes \
        -iquote "$SML_LIB/src/Runtime" -c "$fixtures/parallel-callback.c" -o callback.o
      run "$name-compile" "$compiler" --no_basislib --no_delete_target_files \
        -par $flags $extra -ldexe "gcc -arch arm64 $callback $libraries" -o program main.mlb
      printf 'parallel ML passed\n' > expected
      [ "$policy" = private ] || printf 'parallel callback passed\n' >> expected
      for streams in 1 4; do
        run "$name-$streams" ./program -p "$streams"
        cmp expected "$scratch/$name-$streams.log"
      done
      if [ "$policy" = private ]; then
        grep -q 'bl _alloc_unprotected' MLB/ARM64_*/*link*.s
      else
        grep -q 'bl _alloc$' MLB/ARM64_*/*link*.s
        grep -q 'bl _thread_init' MLB/ARM64_*/*.s
        grep -q 'bl _thread_exit' MLB/ARM64_*/*.s
        grep -q 'ldaxr ' MLB/ARM64_*/parallel.sml.s
        grep -q 'stlxr ' MLB/ARM64_*/parallel.sml.s
      fi
    done
  done
done
printf 'ARM64 parallel validation passed.\n'

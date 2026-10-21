#!/bin/sh
# Optional Argobots: point ARGOBOTS_ROOT at a configured ARM64 source build.
set -eu
: "${MLKIT_ARM64:?Set MLKIT_ARM64}"
: "${REML_ARM64:?Set REML_ARM64}"
: "${SML_LIB:?Set SML_LIB}"
: "${ARM64_EMITTER:?Set ARM64_EMITTER}"
case "$(uname -s)/$(uname -m)" in Darwin/arm64) ;; *) exit 1;; esac
cd "$(dirname "$0")"
fixtures=$PWD
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-parallel-arm64.XXXXXX")
echo "Parallel validation logs: $scratch"
run() {
  run_log=$1; shift
  if ! "$@" > "$scratch/$run_log.log" 2>&1; then
    cat "$scratch/$run_log.log" >&2; exit 1
  fi
}
cd "$scratch"
run emitter "$ARM64_EMITTER"
# Check the allocation helper itself: exception counters also use exclusives.
awk '/^_mlkit_arm64_allocate_preserving:/{p=1} p{print} p && /clrex/{exit}' \
  allocation-concurrent-link.s > allocation-helper.s
grep -q 'sub sp, sp, #48' allocation-helper.s
grep -q 'ldaxr x0, \[x16, #0\]' allocation-helper.s
grep -q 'stlxr w2, x1, \[x16, #0\]' allocation-helper.s
grep -q 'clrex' allocation-helper.s
run allocation-probes-link gcc -arch arm64 -O2 -Wall -Wextra -Werror -DPARALLEL \
  -iquote "$SML_LIB/src/Runtime" "$fixtures/allocation.c" \
  allocation-parallel.s allocation-parallel-link.s \
  "$SML_LIB/lib/darwin-arm64/runtimeSystemPar.a" -lm -pthread -o allocation-probes
run allocation-probes ./allocation-probes
# Deliberately lose every reservation to exercise argument/register restoration
# deterministically, independently of scheduler contention.
sed '/stlxr /i\
\	clrex
' allocation-parallel-link.s > allocation-failure-link.s
run allocation-failure-link gcc -arch arm64 -O2 -Wall -Wextra -Werror -DPARALLEL \
  -iquote "$SML_LIB/src/Runtime" "$fixtures/allocation.c" \
  allocation-parallel.s allocation-failure-link.s \
  "$SML_LIB/lib/darwin-arm64/runtimeSystemPar.a" -lm -pthread -o allocation-failure
run allocation-failure ./allocation-failure
# Reject options that have no matching runtime before compiling source.
printf 'val x = 1\n' > "$scratch/guard.sml"
for options in '-argo' '-par0' '-par --tag_values' '-par -gc' '-par -gengc'; do
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
  cd "$scratch"
  run "$mode-generated-link" gcc -arch arm64 -O2 -Wall -Wextra -Werror \
    -DPARALLEL $includes -iquote "$SML_LIB/src/Runtime" \
    "$fixtures/allocation-concurrent.c" "$fixtures/allocation-bridge.s" \
    allocation-concurrent-link.s "$SML_LIB/lib/darwin-arm64/$runtime" \
    $libraries -lm -pthread -o "$mode-generated"
  # Execution-stream counts are an Argobots runtime option, not a pthread option.
  stream_counts=1
  [ "$mode" != argobots ] || stream_counts="1 4"
  for scenario in single shared mixed expansion private; do
    for streams in $stream_counts; do
      set --
      [ "$mode" != argobots ] || set -- +RTS -par_threads "$streams" -RTS
      run "$mode-generated-$scenario-$streams" env ALLOCATION_SCENARIO="$scenario" \
        "./$mode-generated" "$@"
      grep -qx 'generated/C allocation passed' "$scratch/$mode-generated-$scenario-$streams.log"
    done
  done
  for test in parallel-allocation parallel-publication; do
    # Argobots paths must not contain spaces (the runtime build has this limit).
    run "$mode-$test-link" gcc -arch arm64 -O2 -Wall -Wextra -Werror \
      -DPARALLEL $includes -iquote "$SML_LIB/src/Runtime" \
      "$SML_LIB/src/Runtime/tests/$test.c" "$SML_LIB/lib/darwin-arm64/$runtime" \
      $libraries -Wl,-dead_strip -lm -pthread -o "$scratch/$mode-$test"
    for streams in $stream_counts; do
      set --
      [ "$mode" != argobots ] || set -- +RTS -par_threads "$streams" -RTS
      run "$mode-$test-$streams" "$scratch/$mode-$test" "$@"
    done
  done
  for compiler in "$MLKIT_ARM64" "$REML_ARM64"; do
    default_gc=-no_gc
    [ "$compiler" != "$REML_ARM64" ] || default_gc=""
    for policy in inferred always private profile; do
      name="$mode-$(basename "$compiler")-$policy"
      mkdir "$scratch/$name"
      cd "$scratch/$name"
      cp "$fixtures"/parallel*.sml .
      cp "$SML_LIB/basis/THREAD.sig" "$SML_LIB/basis/Thread.sml" .
      sample=parallel; extra=""; callback=callback.o
      case "$policy" in
        always) extra=--alloc_protect_always ;;
        private) extra=-par0; sample=parallel-private; callback="" ;;
        profile) extra=-rp ;;
      esac
      printf 'parallel-prelude.sml\nTHREAD.sig\nThread.sml\n%s.sml\n' "$sample" > main.mlb
      run "$name-callback" gcc -arch arm64 -DPARALLEL $includes \
        -iquote "$SML_LIB/src/Runtime" -c "$fixtures/parallel-callback.c" -o callback.o
      run "$name-compile" "$compiler" $default_gc --no_basislib --no_delete_target_files \
        -par $flags $extra -ldexe "gcc -arch arm64 $callback $libraries" -o program main.mlb
      printf 'parallel ML passed\n' > expected
      [ "$policy" = private ] || printf 'parallel callback passed\n' >> expected
      for streams in $stream_counts; do
        set --
        [ "$mode" != argobots ] || set -- +RTS -par_threads "$streams" -RTS
        run "$name-$streams" ./program "$@"
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
      if [ "$policy" = profile ]; then
        if grep -q 'clrex' MLB/ARM64_*/*link*.s; then
          echo 'Profiling unexpectedly entered the atomic assembly helper.' >&2; exit 1
        fi
      fi
    done
  done
done
printf 'ARM64 parallel validation passed.\n'

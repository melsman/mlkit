#!/bin/sh
# Run the existing suites in fresh directories without disturbing developer outputs.
set -eu
: "${SML_LIB:?Set SML_LIB to the source checkout}"
: "${MLKIT:?Set MLKIT}"
: "${KITTESTER:?Set KITTESTER to the native test driver}"
# Prefer suite concurrency within the CPU budget; REGRESSION_JOBS can cap it.
REGRESSION_CPUS=${REGRESSION_CPUS:-$(getconf _NPROCESSORS_ONLN)}
REGRESSION_JOBS=${REGRESSION_JOBS:-$REGRESSION_CPUS}
TEST_JOBS=${TEST_JOBS:-$REGRESSION_CPUS}
positive_integer () {
  case "$2" in
    ''|*[!0-9]*|0*) echo "$1 must be a positive integer" >&2; exit 1 ;;
  esac
}
positive_integer REGRESSION_CPUS "$REGRESSION_CPUS"
positive_integer REGRESSION_JOBS "$REGRESSION_JOBS"
positive_integer TEST_JOBS "$TEST_JOBS"
if [ "$REGRESSION_JOBS" -gt "$REGRESSION_CPUS" ]; then
  REGRESSION_JOBS=$REGRESSION_CPUS
fi
VERBOSE=${VERBOSE:-0}
# Run independent kittester suites before the exclusive development/REPL checks.
REGRESSION_SUITES=${REGRESSION_SUITES:-'plain gc gengc prof gcprof par explicit parallel dev repl replgc repltagged replgengc'}
printf 'Regression CPUs: %s; maximum concurrent suites: %s\n' "$REGRESSION_CPUS" "$REGRESSION_JOBS"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-regressions.XXXXXX")
echo "Regression outputs: $scratch"
export SML_LIB
run_suite () (
  suite=$1
  suite_jobs=$2
  # test_dev parallelizes make targets; do not also multiply compiler workers.
  SUITE_COMPILER_JOBS=$suite_jobs
  if [ "$suite" = dev ]; then
    SUITE_COMPILER_JOBS=1
    if [ "$suite_jobs" -gt "$TEST_JOBS" ]; then suite_jobs=$TEST_JOBS; fi
  fi
  export SUITE_COMPILER_JOBS
  root=$scratch/$suite
  mkdir -p "$root"
  (cd "$SML_LIB" && tar -cf - --exclude=MLB --exclude=.test-build --exclude='*.exe' \
    --exclude='*.res' --exclude='*.mlbres' --exclude='*.log' --exclude='*.out' \
    --exclude='*.mlbexe' --exclude='*.mlbout' \
    --exclude='*.o' --exclude='*.rp' --exclude='*.ps' --exclude=run \
    --exclude=runexe --exclude=TESTmessages --exclude=test_report.html test test_dev) |
    (cd "$root" && tar -xf -)
  for dir in basis lib src kitlib ml-yacc-lib; do ln -s "$SML_LIB/$dir" "$root/$dir"; done
  REGRESSION_COMPILER=$MLKIT
  # Test sources are private, but shared libraries should reuse the caches
  # prepared by mlkit_basislibs. A unique suffix rebuilds the entire Basis in
  # every runtime configuration. Set a suffix explicitly for cold-cache checks.
  REGRESSION_CACHE=${REGRESSION_CACHE:-}
  REGRESSION_LINKER=
  export REGRESSION_COMPILER REGRESSION_CACHE REGRESSION_LINKER
  cat > "$root/compiler" <<'EOF'
#!/bin/sh
set -- -j "$SUITE_COMPILER_JOBS" "$@"
if [ -n "$REGRESSION_CACHE" ]; then
  set -- --mlb-subdir "$REGRESSION_CACHE" "$@"
fi
if [ -n "$REGRESSION_LINKER" ]; then
  exec "$REGRESSION_COMPILER" -ldexe "$REGRESSION_LINKER" "$@"
fi
exec "$REGRESSION_COMPILER" "$@"
EOF
  chmod +x "$root/compiler"
  printf 'Running %s (%s CPUs; compiler -j %s)\n' "$suite" "$suite_jobs" "$SUITE_COMPILER_JOBS"
  case "$suite" in
    dev)
      (cd "$root/test_dev" && make -j "$suite_jobs" test test_prof MLKIT="$root/compiler" VERBOSE="$VERBOSE") ;;
    explicit)
      REGRESSION_COMPILER=${REML:?Set REML for explicit regions}
      export REGRESSION_COMPILER
      # Direct diagnostic logs assume Basis signatures have already been cached.
      (cd "$root" && "$root/compiler" --maximum_inline_size 0 -c basis/basis.mlb)
      (cd "$root/test/explicit_regions" && "$KITTESTER" "$root/compiler" --logdirect all.tst --maximum_inline_size 0) ;;
    parallel)
      (cd "$root/test/parallelism" && "$KITTESTER" "$root/compiler" --logdirect all.tst -no_gc -par) ;;
    region-info)
      REGRESSION_COMPILER=${REML:?Set REML for region information}
      export REGRESSION_COMPILER
      (cd "$root/test/region_info" && "$KITTESTER" "$root/compiler" --logdirect all2.tst) ;;
    argobots)
      : "${ARGOBOTS_ROOT:?Set ARGOBOTS_ROOT to a configured source build}"
      REGRESSION_LINKER="${CC:-cc} $ARGOBOTS_ROOT/src/.libs/libabt.a"
      export REGRESSION_LINKER
      (cd "$root/test/parallelism" && "$KITTESTER" "$root/compiler" --logdirect all.tst \
        -no_gc -par -argo) ;;
    repl|replgc|repltagged|replgengc)
      case "$suite" in
        repl) flags=-no_gc ;;
        replgc) flags=-gc ;;
        repltagged) flags='-gc -tag_pairs' ;;
        replgengc) flags=-gengc ;;
      esac
      (cd "$root/test/repl" && make test MLKIT="$root/compiler" MLKIT_FLAGS="$flags") ;;
    *)
      case "$suite" in
        plain) flags='-no_gc -no_par' ;; gc) flags=-gc ;; gengc) flags=-gengc ;;
        prof) flags='-no_gc -no_par -rp' ;; gcprof) flags='-gc -rp' ;;
        par) flags='-no_gc -par' ;;
        *) echo "Unknown suite: $suite" >&2; exit 1 ;;
      esac
      (cd "$root/test" && make prepare && "$KITTESTER" "$root/compiler" all.tst $flags) ;;
  esac < /dev/null > "$scratch/$suite.log" 2>&1 || {
    tail -60 "$scratch/$suite.log" >&2
    # Runtime failures are redirected by kittester; include their actual output
    # in the CI log rather than leaving only a runexe failure marker.
    for actual in "$root"/test/*.out; do
      expected=$actual.ok
      if [ -f "$expected" ] && ! cmp -s "$actual" "$expected"; then
        printf '\nUnexpected test output: %s\n' "$actual" >&2
        tail -30 "$actual" >&2
      fi
    done
    exit 1;
  }
  echo "$suite passed"
)

# Keep the existing batches and cache barriers. Reserve one CPU per suite
# first; distribute only the remaining CPUs to its compiler subprocesses.
batch=
pending_modes=
workers=0
wait_batch () {
  [ "$workers" -gt 0 ] || return 0
  suite_jobs=$((REGRESSION_CPUS / workers))
  extra=$((REGRESSION_CPUS % workers))
  pending=
  for item in $batch; do
    jobs=$suite_jobs
    if [ "$extra" -gt 0 ]; then jobs=$((jobs + 1)); extra=$((extra - 1)); fi
    run_suite "$item" "$jobs" &
    pending="$pending $!"
  done
  failed=0
  for child in $pending; do
    wait "$child" || failed=1
  done
  batch=
  pending_modes=
  workers=0
  return "$failed"
}
for suite in $REGRESSION_SUITES; do
  case "$suite" in
    plain|gc|gengc|prof|gcprof|par|explicit|parallel|region-info)
      case "$suite" in
        parallel) cache_mode=par ;;
        explicit|region-info) cache_mode=reml ;;
        *) cache_mode=$suite ;;
      esac
      # Parallelism uses the full par suite's caches. Both ReML suites may
      # update the same Basis caches, even though their sources are private.
      case " $pending_modes " in
        *" $cache_mode "*) wait_batch ;;
      esac
      batch="$batch $suite"
      pending_modes="$pending_modes $cache_mode"
      workers=$((workers + 1))
      if [ "$workers" -ge "$REGRESSION_JOBS" ]; then wait_batch; fi ;;
    *)
      # Development and REPL checks can use several cache modes, so keep
      # them outside concurrent batches.
      wait_batch
      run_suite "$suite" "$REGRESSION_CPUS" ;;
  esac
done
wait_batch
echo 'Regression suites passed.'

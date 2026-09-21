#!/bin/sh
# Run the existing suites in fresh directories without disturbing developer outputs.
set -eu
: "${SML_LIB:?Set SML_LIB to the source checkout}"
: "${MLKIT_ARM64:?Set MLKIT_ARM64}"
: "${KITTESTER:?Set KITTESTER to the native test driver}"
REGRESSION_SUITES=${REGRESSION_SUITES:-'dev plain gc gengc prof gcprof par explicit parallel repl replgc repltagged replgengc'}
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-regressions.XXXXXX")
echo "Regression outputs: $scratch"
export SML_LIB
for suite in $REGRESSION_SUITES; do
  root=$scratch/$suite
  mkdir -p "$root"
  (cd "$SML_LIB" && tar -cf - --exclude=MLB --exclude='*.exe' \
    --exclude='*.res' --exclude='*.mlbres' --exclude='*.log' --exclude='*.out' \
    --exclude='*.o' --exclude='*.rp' --exclude='*.ps' --exclude=run \
    --exclude=runexe --exclude=TESTmessages --exclude=test_report.html test test_dev) |
    (cd "$root" && tar -xf -)
  for dir in basis lib src kitlib ml-yacc-lib; do ln -s "$SML_LIB/$dir" "$root/$dir"; done
  REGRESSION_COMPILER=$MLKIT_ARM64
  REGRESSION_CACHE=$(basename "$scratch" | tr -cd '[:alnum:]')
  REGRESSION_LINKER=
  export REGRESSION_COMPILER REGRESSION_CACHE REGRESSION_LINKER
  cat > "$root/compiler" <<'EOF'
#!/bin/sh
if [ -n "$REGRESSION_LINKER" ]; then
  exec "$REGRESSION_COMPILER" --mlb-subdir "$REGRESSION_CACHE" -ldexe "$REGRESSION_LINKER" "$@"
fi
exec "$REGRESSION_COMPILER" --mlb-subdir "$REGRESSION_CACHE" "$@"
EOF
  chmod +x "$root/compiler"
  echo "Running $suite"
  case "$suite" in
    dev) (cd "$root/test_dev" && make test MLKIT="$root/compiler") ;;
    explicit)
      REGRESSION_COMPILER=${REML_ARM64:?Set REML_ARM64 for explicit regions}
      export REGRESSION_COMPILER
      # Direct diagnostic logs assume Basis signatures have already been cached.
      (cd "$root" && "$root/compiler" --maximum_inline_size 0 -c basis/basis.mlb)
      (cd "$root/test/explicit_regions" && "$KITTESTER" "$root/compiler" --logdirect all.tst --maximum_inline_size 0) ;;
    parallel)
      (cd "$root/test/parallelism" && "$KITTESTER" "$root/compiler" --logdirect all.tst -no_gc -par) ;;
    argobots)
      : "${ARGOBOTS_ROOT:?Set ARGOBOTS_ROOT to a configured ARM64 source build}"
      REGRESSION_LINKER="gcc -arch arm64 $ARGOBOTS_ROOT/src/.libs/libabt.a"
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
        plain) flags=-no_gc ;; gc) flags=-gc ;; gengc) flags=-gengc ;;
        prof) flags='-no_gc -prof' ;; gcprof) flags='-gc -prof' ;;
        par) flags='-no_gc -par' ;;
        *) echo "Unknown suite: $suite" >&2; exit 1 ;;
      esac
      (cd "$root/test" && make prepare && "$KITTESTER" "$root/compiler" all.tst $flags) ;;
  esac < /dev/null > "$scratch/$suite.log" 2>&1 || {
    tail -60 "$scratch/$suite.log" >&2; exit 1;
  }
  echo "$suite passed"
done
echo 'Regression suites passed.'

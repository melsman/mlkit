#!/bin/sh
set -eu
: "${MLKIT_ARM64:?Set MLKIT_ARM64 to the MLKit ARM64 compiler executable}"
: "${REML_ARM64:?Set REML_ARM64 to the ReML ARM64 compiler executable}"
: "${SML_LIB:?Set SML_LIB to the configured MLKit source/install directory}"
case "$(uname -s)/$(uname -m)" in
  Darwin/arm64) ;;
  *) echo 'Native execution checks require Apple Silicon' >&2; exit 1 ;;
esac
cd "$(dirname "$0")"
arm_test_dir=$(mktemp -d /tmp/mlkit-native-arm64.XXXXXX)
trap 'status=$?; if [ "$status" -ne 0 ]; then
  for log in "$arm_test_dir"/*.log; do [ ! -f "$log" ] || cat "$log"; done
fi; rm -rf "$arm_test_dir"; exit "$status"' EXIT
trap 'exit 1' HUP INT TERM
cp *.sml native.mlb probe.c scalar-calls.c callback.s foreign.c repl-input.txt "$arm_test_dir/"
cd "$arm_test_dir"
# Generate a finite 33,600-byte record without checking in a huge fixture.
awk 'BEGIN {
  print "fun large__noinline(w:word):word = let val t = (w";
  for(i=1;i<4200;i++) printf ",0w%d",i;
  print ") in prim(\"arm64_large\",(t,w)) end";
  print "val w:word = prim(\"getchar\",())";
  print "val _:unit = prim(\"putchar\",large__noinline w)";
  print "val _:unit = prim(\"putchar\",0w10)";
}' > large.sml
gcc -arch arm64 -Wall -Wextra -Werror -c probe.c -o probe.o
printf 'A\n' > expected
check_compiler () {
  compiler=$1
  prefix=$2
  "$compiler" --no_basislib --no_delete_target_files -o "$prefix-native" native.mlb > "$prefix.log" 2>&1
  printf '@' | "./$prefix-native" > actual
  cmp expected actual
  file "$prefix-native" | grep -q 'Mach-O 64-bit executable arm64'
  "$compiler" --no_basislib --no_delete_target_files -o "$prefix-calls" calls.sml >> "$prefix.log" 2>&1
  printf '@' | "./$prefix-calls" > actual
  cmp expected actual
  "$compiler" --no_basislib --no_delete_target_files -o "$prefix-branch" branch.sml >> "$prefix.log" 2>&1
  printf '@' | "./$prefix-branch" > actual
  cmp expected actual
  printf 'B\n' > other
  printf 'A' | "./$prefix-branch" > actual
  cmp other actual
  for sample in stack closure float exn unwind overflow list spills nan immediates large; do
    "$compiler" --no_basislib --no_delete_target_files -ldexe 'gcc -arch arm64 probe.o' \
      -o "$prefix-$sample" "$sample.sml" >> "$prefix.log" 2>&1
    printf '@@' | "./$prefix-$sample" > actual
    cmp expected actual
  done
}
check_compiler "$MLKIT_ARM64" mlkit
# Reuse the source tree to exercise ReML's separate cache variant.
check_compiler "$REML_ARM64" reml
"$REML_ARM64" --no_basislib -o reml-regions regions.sml > regions.log 2>&1
printf '@' | ./reml-regions > actual
cmp expected actual
# Internal multi-result calls, including odd and even result padding.
"${ARM64_EMITTER:?Set ARM64_EMITTER to the host emitter-test executable}" > emitter.log 2>&1
for n in 4 5 6 7; do
  for suffix in "" -shrink; do
    gcc -arch arm64 "results$n$suffix.s" "results$n$suffix-link.s" "$SML_LIB/lib/darwin-arm64/runtimeSystem.a" -o "results$n"
    "./results$n" > actual
    printf 'ABCDEFG' | cut -c "1-$n" | tr -d '\n' > returned
    cmp returned actual
  done
done
# Ensure the noinline sample really exercised nested ML calls and a tail branch.
grep -q 'bl _F.step__noinline' MLB/ARM64_*/calls.sml.s
grep -q 'b _F.step__noinline' MLB/ARM64_*/calls.sml.s
grep -Eq 'br x17|blr x17' MLB/ARM64_*/closure.sml.s
grep -q 'fadd ' MLB/ARM64_*/float.sml.s
grep -q 'sub sp, sp, #4080' MLB/ARM64_*/large.sml.s
# Every generated object is ARM64, and X64 cache paths were not populated.
find MLB -name '*.o' -exec file {} \; > objects
if grep -v 'Mach-O 64-bit object arm64' objects; then exit 1; fi
[ ! -d MLB/RI ]
# Parallel runtime code generation belongs to milestone 6.
if "$MLKIT_ARM64" --no_basislib -par -o unsupported native.mlb > unsupported.log 2>&1; then
  echo 'ARM parallelism was incorrectly accepted' >&2; exit 1
fi
grep -q 'ARM64 backend does not support parallelism' unsupported.log
[ ! -e unsupported ]
gcc -arch arm64 -O2 -Wall -Wextra -Werror scalar-calls.c scalar-calls.s -o scalar-calls
./scalar-calls
gcc -arch arm64 -c callback.s -o callback.o
gcc -arch arm64 -O2 -Wall -Wextra -Werror -c foreign.c -o foreign.o
printf 'OK\nOK\nOK\nOK\nOK\nOK\n' > foreign-expected
printf 'OK\n' > gc-expected
for compiler in "$MLKIT_ARM64" "$REML_ARM64"; do
  extra_gc=-extra_gc_checks
  [ "$compiler" != "$REML_ARM64" ] || extra_gc=""
  for flags in '' '--tag_values' '-prof' '-gc' '-gc -tag_pairs' '-gengc' '-gc -prof' '-gc -tag_pairs -prof' '-gengc -prof'; do
    if [ "$compiler" = "$REML_ARM64" ]; then
      case "$flags" in ''|'-prof') ;; *) continue;; esac
    fi
    "$compiler" --no_basislib $flags $extra_gc -o gc-roots gc-roots.sml >> integration.log 2>&1
    case "$flags" in *-prof*) profile_flags="-notimer 17 -file integration.rp";; *) profile_flags="";; esac
    case "$flags" in *gc*) report_flags="-report_gc";; *) report_flags="";; esac
    ./gc-roots $profile_flags $report_flags > actual 2> gc-report.log
    cmp gc-expected actual
    if [ -n "$report_flags" ]; then grep -Eq "[1-9][0-9]* collections" gc-report.log; fi
    "$compiler" --no_basislib $flags $extra_gc -o gc-frames gc-frames.sml >> integration.log 2>&1
    ./gc-frames $profile_flags > actual
    cmp gc-expected actual
    "$compiler" --no_basislib $flags $extra_gc -ldexe 'gcc -arch arm64 callback.o foreign.o' \
      -o foreign foreign.sml >> integration.log 2>&1
    ./foreign $profile_flags > actual
    cmp foreign-expected actual
  done
  for flags in '' '-gc' '-gc -tag_pairs' '-gengc'; do
    [ "$compiler" != "$REML_ARM64" ] || [ -z "$flags" ] || continue
    "$compiler" --no_basislib $flags $extra_gc < repl-input.txt > repl.log 2>&1
    grep -q 'REPL GC OK' repl.log
    grep -q 'uncaught exception Overflow' repl.log
    grep -q 'REPL RECOVERED' repl.log
    if grep -Eq 'BAD|Compile error|ARM64 GC metadata|Garbage collection disabled' repl.log; then
      cat repl.log; exit 1
    fi
  done
done
gcc -arch arm64 -O2 -Wall -Wextra -Werror -iquote "$SML_LIB/src/Runtime" \
  "$SML_LIB/src/Runtime/Arm64GC.c" "$SML_LIB/src/Runtime/tests/arm64-gc-metadata.c" -o gc-metadata
./gc-metadata
printf 'Native ARM64 MLKit/ReML calls, closures, regions, exceptions, floats, spills, large frames, and target guards passed\n'

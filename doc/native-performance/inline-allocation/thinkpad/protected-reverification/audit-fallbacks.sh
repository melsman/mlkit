#!/bin/sh
# Untimed check: compare runtime-fallback counts in the short fixture.
set -eu
: "${M8_ROOT:?Set M8_ROOT}"
: "${REVERIFY_BENCHMARK:?Set REVERIFY_BENCHMARK to the fresh benchmark directory}"
cd "$REVERIFY_BENCHMARK"
awk '
  /extern uint64_t mlkit_audit_helper_calls/ {print; print "extern uint64_t mlkit_audit_fallback_calls;";next}
  /fprintf\(stderr,"Protected helper calls:/ {print "  fprintf(stderr,\"Runtime fallback calls: %\" PRIu64 \"\\n\",mlkit_audit_fallback_calls);"}
  {print}
' audit.c > audit-fallback.c
cc -O2 -Wall -Wextra -Werror -DPARALLEL -DEXPECTED_CALLS=100000000 -DEXPECTED_REGIONS=1000 \
  -iquote "$M8_ROOT/fast/src/Runtime" -c audit-fallback.c -o short/audit-fallback.o
for variant in baseline current; do
  awk '
    /^__allocate:$/ {print;print "\tincq mlkit_audit_fallback_calls(%rip)";next}
    {print}
    END {print ".comm mlkit_audit_fallback_calls,8,8"}
  ' "short/$variant-audit.s" > "short/$variant-fallback-audit.s"
  cc -c "short/$variant-fallback-audit.s" -o "short/$variant-fallback-audit.o"
  cc -Wl,--wrap=allocateRegion -o "short/$variant-fallback-audit" \
    "short/MLB/RI_PAR_Verify$variant/main.sml.o" "short/$variant-fallback-audit.o" \
    short/audit-fallback.o "$M8_ROOT/fast/lib/runtimeSystemPar.a" -lm -ldl -pthread
  taskset -c 0 "short/$variant-fallback-audit" > actual 2> "short/$variant-fallback-audit.log"
  grep -qx ok actual
  cat "short/$variant-fallback-audit.log"
done

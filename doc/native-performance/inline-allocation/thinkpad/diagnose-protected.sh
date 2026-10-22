#!/bin/sh
# Manual diagnostic variants; only valid for the bounded protected test fixture.
set -eu
: "${SML_LIB:?Set SML_LIB}"
: "${FORCED_BENCHMARK:?Set FORCED_BENCHMARK to the paired benchmark directory}"
scratch=$(mktemp -d "${TMPDIR:-/tmp}/mlkit-protected-diagnosis.XXXXXX")
echo "Protected diagnosis outputs: $scratch"
cd "$scratch"
cp "$FORCED_BENCHMARK/MLB/RI_PAR_Localfast/main.sml.o" main.o
cp "$FORCED_BENCHMARK/MLB/RI_PAR_Localfast/base-link_objects.o" original-link.o
cp "$FORCED_BENCHMARK/baseline" baseline
cp "$FORCED_BENCHMARK/fast" fast
# Remove the helper's redundant size guard; every call in this fixture is 2 words.
awk '
  /^allocinreg_protected:$/ {protected=1}
  protected && /cmpq \$1022,%r10/ {getline; next}
  {print}
' "$FORCED_BENCHMARK/MLB/RI_PAR_Localfast/base-link_objects.s" > unchecked-link.s
cc -c unchecked-link.s -o unchecked-link.o
cc -o unchecked main.o unchecked-link.o "$SML_LIB/lib/runtimeSystemPar.a" -lm -ldl -pthread
# Call the old stack-saving protected helper from the same new allocation site.
awk '
  /jne L_alloc_protect_/ {label=$2}
  {if ($0 == label ":") {print ".globl allocinreg_protected"; print "allocinreg_protected:"} print}
' "$FORCED_BENCHMARK/MLB/RI_PAR_Localbaseline/base-link_objects.s" > stack-link.s
cc -c stack-link.s -o stack-link.o
cc -o stack main.o stack-link.o "$SML_LIB/lib/runtimeSystemPar.a" -lm -ldl -pthread
# Diagnostic lower bound: remove dispatch/local path at this known-protected site.
awk '
  /cmpq \$0,32\(%r11\)/ {skip=1; print "\tmovq $2,%r10"; print "\tcall allocinreg_protected@PLT"; next}
  skip && /^L_local_alloc_done_/ {skip=0}
  !skip {print}
' "$FORCED_BENCHMARK/MLB/RI_PAR_Localfast/main.sml.s" > direct-main.s
cc -c direct-main.s -o direct-main.o
cc -o direct direct-main.o original-link.o "$SML_LIB/lib/runtimeSystemPar.a" -lm -ldl -pthread
for variant in baseline fast unchecked stack direct; do
  "./$variant" > actual
  grep -qx ok actual
done
printf 'run,variant,real,user,sys\n' > samples.csv
for run in 1 2 3 4 5 6 7 8 9; do
  order='baseline fast unchecked stack direct'
  [ $((run % 2)) -ne 0 ] || order='direct stack unchecked fast baseline'
  for variant in $order; do
    /usr/bin/time -p "./$variant" > actual 2> timing
    grep -qx ok actual
    awk -v r="$run" -v v="$variant" '
      $1=="real" {real=$2} $1=="user" {user=$2} $1=="sys" {sys=$2}
      END {printf "%s,%s,%s,%s,%s\n",r,v,real,user,sys}
    ' timing >> samples.csv
  done
done
cat samples.csv

#!/bin/sh
# Detailed markers must preserve register/stack arguments, converted results and policy.
set -eu
case $(uname -sm) in 'Linux x86_64') ;; *) echo 'Requires Linux X64' >&2; exit 1;; esac
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT=${OUT:-$(mktemp -d "${TMPDIR:-/tmp}/mlkit-time-x64-abi.XXXXXX")}
mkdir -p "$OUT"
export SML_LIB=${SML_LIB:-$ROOT}
MLKIT=${MLKIT:-$ROOT/bin/mlkit}
PVIEW=${PVIEW:-$ROOT/bin/pview}
cat > "$OUT/foreign.c" <<'C'
#include <assert.h>
#include <stdint.h>
#include <stdatomic.h>
extern _Atomic uintptr_t mlkit_tp_context;
#ifdef TP_GC
extern long disable_gc;
#endif
static void check(void) {
  assert((atomic_load(&mlkit_tp_context)&3)==2);
#ifdef TP_GC
  assert(disable_gc==1);
#endif
}
long tp_nine(long a,long b,long c,long d,long e,long f,long g,long h,long i) {
  check();
#ifdef TP_GC
  a=(a-1)/2; b=(b-1)/2; c=(c-1)/2; d=(d-1)/2; e=(e-1)/2;
  f=(f-1)/2; g=(g-1)/2; h=(h-1)/2; i=(i-1)/2;
  return 2*(a+2*b+3*c+4*d+5*e+6*f+7*g+8*h+9*i)+1;
#else
  return a+2*b+3*c+4*d+5*e+6*f+7*g+8*h+9*i;
#endif
}
int64_t tp_eight64(int64_t a,int64_t b,int64_t c,int64_t d,int64_t e,int64_t f,int64_t g,int64_t h) {
  check(); return a+2*b+3*c+4*d+5*e+6*f+7*g+8*h;
}
long tp_policy(long unused) {
  (void)unused;
#ifdef TP_GC
  return disable_gc*2+1;
#else
  return 1;
#endif
}
C
cat > "$OUT/main.sml" <<'SML'
fun raw__noinline (n:int) : int = prim ("tp_nine", (n,2,3,4,5,6,7,8,9))
fun converted__noinline (n:Int64.int) : Int64.int =
  prim ("@tp_eight64", (n,Int64.fromInt 2,Int64.fromInt 3,Int64.fromInt 4,
                        Int64.fromInt 5,Int64.fromInt 6,Int64.fromInt 7,Int64.fromInt 8))
fun loop 0 = ()
  | loop n =
      if raw__noinline 1 = 285 andalso converted__noinline (Int64.fromInt 1) = Int64.fromInt 204
      then loop (n-1) else raise Fail "C arguments/results changed"
val () = loop 10000
val () = if (prim ("tp_policy",0):int) = 1 then print "abi ok\n" else raise Fail "C GC policy"
SML
printf '$(SML_LIB)/basis/basis.mlb\nmain.sml\n' > "$OUT/main.mlb"
for config in gc gengc no_gc; do
 define=-DTP_GC
 [ "$config" != no_gc ] || define=
 cc -O2 -Wall -Wextra -Werror $define -c "$OUT/foreign.c" -o "$OUT/foreign.o"
 ar rcs "$OUT/libforeign.a" "$OUT/foreign.o"
 "$MLKIT" -no_par -"$config" -rp -libdirs "$OUT" -libs foreign,m,c,dl -o "$OUT/$config" "$OUT/main.mlb" > "$OUT/$config-build.log" 2>&1
 "$OUT/$config" > "$OUT/$config-off.log"
 grep -qx 'abi ok' "$OUT/$config-off.log"
 "$OUT/$config" +RTS -tp -tp_interval 100us -p_file "$OUT/$config.prof" > "$OUT/$config-on.log"
 cmp "$OUT/$config-off.log" "$OUT/$config-on.log"
 "$PVIEW" "$OUT/$config.prof" --format json -o "$OUT/$config.json"
done
echo "PASS: X64 register/stack C arguments, converted Int64 results and protected policy ($OUT)"

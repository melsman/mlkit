#include <inttypes.h>
#include <stdio.h>
#include "Region.h"
uint64_t diag_site_hits,diag_shared_entries,diag_shared_hits,diag_slow_entries,diag_legacy_entries;
static uint64_t c_calls,c_eligible,c_large,c_multi;
Region diag_resetRegion(Region r) {
  Ro *ro=clearStatusBits(r);
  int big=ro->lobjs!=NULL;
  int multi=clear_fp(ro->g0.fp)->n!=NULL;
#ifdef ENABLE_GEN_GC
  multi=multi || clear_fp(ro->g1.fp)->n!=NULL;
#endif
  ++c_calls; c_large+=big; c_multi+=multi; c_eligible+=!big && !multi;
  return resetRegion(r);
}
__attribute__((destructor)) static void report(void) {
  fprintf(stderr,"reset-audit site=%" PRIu64 " shared_entry=%" PRIu64 " shared_hit=%" PRIu64 " slow=%" PRIu64 " legacy=%" PRIu64 " c=%" PRIu64 " c_eligible=%" PRIu64 " c_large=%" PRIu64 " c_multi=%" PRIu64 "\n",diag_site_hits,diag_shared_entries,diag_shared_hits,diag_slow_entries,diag_legacy_entries,c_calls,c_eligible,c_large,c_multi);
}

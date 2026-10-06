/* Foreign-origin nesting, cleanup on exceptions, and global binding keys. */
#include "RegionProfile.h"
#include "String.h"
#include <assert.h>
#include <string.h>

const volatile uintptr_t mlkit_rp_capable = MLKIT_RP_MAGIC;
const volatile uintptr_t mlkit_rp_allocation_capable = 3;
Rp *global_freelist;
Context top_ctx;
static struct { size_t tag; char data[8]; } unit = {0,"fixture"};
static struct { size_t tag; char data[8]; } other = {0,"other"};
static struct { size_t tag; char data[8]; } ml = {0,"ML"};
static struct { size_t tag; char data[8]; } outer = {0,"outer"};
static struct { size_t tag; char data[8]; } inner = {0,"inner"};
static const MlkitAllocationRegion selected = {(String)&unit,(String)&ml,(String)&unit,42};
static const MlkitAllocationRegion unselected = {(String)&other,(String)&ml,(String)&unit,42};
static const MlkitAllocationSite sites[] = {
  {(String)&unit,(String)&ml,(String)&unit,1,0,0,(String)&unit},
  {(String)&unit,(String)&outer,(String)&unit,2,0,0,(String)&unit},
  {(String)&unit,(String)&inner,(String)&unit,3,0,0,(String)&unit}
};
int main(int argc, char **argv) {
  assert(argc == 2);
  context ctx = {0}; top_ctx = &ctx;
  Ro r = {0}, ignored = {0};
  mlkit_rp_enabled = 1; mlkit_rp_interval_us = 0;
  mlkit_rp_region = "fixture:42"; mlkit_rp_filename = argv[1];
  mlkit_rp_init();
  /* Global selectors use compiler keys, not the run-type enumeration. */
  static const char *selectors[] = {
    NULL,"<global>:3","<global>:4","<global>:5","<global>:6",
    "<global>:7","<global>:1","<global>:2"
  };
  for (uintptr_t type = 1; type <= 7; type++) {
    Ro global = {0};
    mlkit_rp_region = selectors[type];
    mlkit_rp_bind_global(&global,type);
    assert(global.allocation_profile);
  }
  mlkit_rp_region = "fixture:42";
  mlkit_rp_bind_region(&r,&selected);
  mlkit_rp_bind_region(&ignored,&unselected);
  assert(r.allocation_profile == &selected && !ignored.allocation_profile);
  assert(mlkit_rp_origin_token(&ctx) == (1));
  mlkit_rp_foreign_enter(&ctx,&sites[1],2000);
  assert(mlkit_rp_origin_token(&ctx) == ((uintptr_t)&sites[1] >> 3));
  mlkit_rp_foreign_enter(&ctx,&sites[2],1000);
  assert(mlkit_rp_origin_token(&ctx) == ((uintptr_t)&sites[2] >> 3));
  /* ML allocations carry their own token; the enclosing C origin is unchanged. */
  assert(mlkit_rp_origin_token(&ctx) == ((uintptr_t)&sites[2] >> 3));
  mlkit_rp_foreign_leave(&ctx,1000);
  assert(mlkit_rp_origin_token(&ctx) == ((uintptr_t)&sites[1] >> 3));
  mlkit_rp_foreign_enter(&ctx,&sites[2],900);
  mlkit_rp_foreign_unwind(&ctx,1500);
  assert(mlkit_rp_origin_token(&ctx) == ((uintptr_t)&sites[1] >> 3));
  mlkit_rp_foreign_unwind(&ctx,3000);
  assert(mlkit_rp_origin_token(&ctx) == (1));
  mlkit_rp_close();
  return 0;
}

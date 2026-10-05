/* Exercises counter identities and origin nesting independently of generated
 * code. Generated-code tests cover the native ABIs and real allocators. */
#include "RegionProfile.h"
#include "String.h"
#include <assert.h>
#include <string.h>

const volatile uintptr_t mlkit_rp_capable = MLKIT_RP_MAGIC;
const volatile uintptr_t mlkit_rp_allocation_capable = 2;
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
  mlkit_rp_allocation(&r,2,&ctx,&sites[0]);
  mlkit_rp_foreign_enter(&ctx,&sites[1],2000);
  mlkit_rp_allocation(&r,3,NULL,NULL);
  mlkit_rp_foreign_enter(&ctx,&sites[2],1000);
  mlkit_rp_allocation(&r,4,NULL,NULL);
  /* A callback allocates at its own ML site, not its C caller's origin. */
  mlkit_rp_allocation(&r,5,&ctx,&sites[0]);
  mlkit_rp_foreign_leave(&ctx,1000);
  mlkit_rp_allocation(&r,6,NULL,NULL);
  mlkit_rp_foreign_enter(&ctx,&sites[2],900);
  mlkit_rp_foreign_unwind(&ctx,1500);
  mlkit_rp_allocation(&r,7,NULL,NULL);
  mlkit_rp_foreign_unwind(&ctx,3000);
  mlkit_rp_allocation(&r,8,NULL,NULL);
  mlkit_rp_close();
  return 0;
}

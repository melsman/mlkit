/* Controlled maps and storage validate the byte accounting independently of
 * compiler lowering. Generated-ML tests exercise the native maps separately. */
#include "RegionProfile.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

const volatile uintptr_t mlkit_rp_capable = MLKIT_RP_MAGIC;
Rp *global_freelist;
static const struct { size_t size; char data[5]; } unit = {0,"test"};
static const struct { size_t size; char data[19]; } source = {0,"/fixtures/test.sml"};
static const uintptr_t sentinel[] = {UINTPTR_MAX, MLKIT_RP_MAGIC};
static uintptr_t *make_map(uintptr_t *dst, uintptr_t ret, uintptr_t delta,
                           uintptr_t count, const uintptr_t *entries) {
  uintptr_t header[] = {MLKIT_RP_MAGIC,ret,delta,count,(uintptr_t)&unit,(uintptr_t)&source};
  uintptr_t *end = dst+6+5*count;
  for (size_t i=0; i<6; i++) *(end-1-i)=header[i];
  end[-5] -= (uintptr_t)(end-5);
  end[-6] -= (uintptr_t)(end-6);
  for (size_t i=0; i<5*count; i++) *(end-7-i)=entries[i];
  return end;
}
int main(int argc, char **argv) {
  assert(argc == 2);
  mlkit_rp_enabled=1;
  mlkit_rp_initially_paused=1;
  mlkit_rp_filename=argv[1];
  assert(!mlkit_rp_parse_interval("-1ms"));
  assert(!mlkit_rp_parse_interval("10"));
  assert(!mlkit_rp_parse_interval("999999999999999999999999s"));
  assert(mlkit_rp_parse_interval("1us") && mlkit_rp_interval_us==1);
  assert(mlkit_rp_parse_interval("400us") && mlkit_rp_interval_us==400);
  assert(mlkit_rp_parse_interval("1000001us") && mlkit_rp_interval_us==1000001);
  assert(!mlkit_rp_parse_interval("400.5us"));
  assert(!mlkit_rp_parse_interval("-1us"));
  assert(!mlkit_rp_parse_interval("400usjunk"));
  assert(!mlkit_rp_parse_interval("2147483647000001us"));
  assert(mlkit_rp_parse_interval("0us") && mlkit_rp_interval_us==0);
  assert(mlkit_rp_parse_interval("10ms") && mlkit_rp_interval_us==10000);
  assert(mlkit_rp_parse_interval("2s") && mlkit_rp_interval_us==2000000);
  assert(mlkit_rp_parse_interval("8000i") && mlkit_rp_interval_entries==8000 && mlkit_rp_interval_us==0);
  assert(mlkit_rp_parse_interval("1i") && mlkit_rp_interval_entries==1);
  assert(!mlkit_rp_parse_interval("0i"));
  assert(!mlkit_rp_parse_interval("-1i"));
  assert(!mlkit_rp_parse_interval("1.5i"));
  assert(!mlkit_rp_parse_interval("8000ijunk"));
  assert(!mlkit_rp_parse_interval("18446744073709551616i"));
  assert(mlkit_rp_parse_interval("400us") && !mlkit_rp_interval_entries);
  assert(mlkit_rp_parse_interval("1i"));
  assert(mlkit_rp_parse_interval("0") && !mlkit_rp_interval_entries);
  mlkit_rp_init();
  uintptr_t stack[192]={0}, map1[32], map2[32];
  Ro *r=(Ro *)(stack+8);
  Rp *first, *last;
  assert(posix_memalign((void **)&first,sizeof(Rp),sizeof(Rp))==0);
  assert(posix_memalign((void **)&last,sizeof(Rp),sizeof(Rp))==0);
  first->n=last; last->n=NULL;
  mlkit_rp_page_alloc(); mlkit_rp_page_alloc();
  r->g0.fp=first; r->g0.a=last->i+7;
#ifdef ENABLE_GEN_GC
  Rp *older;
  assert(posix_memalign((void **)&older,sizeof(Rp),sizeof(Rp))==0);
  older->n=NULL;
  mlkit_rp_page_alloc();
  r->g1.fp=older; r->g1.a=older->i+5;
#endif
  Lobjs *big=malloc(sizeof(Lobjs)+4096);
  big->next=NULL; r->lobjs=big;
  mlkit_rp_large_alloc(big,512);
  context ctx={0}; ctx.topregion=r;
  uintptr_t entries[]={11,8,UINTPTR_MAX,0,2,12,2,3,0,1,13,3,0,0,7};
  uintptr_t *parent=make_map(map1,63,5,3,entries);
  uintptr_t child_entries[]={12,2,3,0,1};
  uintptr_t *child=make_map(map2,63,0,1,child_entries);
  /* Parent map is associated with child's return PC; parent storage uses the
   * second frame, after four spilled-result words. */
  memcpy(stack+68+8,r,sizeof(*r));
  ctx.topregion=(Ro *)(stack+68+8);
  stack[63]=(uintptr_t)parent;
  stack[131]=(uintptr_t)(sentinel+2);
  mlkit_rp_capture(&ctx,stack,child,2);
  mlkit_rp_large_free(big); free(big);
  ctx.topregion->lobjs=NULL;
  ctx.topregion->g0.fp=last;
  ctx.topregion->g0.a=last->i;
  mlkit_rp_capture(&ctx,stack+68,parent,0);
  mlkit_rp_capture(&ctx,stack+68,parent,0); /* idempotent */
  mlkit_rp_capture(&ctx,stack+68,parent,1);
  mlkit_rp_capture(&ctx,stack+68,parent,1); /* idempotent */
  mlkit_rp_capture(&ctx,stack+68,parent,2); /* explicit while paused */
  /* A peak entirely between snapshots, after the last snapshot and paused.
   * Releasing and reacquiring the same chain must not accumulate live pages. */
  mlkit_rp_pages_free(first);
  for (int round = 0; round < 3; round++) {
    for (int i = 0; i < 7; i++) mlkit_rp_page_alloc();
    for (int i = 0; i < 3; i++) mlkit_rp_pages_free(first);
    mlkit_rp_pages_free(last);
  }
  mlkit_rp_flush();
  mlkit_rp_close();
  free(first); free(last);
#ifdef ENABLE_GEN_GC
  free(older);
#endif
  return 0;
}

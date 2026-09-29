/* Controlled maps and storage validate the byte accounting independently of
 * compiler lowering. Generated-ML tests exercise the native maps separately. */
#include "RegionProfile.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

const uintptr_t mlkit_rp_capable = MLKIT_RP_MAGIC;
static const struct { size_t size; char data[5]; } unit = {0,"test"};
static const uintptr_t sentinel[] = {UINTPTR_MAX, MLKIT_RP_MAGIC};
static uintptr_t *make_map(uintptr_t *dst, uintptr_t ret, uintptr_t delta,
                           uintptr_t count, const uintptr_t *entries) {
  uintptr_t header[] = {MLKIT_RP_MAGIC,ret,delta,count,(uintptr_t)&unit};
  uintptr_t *end = dst+5+3*count;
  for (size_t i=0; i<5; i++) *(end-1-i)=header[i];
  end[-5] -= (uintptr_t)(end-5);
  for (size_t i=0; i<3*count; i++) *(end-6-i)=entries[i];
  return end;
}
int main(int argc, char **argv) {
  assert(argc == 2);
  mlkit_rp_enabled=1;
  mlkit_rp_initially_paused=1;
  mlkit_rp_filename=argv[1];
  mlkit_rp_init();
  uintptr_t stack[192]={0}, map1[32], map2[32];
  Ro *r=(Ro *)(stack+8);
  Rp *first, *last;
  assert(posix_memalign((void **)&first,sizeof(Rp),sizeof(Rp))==0);
  assert(posix_memalign((void **)&last,sizeof(Rp),sizeof(Rp))==0);
  first->n=last; last->n=NULL;
  r->g0.fp=first; r->g0.a=last->i+7;
  Lobjs *big=malloc(sizeof(Lobjs)+4096);
  big->next=NULL; r->lobjs=big;
  mlkit_rp_large_alloc(big,512);
  context ctx={0}; ctx.topregion=r;
  uintptr_t entries[]={11,8,UINTPTR_MAX,12,2,3,13,3,0};
  uintptr_t *parent=make_map(map1,63,5,3,entries);
  uintptr_t child_entries[]={12,2,3};
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
  mlkit_rp_flush();
  mlkit_rp_close();
  free(first); free(last);
  return 0;
}

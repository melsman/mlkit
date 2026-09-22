#include "Arm64GC.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

static uintptr_t relocate(uintptr_t p) { return p+1000; }
int main(void) {
  uintptr_t frame[]={1, UINT32_C(0x80000001),40,35,1};
  uintptr_t sentinel[]={UINTPTR_MAX,0,0};
  uintptr_t global=7, *roots[]={&global};
  mlkit_arm64_register_static_image(roots,(void *)0x4000,(void *)0x5000,roots,1);
  assert(mlkit_arm64_static_pointer((void *)0x4000));
  assert(!mlkit_arm64_static_pointer((void *)0x5000));
  uintptr_t second = 8, *second_roots[] = {&second};
  uintptr_t dynamic = 9, *dynamic_roots[] = {&dynamic};
  mlkit_arm64_register_static_image(second_roots, (void *)0x6000,
                                    (void *)0x7000, second_roots, 1);
  assert(!mlkit_arm64_in_static_data((void *)0x5500));
  mlkit_arm64_seal_main_image();
  assert(mlkit_arm64_dynamic_images == 0);
  assert(mlkit_arm64_in_static_data((void *)0x5500));
  assert(!mlkit_arm64_in_static_data((void *)0x7000));
  mlkit_arm64_register_static_image(dynamic_roots, (void *)0x9000,
                                    (void *)0xa000, dynamic_roots, 1);
  assert(mlkit_arm64_dynamic_images == 1);
  assert(mlkit_arm64_in_static_data((void *)0x9000));
  assert(!mlkit_arm64_in_static_data((void *)0x8000));
  assert(!mlkit_arm64_in_static_data((void *)0xa000));
  uintptr_t stack[64]={0}, snapshot[44]={0};
  for(unsigned i=0;i<64;i++) stack[i]=i;
  snapshot[31]=11; snapshot[12]=19; /* x0 and x19 */
  snapshot[13]=18; snapshot[40]=2; snapshot[41]=1; snapshot[42]=5;
  snapshot[43]=(uintptr_t)stack;
  stack[7]=(uintptr_t)(frame+5); stack[10+35]=(uintptr_t)(sentinel+3);
  mlkit_arm64_visit_roots(snapshot,(1u<<19)|1,relocate);
  assert(snapshot[31]==1011 && snapshot[12]==1019 && snapshot[13]==18);
  assert(stack[0]==0 && stack[1]==1 && stack[2]==1002 && stack[4]==1004);
  assert(stack[8]==8 && stack[9]==9); /* uninitialized result slots/padding */
  assert(stack[49]==1049 && stack[18]==1018 && stack[17]==1017);
  assert(stack[45]==(uintptr_t)(sentinel+3) && global==1007);
  assert(second == 1008 && dynamic == 1009);
  mlkit_arm64_unregister_static_image(dynamic_roots);
  assert(mlkit_arm64_dynamic_images == 0);
  assert(!mlkit_arm64_in_static_data((void *)0x9000));
  mlkit_arm64_unregister_static_image(second_roots);
  assert(!mlkit_arm64_in_static_data((void *)0x5500));
  mlkit_arm64_unregister_static_image(roots);
  assert(!mlkit_arm64_static_pointer((void *)0x4000));
  /* Frame walking is independent of image registration. */
  mlkit_arm64_visit_roots(snapshot,0,relocate);
  assert(stack[49]==2049 && global==1007);
  pid_t child=fork(); assert(child>=0);
  if(child==0) { freopen("/dev/null","w",stderr); mlkit_arm64_visit_roots(snapshot,(uintptr_t)1<<18,relocate); _exit(0); }
  int status; assert(waitpid(child,&status,0)==child);
  assert(WIFSIGNALED(status) && WTERMSIG(status)==SIGABRT);
  puts("ARM64 GC register/stack/global relocation, inline descriptors, and reserved-register checks passed");
}

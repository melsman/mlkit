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
  MLKitArm64Frame table[]={{0x200,sentinel+3},{0x100,frame+5}};
  uintptr_t global=7, *roots[]={&global};
  mlkit_arm64_register_image(table,2,(void *)0x4000,(void *)0x5000,roots,1);
  assert(mlkit_arm64_static_pointer((void *)0x4000));
  assert(!mlkit_arm64_static_pointer((void *)0x5000));
  uintptr_t stack[64]={0}, snapshot[44]={0};
  for(unsigned i=0;i<64;i++) stack[i]=i;
  snapshot[31]=11; snapshot[12]=19; /* x0 and x19 */
  snapshot[13]=18; snapshot[40]=2; snapshot[41]=1; snapshot[42]=5;
  snapshot[43]=(uintptr_t)stack;
  stack[7]=0x100; stack[10+35]=0x200;
  mlkit_arm64_visit_roots(snapshot,(1u<<19)|1,relocate);
  assert(snapshot[31]==1011 && snapshot[12]==1019 && snapshot[13]==18);
  assert(stack[0]==0 && stack[1]==1 && stack[2]==1002 && stack[4]==1004);
  assert(stack[8]==8 && stack[9]==9); /* uninitialized result slots/padding */
  assert(stack[49]==1049 && stack[18]==1018 && stack[17]==1017);
  assert(stack[45]==0x200 && global==1007);
  mlkit_arm64_unregister_image(table);
  assert(!mlkit_arm64_static_pointer((void *)0x4000));
  pid_t child=fork(); assert(child>=0);
  if(child==0) { freopen("/dev/null","w",stderr); mlkit_arm64_visit_roots(snapshot,0,relocate); _exit(0); }
  int status; assert(waitpid(child,&status,0)==child);
  assert(WIFSIGNALED(status) && WTERMSIG(status)==SIGABRT);
  puts("ARM64 GC register/stack/global relocation and missing-PC checks passed");
}

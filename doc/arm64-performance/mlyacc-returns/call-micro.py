from pathlib import Path
import subprocess,json,random,statistics
r=Path(__file__).resolve().parent
asm=['.text']
variants=['b_ret','b_br','bl_ret','bl_br']
for v in variants:
 call,ret=v.split('_')
 asm += ['.p2align 6','.globl _'+v,'_'+v+':','stp x19, x30, [sp, #-16]!','mov x19, x0','mov x0, #0','L_'+v+'_loop:']
 asm += ['adr x30, L_'+v+'_cont','b L_'+v+'_leaf'] if call=='b' else ['nop','bl L_'+v+'_leaf']
 asm += ['L_'+v+'_cont:','subs x19, x19, #1','b.ne L_'+v+'_loop','ldp x19, x30, [sp], #16','ret','L_'+v+'_leaf:','add x0, x0, #1','ret' if ret=='ret' else 'br x30']
(r/'call-micro.s').write_text('\n'.join(asm)+'\n')
(r/'call-micro.c').write_text('''#include <stdio.h>
#include <stdlib.h>
#include <time.h>
extern unsigned long b_ret(unsigned long), b_br(unsigned long), bl_ret(unsigned long), bl_br(unsigned long);
int main(int argc, char **argv) {
  unsigned long (*f[])(unsigned long) = {b_ret,b_br,bl_ret,bl_br};
  struct timespec a,b;
  if (argc != 2) return 1;
  int mode=atoi(argv[1]); if (mode<0 || mode>3) return 1;
  clock_gettime(CLOCK_MONOTONIC,&a);
  unsigned long result=f[mode](10000000);
  clock_gettime(CLOCK_MONOTONIC,&b);
  if(result!=10000000) return 2;
  printf("%.9f\\n",(b.tv_sec-a.tv_sec)+(b.tv_nsec-a.tv_nsec)*1e-9);
  return 0;
}
''')
subprocess.run(['gcc','-arch','arm64','-O2',str(r/'call-micro.c'),str(r/'call-micro.s'),'-o',str(r/'call-micro.exe')],check=True)
samples={v:[] for v in variants};rng=random.Random(224)
for rep in range(6):
 order=list(enumerate(variants));rng.shuffle(order)
 for i,v in order:
  t=float(subprocess.check_output([str(r/'call-micro.exe'),str(i)]))
  if rep:samples[v].append(t)
result={'iterations':10000000,'samples_seconds':samples,'median_seconds':{v:statistics.median(s) for v,s in samples.items()}}
(r/'call-micro.json').write_text(json.dumps(result,indent=2)+'\n');print(result['median_seconds'])

from pathlib import Path
import subprocess,shutil,os,sys
r=Path('/private/tmp/mlkit-three-fixed');variant=sys.argv[1]
compiler={'arm64':r/'mlkit-v2','old':Path('/private/tmp/mlkit-nogc20-current/mlkit-arm64'),'x64':Path('/private/tmp/mlkit-nogc20-current/mlkit-x64')}[variant]
for mode in ['no_gc','gc']:
 cache='ThreeFinal'+mode.replace('_','')+variant
 for name in ['simple','mpuz','uf']:
  key=f'final-{mode}-{variant}-{name}';d=r/key;d.mkdir(exist_ok=True)
  if name=='uf':
   for p in Path('/private/tmp/mlkit-nogc20-current/paper/src/uf').iterdir():
    if p.is_file():shutil.copy2(p,d/p.name)
  else:shutil.copy2('/private/tmp/mlkit-nogc20-current/bench/benchmarks/'+name+'.sml',d/(name+'.sml'))
  cmd=[str(compiler),'-'+mode,'--mlb-subdir',cache,'--no_delete_target_files','-ldexe','gcc -arch '+('x86_64 -Wl,-ld_classic' if variant=='x64' else 'arm64'),'-o',str(r/(key+'.exe')),name+('.mlb' if name=='uf' else '.sml')]
  with (r/(key+'-build.log')).open('w') as log:subprocess.run(cmd,cwd=d,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=log,stderr=subprocess.STDOUT,check=True)
  print('built',key,flush=True)

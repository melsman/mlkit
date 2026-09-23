from pathlib import Path
import subprocess,os
r=Path('/private/tmp/mlkit-nogc20-current');repo=Path('/private/tmp/mlkit-m2-source')
env=dict(os.environ,SML_LIB=str(repo))
def run(cmd,name):
 with (r/name).open('w') as log:subprocess.run(cmd,cwd=repo,env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
for arch,mlb in [('arm64','mlkitarm64.mlb'),('x64','mlkit64.mlb')]:
 run(['/private/tmp/mlkit-m7/x64-final','-gc','--mlb-subdir','M7X64Final','-ldexe','gcc -arch x86_64 -Wl,-ld_classic,-stack_size,0x40000000','-o',str(r/('mlkit-'+arch)),'src/Compiler/'+mlb],arch+'-build.log')
 print('Built host compiler',arch,flush=True)
try:
 for arch,native in [('arm64','1'),('x64','0')]:
  run(['./configure','CC=gcc','DARWIN_NATIVE='+native],'configure-'+arch+'.log')
  run(['make','-C','src/Runtime','-j4','runtimeSystem.a'],'runtime-'+arch+'.log')
  print('Built no-GC runtime',arch,flush=True)
finally:
 run(['./configure','CC=gcc','DARWIN_NATIVE=0'],'configure-restore.log')

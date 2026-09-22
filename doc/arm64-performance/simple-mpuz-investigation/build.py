from pathlib import Path
import subprocess,shutil,os
r=Path('/private/tmp/mlkit-simple-mpuz')
for n in ['simple','mpuz']:
 for a in ['arm64','x64']:
  d=r/(n+'-'+a);d.mkdir(exist_ok=True);shutil.copy2('/private/tmp/mlkit-nogc20-current/bench/benchmarks/'+n+'.sml',d/(n+'.sml'))
  with (r/(n+'-'+a+'-build.log')).open('w') as log:subprocess.run(['/private/tmp/mlkit-nogc20-current/mlkit-'+a,'-no_gc','--mlb-subdir','NoGC20Current'+a,'--no_delete_target_files','-ldexe','python3 '+str(r/'link.py')+' '+('arm64' if a=='arm64' else 'x86_64'),'-o',str(r/(n+'-'+a+'.exe')),n+'.sml'],cwd=d,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=log,stderr=subprocess.STDOUT,check=True)
  print('built',n,a,flush=True)

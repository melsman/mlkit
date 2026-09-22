from pathlib import Path
import subprocess,shutil,os
r=Path('/private/tmp/mlkit-uf-nogc')
for a in ['arm64','x64']:
 d=r/a;shutil.copytree('/private/tmp/mlkit-nogc20-current/paper/src/uf',d,ignore=shutil.ignore_patterns('MLB'))
 with (r/(a+'-build.log')).open('w') as log:subprocess.run(['/private/tmp/mlkit-nogc20-current/mlkit-'+a,'-no_gc','--mlb-subdir','NoGC20Current'+a,'--no_delete_target_files','-ldexe','python3 '+str(r/'link.py')+' '+('arm64' if a=='arm64' else 'x86_64'),'-o',str(r/(a+'.exe')),'uf.mlb'],cwd=d,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=log,stderr=subprocess.STDOUT,check=True)

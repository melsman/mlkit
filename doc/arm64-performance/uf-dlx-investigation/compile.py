import subprocess,os,shutil
from pathlib import Path
r=Path('/private/tmp/mlkit-outliers')
for name in ['uf','dlx']:
 for arch in ['arm64','x64']:
  cwd=r/(name+'-'+arch);shutil.copytree(Path('/private/tmp/mlkit-gc20-current/paper/src')/name,cwd,ignore=shutil.ignore_patterns('MLB'))
  cmd=[f'/private/tmp/mlkit-gc20-current/mlkit-{arch}','-gc','--mlb-subdir','GC20Current'+arch,'--no_delete_target_files','-ldexe','python3 '+str(r/'link.py')+' '+('x86_64' if arch=='x64' else arch),'-o',str(r/(name+'-'+arch+'.exe')),name+'.mlb']
  with (r/(name+'-'+arch+'.log')).open('w') as f:subprocess.run(cmd,cwd=cwd,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=f,stderr=subprocess.STDOUT,check=True)
  print('compiled',name,arch,flush=True)

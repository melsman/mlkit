from pathlib import Path
import subprocess,shutil,os
r=Path('/private/tmp/mlkit-outliers-fixed')
for arch in ['arm64','x64']:
 compiler='/private/tmp/mlkit-outliers/mlkit-fixed' if arch=='arm64' else '/private/tmp/mlkit-gc20-current/mlkit-x64'
 for name in ['uf','dlx']:
  cwd=r/(name+'-'+arch)
  if not cwd.exists():shutil.copytree(Path('/private/tmp/mlkit-gc20-current/paper/src')/name,cwd,ignore=shutil.ignore_patterns('MLB'))
  cmd=[compiler,'-gc','--mlb-subdir','UfDlxFixed'+arch,'--no_delete_target_files','-o',str(r/(name+'-'+arch+'.exe')),name+'.mlb']
  with (r/(name+'-'+arch+'.log')).open('w') as log:subprocess.run(cmd,cwd=cwd,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=log,stderr=subprocess.STDOUT,check=True)
  print('built',name,arch,flush=True)

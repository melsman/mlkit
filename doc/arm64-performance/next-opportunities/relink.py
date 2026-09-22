import os,subprocess
from pathlib import Path
r=Path('/private/tmp/mlkit-m12-next');cwd=Path('/private/tmp/mlkit-m12-return-fix/after/bench')
for name,src in [('mlyacc','mlyacc.mlb')]:
 with (r/(name+'-link.log')).open('w') as f:
  subprocess.run(['/private/tmp/mlkit-m12-return-fix/mlkit','-gc','--mlb-subdir','M12RetAfter','--no_delete_target_files','-ldexe','python3 '+str(r/'link.py'),'-o',str(r/(name+'-baseline.exe')),src],cwd=cwd,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=f,stderr=subprocess.STDOUT,check=True)
 print(name,'linked',flush=True)

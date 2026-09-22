import os,subprocess
from pathlib import Path
r=Path('/private/tmp/mlkit-m12-residual');cwd=Path('/private/tmp/mlkit-m12-fresh-basis/bench')
for name,src in [('mlyacc','mlyacc.mlb'),('professor','professor.sml')]:
 with (r/(name+'-link.log')).open('w') as f:
  subprocess.run(['/private/tmp/mlkit-m12-frames/mlkit','-gc','--mlb-subdir','M12FreshFrames','--no_delete_target_files','-ldexe','python3 '+str(r/'link.py'),'-o',str(r/(name+'-baseline.exe')),src],cwd=cwd,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=f,stderr=subprocess.STDOUT,check=True)
 print(name,'linked',flush=True)

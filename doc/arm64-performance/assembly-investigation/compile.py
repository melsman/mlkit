from pathlib import Path
import os,shutil,subprocess
root=Path('/private/tmp/mlkit-m12-assembly')
for folder in [root/'bench',root/'artifact']:
 for p in list(folder.rglob('MLB')):
  if p.is_dir(): shutil.rmtree(p)
programs=[('nucleic',root/'artifact/nucleic','nucleic.mlb'),('mlyacc',root/'bench','mlyacc.mlb'),('professor',root/'bench','professor.sml')]
for name,cwd,source in programs:
 cmd=['/private/tmp/mlkit-m12-static/mlkit','-gc','--mlb-subdir','M12Study','--no_delete_target_files','-o',str(root/(name+'-gc.exe')),source]
 with (root/(name+'-gc.final-compile.log')).open('w') as log:
  subprocess.run(cmd,cwd=cwd,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=log,stderr=subprocess.STDOUT,check=True)
 print(name+' compiled',flush=True)

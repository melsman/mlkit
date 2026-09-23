from pathlib import Path
import os,subprocess,shutil
root=Path('/private/tmp/mlkit-m12-select')
shutil.copytree('/private/tmp/mlkit-m12-fresh-basis/bench',root/'bench',ignore=shutil.ignore_patterns('MLB'),dirs_exist_ok=True)
for name,src in [('mlyacc','mlyacc.mlb'),('professor','professor.sml')]:
 with (root/(name+'-compile.log')).open('w') as out:
  subprocess.run([str(root/'mlkit'),'-gc','--mlb-subdir','M12SelectFinal','--no_delete_target_files','-o',str(root/(name+'-gc.exe')),src],cwd=root/'bench',env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=out,stderr=subprocess.STDOUT,check=True)
 print(name+' compiled',flush=True)

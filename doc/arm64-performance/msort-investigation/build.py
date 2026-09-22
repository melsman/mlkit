from pathlib import Path
import subprocess,shutil,os
r=Path('/private/tmp/mlkit-msort')
for arch in ['arm64','x64']:
 d=r/arch;d.mkdir(exist_ok=True);shutil.copy2('/private/tmp/mlkit-gc20-current/bench/benchmarks/msort.sml',d/'msort.sml')
 compiler='/private/tmp/mlkit-outliers/mlkit-fixed' if arch=='arm64' else '/private/tmp/mlkit-gc20-current/mlkit-x64'
 with (r/(arch+'-build.log')).open('w') as log:
  subprocess.run([compiler,'-gc','--mlb-subdir','UfDlxFixed'+arch,'--no_delete_target_files','-o',str(r/(arch+'.exe')),'msort.sml'],cwd=d,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=log,stderr=subprocess.STDOUT,check=True)
 print(arch,'built',flush=True)

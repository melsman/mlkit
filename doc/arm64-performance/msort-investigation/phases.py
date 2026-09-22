from pathlib import Path
import subprocess,os
r=Path('/private/tmp/mlkit-msort')
for a in ['arm64','x64']:
 d=r/a;source=(d/'msort.sml').read_text();source=source.replace('val nums =', 'val _:int = prim("@msort_mark",0)\nval nums =').replace('val result = print "Sorting...', 'val _:int = prim("@msort_mark",1)\nval result = print "Sorting...').replace('val result = print "Done.', 'val _:int = prim("@msort_mark",2)\nval result = print "Done.')
 (d/'phase.sml').write_text(source)
 arch='x86_64' if a=='x64' else a
 subprocess.run(['gcc','-arch',arch,'-O2','-Wall','-Wextra','-Werror','-c',str(r/'phase.c'),'-o',str(r/(a+'-phase.o'))],check=True)
 compiler='/private/tmp/mlkit-outliers/mlkit-fixed' if a=='arm64' else '/private/tmp/mlkit-gc20-current/mlkit-x64'
 with (r/(a+'-phase-build.log')).open('w') as log:subprocess.run([compiler,'-gc','--mlb-subdir','UfDlxFixed'+a,'--no_delete_target_files','-ldexe','gcc -arch '+arch+' '+str(r/(a+'-phase.o')),'-o',str(r/(a+'-phase.exe')),'phase.sml'],cwd=d,env=dict(os.environ,SML_LIB='/private/tmp/mlkit-m2-source'),stdout=log,stderr=subprocess.STDOUT,check=True)

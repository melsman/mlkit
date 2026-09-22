from pathlib import Path
import os,subprocess,shutil
root=Path('/private/tmp/mlkit-m12-return-fix')
repo=Path('/private/tmp/mlkit-m2-source')
for variant,compiler,cache in [('before','/private/tmp/mlkit-m12-typed/mlkit','M12RetBefore'),('after',str(root/'mlkit'),'M12RetAfter'),('x64',str(root/'mlkit-x64'),'M12RetX64')]:
 assert not list((repo/'basis').rglob('*'+cache+'*')),cache
 for folder in ['bench','nucleic']:
  shutil.copytree(Path('/private/tmp/mlkit-m12-fresh-basis')/folder,root/variant/folder,ignore=shutil.ignore_patterns('MLB'),dirs_exist_ok=True)
 for name,folder,src in [('nucleic','nucleic','nucleic.mlb'),('mlyacc','bench','mlyacc.mlb'),('professor','bench','professor.sml')]:
  with (root/(variant+'-'+name+'-compile.log')).open('w') as out:
   subprocess.run([compiler,'-gc','--mlb-subdir',cache,'--no_delete_target_files','-o',str(root/(variant+'-'+name+'.exe')),src],cwd=root/variant/folder,env=dict(os.environ,SML_LIB=str(repo)),stdout=out,stderr=subprocess.STDOUT,check=True)
  print(variant,name,'compiled',flush=True)

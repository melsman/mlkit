import sys,os,json,shutil
from pathlib import Path
root=Path('/private/tmp/mlkit-simple-mpuz');arch=sys.argv[1];args=sys.argv[2:];name=Path(args[args.index('-o')+1]).stem
for i,a in enumerate(args):
 if a.endswith('base-link_objects.o'):
  obj=root/(name+'-link.o');shutil.copyfile(a,obj);args[i]=str(obj)
  if Path(a).with_suffix('.s').exists():shutil.copyfile(Path(a).with_suffix('.s'),obj.with_suffix('.s'))
with (root/'links.jsonl').open('a') as f:f.write(json.dumps({'arch':arch,'cwd':os.getcwd(),'args':args})+'\n')
os.execvp('gcc',['gcc','-arch',arch]+args)

#!/usr/bin/env python3
import json,os,sys,shutil
from pathlib import Path
args=sys.argv[1:]
root=Path('/private/tmp/mlkit-m12-next')
name=Path(args[args.index('-o')+1]).stem
for i,a in enumerate(args):
 if a.endswith('base-link_objects.o'):
  saved=root/(name+'-link.o');shutil.copyfile(a,saved)
  shutil.copyfile(Path(a).with_suffix('.s'),saved.with_suffix('.s'))
  args[i]=str(saved)
with (root/'links.jsonl').open('a') as f:
 f.write(json.dumps({'cwd':os.getcwd(),'args':args})+'\n')
os.execvp('gcc',['gcc','-arch','arm64']+args)

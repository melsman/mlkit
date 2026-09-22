from pathlib import Path
import json,re,subprocess
r=Path('/private/tmp/mlkit-simple-mpuz');rows=[json.loads(x) for x in (r/'links.jsonl').read_text().splitlines()];stats={}
for name in ['simple','mpuz']:
 row=next(x for x in rows if str(r/(name+'-arm64.exe')) in x['args']);args=row['args'];idx=next(i for i,a in enumerate(args) if a.endswith('/'+name+'.sml.o'));p=Path(args[idx]);p=p if p.is_absolute() else Path(row['cwd'])/p;source=p.with_suffix('.s').read_text()
 for variant in ['loads','combined']:
  s=source if variant=='loads' else (r/(name+('-loops.s' if name=='simple' else '-arithmetic.s'))).read_text()
  fun='sub_14' if name=='simple' else 'app15';match=re.search(r'^_F\.'+fun+r'_[^:]+:\n[\s\S]*?(?=^\.text)',s,re.M);assert match
  def split(m):
   a,b,base,off=m.groups();off=int(off)
   ins=[(a,off),(b,off+8)]
   if a==base:ins.reverse()
   return ''.join('\tldr '+reg+', ['+base+', #'+str(offset)+']\n' for reg,offset in ins)
  body,n=re.subn(r'\tldp (x\d+), (x\d+), \[(x\d+), #(-?\d+)\]\n',split,match[0]);assert n==(1 if name=='simple' else 2),n
  s=s[:match.start()]+body+s[match.end():];key=name+'-'+variant;asm=r/(key+'.s');asm.write_text(s);obj=r/(key+'.o');subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(obj)],check=True)
  new=args[:];new[idx]=str(obj);new[new.index('-o')+1]=str(r/(key+'.exe'));subprocess.run(['gcc','-arch','arm64']+new,cwd=row['cwd'],check=True);stats[key]=n
(r/'load-experiments.json').write_text(json.dumps(stats,indent=2)+'\n');print(stats)

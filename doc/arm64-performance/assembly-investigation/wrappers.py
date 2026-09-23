from pathlib import Path
import json,re,subprocess,collections
r=Path('/private/tmp/mlkit-m12-assembly');links={}
for row in map(json.loads,(r/'links.jsonl').read_text().splitlines()):links[Path(row['args'][1]).name.split('-')[0]]=row
stats={}
for name,row in links.items():
 folder=r/(name+'-wrappers');folder.mkdir(exist_ok=True);args=list(row['args']);args[1]=str(r/(name+'-wrappers.exe'));count=0
 for i,a in enumerate(args):
  if not a.endswith('.o'):continue
  obj=Path(a);obj=obj if obj.is_absolute() else Path(row['cwd'])/obj
  if not str(obj).startswith(str(r/'bench')):continue
  asm=obj.with_suffix('.s');s=asm.read_text()
  pattern=r'(^_F\.[^\n]+:\n)\tstp x29, x30, \[sp, #0\]\n\tadd x29, sp, #0\n\tsub sp, sp, #16\n\tldp x29, x30, \[sp, #16\]\n\tadd sp, sp, #16\n(\tb _F\.[^\n]+\n)'
  t,n=re.subn(pattern,r'\1\2',s,flags=re.M)
  if not n:continue
  out=folder/(str(i)+'-'+asm.name);out.write_text(t);objout=out.with_suffix('.o')
  subprocess.run(['gcc','-arch','arm64','-c',str(out),'-o',str(objout)],check=True)
  args[i]=str(objout);count+=n
 subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True)
 stats[name]={'identity_tail_wrappers_simplified':count,'instructions_removed':count*5}
 print(name,stats[name])
(r/'wrappers.json').write_text(json.dumps(stats,indent=2)+'\n')

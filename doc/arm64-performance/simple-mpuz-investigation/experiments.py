from pathlib import Path
import subprocess,json,re
r=Path('/private/tmp/mlkit-simple-mpuz');rows=[json.loads(x) for x in (r/'links.jsonl').read_text().splitlines()];stats={}
for name in ['simple','mpuz']:
 row=next(x for x in rows if x['arch']=='arm64' and str(r/(name+'-arm64.exe')) in x['args']);args=row['args'];idx=next(i for i,a in enumerate(args) if a.endswith('/'+name+'.sml.o'));p=Path(args[idx]);p=p if p.is_absolute() else Path(row['cwd'])/p;source=p.with_suffix('.s').read_text()
 def loops(s):
  labels=re.findall(r'^(_F\.(?:sub_14|lr(?:7|8|9|10|11))_[^:]+):$',s,re.M);assert len(labels)==(1 if name=='simple' else 5)
  for i,label in enumerate(labels):
   head=label+':\n\tstp x29, x30, [sp, #0]\n\tadd x29, sp, #0\n';assert head in s
   loop='L_diag_loop_'+str(i);s=s.replace(head,head+loop+':\n')
   tail='\tsub sp, sp, #16\n\tldp x29, x30, [sp, #16]\n\tadd sp, sp, #16\n\tb '+label+'\n';assert s.count(tail)==1,(name,label);s=s.replace(tail,'\tb '+loop+'\n')
  return s,len(labels)
 def arithmetic(s):
  pattern=r'\tsub sp, sp, #16\n\tstr x16, \[sp, #0\]\n(\t(?:movz x17, #[0-9]+, lsl #0|ldr x17, \[sp, #[0-9]+\])\n)\tldr x16, \[sp, #0\]\n\tadd sp, sp, #16\n(?=\tsmulh)'
  def replace(m):
   text=m[1]
   if '[sp,' in text:
    off=int(re.search(r'#(\d+)',text)[1]);assert off>=16;text=text.replace('#'+str(off),'#'+str(off-16))
   return text
  return re.subn(pattern,replace,s)
 for variant in (['loops'] if name=='simple' else ['loops','arithmetic','both']):
  s=source;nl=na=0
  if variant in ['loops','both']:s,nl=loops(s)
  if variant in ['arithmetic','both']:s,na=arithmetic(s);assert na>0
  key=name+'-'+variant;asm=r/(key+'.s');asm.write_text(s);obj=r/(key+'.o');subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(obj)],check=True)
  new=args[:];new[idx]=str(obj);new[new.index('-o')+1]=str(r/(key+'.exe'));subprocess.run(['gcc','-arch','arm64']+new,cwd=row['cwd'],check=True)
  stats[key]={'loops':nl,'multiply_staging_sites':na}
(r/'experiments.json').write_text(json.dumps(stats,indent=2)+'\n');print(stats)

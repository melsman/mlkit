from pathlib import Path
import subprocess,json,bisect,collections,struct
r=Path('/private/tmp/mlkit-simple-mpuz');subprocess.run(['gcc','-arch','arm64','-O2','-Wall','-Wextra','-Werror','-c',str(r/'sampler.c'),'-o',str(r/'sampler.o')],check=True)
rows=[json.loads(x) for x in (r/'links.jsonl').read_text().splitlines()]
for n in ['simple','mpuz']:
 row=next(x for x in rows if str(r/(n+'-arm64.exe')) in x['args']);args=row['args'][:];exe=r/(n+'-sample.exe');args[args.index('-o')+1]=str(exe);args.append(str(r/'sampler.o'));subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True)
 syms=[]
 for line in subprocess.check_output(['nm','-n',str(exe)],text=True).splitlines():
  t=line.split()
  if len(t)==3 and t[1].lower()=='t':syms.append((int(t[0],16),t[2]))
 data=exe.read_bytes();pos=32;bounds=None
 for _ in range(struct.unpack_from('<I',data,16)[0]):
  cmd,size=struct.unpack_from('<II',data,pos)
  if cmd==25 and data[pos+8:pos+24].rstrip(b'\0')==b'__TEXT':bounds=struct.unpack_from('<QQ',data,pos+24)
  pos+=size
 assert bounds
 syms.sort();addrs=[x[0] for x in syms];counts=collections.Counter();pcs=[]
 for rep in range(3):
  p=subprocess.run([str(exe)],capture_output=True,text=True,check=True)
  assert p.stdout==Path('/private/tmp/mlkit-nogc20-current/bench/benchmarks/'+n+'.sml.out.ok').read_text()
  for line in p.stderr.splitlines():
   if line.startswith('PC '):
    pc=int(line[3:],16);pcs.append(pc);i=bisect.bisect_right(addrs,pc)-1
    counts[syms[i][1] if i>=0 and bounds[0]<=pc<sum(bounds) else '[outside executable]']+=1
 print(n,'samples',len(pcs),counts.most_common(18),flush=True)
 (r/(n+'-pc-samples.json')).write_text(json.dumps({'text_segment':bounds,'symbols':syms,'pcs':pcs,'counts':counts},indent=2)+'\n')

from pathlib import Path
import re,json,subprocess
r=Path('/private/tmp/mlkit-simple-mpuz');row=next(json.loads(x) for x in (r/'links.jsonl').read_text().splitlines() if str(r/'mpuz-arm64.exe') in x);args=row['args'];idx=next(i for i,a in enumerate(args) if a.endswith('/mpuz.sml.o'));s=(r/'mpuz-branch-all.s').read_text();m=re.search(r'^_F\.app15_[^:]+:\n[\s\S]*?(?=^\.text)',s,re.M)
def split(m):
 a,b,base,off=m.groups();off=int(off);ins=[(a,off),(b,off+8)]
 if a==base:ins.reverse()
 return ''.join('\tldr '+reg+', ['+base+', #'+str(offset)+']\n' for reg,offset in ins)
body,n=re.subn(r'\tldp (x\d+), (x\d+), \[(x\d+), #(-?\d+)\]\n',split,m[0]);assert n==2;s=s[:m.start()]+body+s[m.end():];key='mpuz-branch-all-loads';asm=r/(key+'.s');asm.write_text(s);obj=r/(key+'.o');subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(obj)],check=True);new=args[:];new[idx]=str(obj);new[new.index('-o')+1]=str(r/(key+'.exe'));subprocess.run(['gcc','-arch','arm64']+new,cwd=row['cwd'],check=True)
s=(r/'measure.py').read_text().replace("['x64','arm64','loops']","['x64','arm64','combined']").replace("['x64','arm64','loops','arithmetic','both']","['x64','arm64','branch-all','branch-all-loads']").replace("r/'measurements.json'","r/'final-measurements.json'");(r/'measure-final.py').write_text(s)

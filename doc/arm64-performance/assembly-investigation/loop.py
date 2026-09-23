from pathlib import Path
import json,re,subprocess
r=Path('/private/tmp/mlkit-m12-assembly')
rows=list(map(json.loads,(r/'links.jsonl').read_text().splitlines()))
row=next(x for x in reversed(rows) if 'professor' in x['args'][1]);args=list(row['args']);args[1]=str(r/'professor-loop.exe')
i=next(i for i,a in enumerate(args) if a.endswith('/professor.sml.o'))
obj=Path(row['cwd'])/args[i];asm=obj.with_suffix('.s');s=asm.read_text()
m=re.search(r'^(_F.count37_[^:]+):\n',s,re.M);assert m
start=m.start();end=s.index('\n.text',start);body=s[start:end];symbol=m[1]
assert '_time_to_gc' not in body and '\tbl ' not in body
entry=f'{symbol}:\n\tstp x29, x30, [sp, #0]\n\tadd x29, sp, #0\n'
assert body.startswith(entry)
body=body.replace(entry,entry+'L_study_count_loop:\n',1)
old='\tsub sp, sp, #16\n\tldp x29, x30, [sp, #16]\n\tadd sp, sp, #16\n\tb '+symbol+'\n'
assert old in body
body=body.replace(old,'\tb L_study_count_loop\n')
out=r/'professor-loop.s';out.write_text(s[:start]+body+s[end:]);objout=out.with_suffix('.o')
subprocess.run(['gcc','-arch','arm64','-c',str(out),'-o',str(objout)],check=True)
args[i]=str(objout);subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True)
s=(r/'measure-wrappers.py').read_text().replace("[('mlyacc','mlyacc.mlb')]","[('professor','professor.sml')]").replace("'wrappers':root/(name+'-wrappers.exe')","'loop':root/(name+'-loop.exe')").replace('wrapper-measurements.json','loop-measurements.json')
(r/'measure-loop.py').write_text(s)

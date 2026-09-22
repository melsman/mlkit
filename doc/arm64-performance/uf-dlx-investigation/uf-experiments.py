from pathlib import Path
import re,json,subprocess
r=Path('/private/tmp/mlkit-outliers');rows=list(map(json.loads,(r/'links.jsonl').read_text().splitlines()));row=next(x for x in rows if x['arch']=='arm64' and any(a.endswith('/uf-arm64.exe') for a in x['args']))
idx=next(i for i,a in enumerate(row['args']) if a.endswith('/uf0.sml.o') or a.endswith('uf0.sml.o'));p=Path(row['args'][idx]);p=p if p.is_absolute() else Path(row['cwd'])/p;text=p.with_suffix('.s').read_text()
call=r'\tsub sp, sp, #48\n\tmov x16, x0\n(?:(?!\tsub sp,)[^\n]*\n)*?\tbl ___mod_word63\n(?:[^\n]*\n)*?\tadd sp, sp, #48\n'
mul=r'\tmovz x16, #33615, lsl #0\n\tlsr x16, x16, #1\n\tubfx x16, x16, #0, #63\n\tsub sp, sp, #16\n\tstr x16, \[sp, #0\]\n\tlsr x17, x1, #1\n\tubfx x17, x17, #0, #63\n\tldr x16, \[sp, #0\]\n\tadd sp, sp, #16\n\tmul x16, x16, x17\n\tubfx x16, x16, #0, #63\n\tlsl x16, x16, #1\n\tadd x16, x16, #1\n\tmov x1, x16\n'
replacement='\tmovz x16, #16807, lsl #0\n\tlsr x17, x1, #1\n\tmul x16, x16, x17\n\tlsl x16, x16, #1\n\tadd x16, x16, #1\n\tmov x1, x16\n'
stats={}
for variant in ['call','mul','both']:
 s=text;nc=nm=0
 if variant in ['call','both']:s,nc=re.subn(call,'\tbl ___mod_word63\n',s);assert nc==3,nc
 if variant in ['mul','both']:s,nm=re.subn(mul,replacement,s);assert nm==3,nm
 asm=r/('uf-'+variant+'.s');asm.write_text(s);obj=asm.with_suffix('.o')
 subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(obj)],check=True)
 args=row['args'][:];args[idx]=str(obj);args[args.index('-o')+1]=str(r/('uf-'+variant+'.exe'))
 subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True)
 stats[variant]={'call_sites':nc,'multiply_sites':nm}
(r/'uf-experiments.json').write_text(json.dumps(stats,indent=2)+'\n')

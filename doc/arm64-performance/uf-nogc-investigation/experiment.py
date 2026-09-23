from pathlib import Path
import json,re,subprocess
r=Path('/private/tmp/mlkit-uf-nogc');rows=[json.loads(x) for x in (r/'links.jsonl').read_text().splitlines()];row=next(x for x in rows if x['arch']=='arm64');args=row['args'];idx=next(i for i,a in enumerate(args) if a.endswith('uf0.sml.o'));p=Path(args[idx]);p=p if p.is_absolute() else Path(row['cwd'])/p;source=p.with_suffix('.s').read_text()
pattern=r'\tsub sp, sp, #48\n\tmov x16, x0\n\tstr x16, \[sp, #0\]\n\tmov x16, x1\n\tstr x16, \[sp, #8\]\n\tmov x16, x2\n\tstr x16, \[sp, #16\]\n\tmov x16, x3\n\tstr x16, \[sp, #24\]\n\tldp x0, x1, \[sp, #0\]\n\tldp x2, x3, \[sp, #16\]\n\tbl ___mod_word64ub\n\tadd sp, sp, #48\n'
for variant in ['direct','result','inline']:
 s,n=re.subn(pattern,'\tbl ___mod_word64ub\n',source);assert n==3,n
 if variant=='result':
  seq='\tbl ___mod_word64ub\n\tsub sp, sp, #16\n\tstr x0, [sp, #0]\n\tmov x16, x0\n\tadd sp, sp, #16\n';assert s.count(seq)==3;s=s.replace(seq,'\tbl ___mod_word64ub\n\tmov x16, x0\n')
 if variant=='inline':s=s.replace('\tbl ___mod_word64ub\n','\tudiv x16, x1, x2\n\tmsub x0, x16, x2, x1\n')
 asm=r/(variant+'.s');asm.write_text(s);obj=r/(variant+'.o');subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(obj)],check=True)
 new=args[:];new[idx]=str(obj);new[new.index('-o')+1]=str(r/(variant+'.exe'));subprocess.run(['gcc','-arch','arm64']+new,cwd=row['cwd'],check=True)

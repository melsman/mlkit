from pathlib import Path
import re,json,subprocess
r=Path('/private/tmp/mlkit-simple-mpuz');row=next(json.loads(x) for x in (r/'links.jsonl').read_text().splitlines() if str(r/'mpuz-arm64.exe') in x);args=row['args'];idx=next(i for i,a in enumerate(args) if a.endswith('/mpuz.sml.o'))
for variant,base in [('branch','local-loop'),('branch-all','all')]:
 s=(r/('mpuz-'+base+'.s')).read_text();m=re.search(r'^(_F\.app15_[^:]+):\n[\s\S]*?(?=^\.text)',s,re.M);body=m[0]
 prefix='L_diag_app_loop:\n\tstr x0, [sp, #24]\n';assert prefix in body;body=body.replace(prefix,'L_diag_app_loop:\n')
 pat=r'\tmov x3, x1\n\tldp x1, x16, \[x3, #0\]\n\tstr x16, \[sp, #16\]\n\tldp x5, x16, \[x1, #0\]\n\tstr x16, \[sp, #8\]\n\tldr x1, \[x16, #0\]\n\tcmp x1, #3\n\tb.ne (L_\w+)\n\tmovz x16, #1, lsl #0\n\tmov x1, x16\n\tb L_\w+\n\1:\n'
 def repl(m):return '\tmov x3, x1\n\tldp x1, x16, [x3, #0]\n\tldp x5, x16, [x1, #0]\n\tldr x1, [x16, #0]\n\tcmp x1, #3\n\tb.ne '+m[1]+'\n\tldr x1, [x3, #8]\n\tb L_diag_app_loop\n'+m[1]+':\n\tstr x0, [sp, #24]\n\tstr x16, [sp, #8]\n\tldr x16, [x3, #8]\n\tstr x16, [sp, #16]\n'
 body,n=re.subn(pat,repl,body);assert n==1;s=s[:m.start()]+body+s[m.end():];key='mpuz-'+variant;asm=r/(key+'.s');asm.write_text(s);obj=r/(key+'.o');subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(obj)],check=True)
 new=args[:];new[idx]=str(obj);new[new.index('-o')+1]=str(r/(key+'.exe'));subprocess.run(['gcc','-arch','arm64']+new,cwd=row['cwd'],check=True)
s=(r/'measure.py').read_text().replace("[('simple',['x64','arm64','loops']),('mpuz',['x64','arm64','loops','arithmetic','both'])]","[('mpuz',['x64','arm64','branch','branch-all'])]").replace("r/'measurements.json'","r/'branch-measurements.json'");(r/'measure-branch.py').write_text(s)

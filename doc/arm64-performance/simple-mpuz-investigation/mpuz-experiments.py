from pathlib import Path
import subprocess,re,json
r=Path('/private/tmp/mlkit-simple-mpuz');row=next(json.loads(x) for x in (r/'links.jsonl').read_text().splitlines() if str(r/'mpuz-arm64.exe') in x);args=row['args'];idx=next(i for i,a in enumerate(args) if a.endswith('/mpuz.sml.o'));p=Path(args[idx]);p=p if p.is_absolute() else Path(row['cwd'])/p;source=p.with_suffix('.s').read_text();stats={}
for variant in ['local-loop','stores','store-loop','all']:
 s=(r/'mpuz-arithmetic.s').read_text() if variant=='all' else source
 m=re.search(r'^(_F\.app15_[^:]+):\n[\s\S]*?(?=^\.text)',s,re.M);body=m[0];label=m[1];nr=nu=0
 if variant in ['local-loop','store-loop','all']:
  head='\tsub sp, sp, #32\n';assert body.count(head)==1;body=body.replace(head,head+'L_diag_app_loop:\n')
  tail='\tsub sp, sp, #16\n\tldp x29, x30, [sp, #48]\n\tadd sp, sp, #48\n\tb '+label+'\n';assert body.count(tail)==1;body=body.replace(tail,'\tb L_diag_app_loop\n')
 if variant in ['stores','store-loop','all']:
  pattern=r'\tsub sp, sp, #16\n\tldr x16, \[sp, #24\]\n\tstr x16, \[sp, #0\]\n\tmov x16, x1\n\tldr x17, \[sp, #0\]\n\tstr x16, \[x17, #0\]\n\tadd sp, sp, #16\n'
  body,nr=re.subn(pattern,'\tldr x17, [sp, #8]\n\tmov x16, x1\n\tstr x16, [x17, #0]\n',body);assert nr==2
  pattern='\tsub sp, sp, #16\n\tstr x17, [sp, #0]\n\tmov x16, x5\n\tldr x17, [sp, #0]\n\tadd sp, sp, #16\n\tstr x16, [x17, #8]\n';assert body.count(pattern)==1;body=body.replace(pattern,'\tmov x16, x5\n\tstr x16, [x17, #8]\n');nu=1
 s=s[:m.start()]+body+s[m.end():];key='mpuz-'+variant;asm=r/(key+'.s');asm.write_text(s);obj=r/(key+'.o');subprocess.run(['gcc','-arch','arm64','-c',str(asm),'-o',str(obj)],check=True)
 new=args[:];new[idx]=str(obj);new[new.index('-o')+1]=str(r/(key+'.exe'));subprocess.run(['gcc','-arch','arm64']+new,cwd=row['cwd'],check=True);stats[variant]={'reference_stores':nr,'array_stores':nu}
(r/'mpuz-experiments.json').write_text(json.dumps(stats,indent=2)+'\n')
s=(r/'measure.py').read_text().replace("[('simple',['x64','arm64','loops']),('mpuz',['x64','arm64','loops','arithmetic','both'])]","[('mpuz',['x64','arm64','local-loop','stores','store-loop','all'])]").replace("r/'measurements.json'","r/'mpuz-measurements.json'");(r/'measure-mpuz.py').write_text(s)

from pathlib import Path
import shutil,subprocess,json
r=Path('/private/tmp/mlkit-outliers');src=Path('/private/tmp/mlkit-m2-source/src/Runtime');dest=r/'runtime-request'
dest.mkdir(exist_ok=True)
for p in src.glob('*.h'): shutil.copy2(p,dest/p.name)
(dest/'config.h').write_text((dest/'config.h').read_text().replace('#define DARWIN_NATIVE 0','#define DARWIN_NATIVE 1'))
s=(src/'Region.c').read_text(encoding='latin1');s=s.replace('(!disable_gc) && (!time_to_gc)','(!time_to_gc)').replace('(!disable_gc) && (lobjs_current>lobjs_gc_treshold)','(lobjs_current>lobjs_gc_treshold)')
(dest/'Region.c').write_text(s,encoding='latin1')
subprocess.run(['gcc','-arch','arm64','-O2','-g','-Wall','-std=gnu99','-fPIC','-iquote',str(src),'-include',str(dest/'Target.h'),'-DTAG_VALUES','-DTAG_FREE_PAIRS','-DENABLE_GC','-c',str(dest/'Region.c'),'-o',str(dest/'Region.o')],check=True)
a=dest/'runtimeSystemGC.a';shutil.copy2('/private/tmp/mlkit-m2-source/lib/darwin-arm64/runtimeSystemGC.a',a)
subprocess.run(['ar','r',str(a),str(dest/'Region.o')],check=True)
rows=list(map(json.loads,(r/'links.jsonl').read_text().splitlines()));row=next(x for x in rows if str(r/'dlx-arm64.exe') in x['args'])
args=row['args'][:];args[args.index('-o')+1]=str(r/'dlx-request.exe');args=[str(a) if x.endswith('/runtimeSystemGC.a') else x for x in args]
subprocess.run(['gcc','-arch','arm64']+args,cwd=row['cwd'],check=True)

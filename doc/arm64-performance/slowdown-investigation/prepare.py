from pathlib import Path
import subprocess
root=Path('/private/tmp/mlkit-m12-diagnosis')
s=Path('src/Runtime/Arm64GC.c').read_text()
# Exact semantics: fast rejection only; all pointers inside enclosing bounds
# still undergo the original membership test. Bounds updated on registration
# and unregistration, keeping support for disjoint REPL images.
p=s.replace('static Image *images;', '''static Image *images;
static uintptr_t static_low=UINTPTR_MAX, static_high=0;
static void bounds(void) {
  static_low=UINTPTR_MAX; static_high=0;
  for (Image *p=images;p;p=p->next) {
    if (p->begin<static_low) static_low=p->begin;
    if (p->end>static_high) static_high=p->end;
  }
}''')
p=p.replace('p->next=images; images=p;', 'p->next=images; images=p; bounds();')
p=p.replace('*link=p->next; free(p);', '*link=p->next; free(p); bounds();')
p=p.replace('uintptr_t v=(uintptr_t)ptr;', 'uintptr_t v=(uintptr_t)ptr;\n  if(v<static_low || v>=static_high) return 0;')
(root/'Arm64GC-fast.c').write_text(p)
c=s.replace('static Image *images;', '''static Image *images;
static unsigned long long calls, probes, hits, registrations;
__attribute__((destructor)) static void report_probes(void) {
 fprintf(stderr,"static lookups=%llu probes=%llu hits=%llu registrations=%llu\\n",calls,probes,hits,registrations);
}''')
c=c.replace('p->next=images; images=p;', 'p->next=images; images=p; registrations++;')
c=c.replace('uintptr_t v=(uintptr_t)ptr;', 'uintptr_t v=(uintptr_t)ptr; calls++;')
c=c.replace('for(Image *p=images;p;p=p->next) if(v>=p->begin && v<p->end) return 1;', 'for(Image *p=images;p;p=p->next) { probes++; if(v>=p->begin && v<p->end) { hits++; return 1; } }')
(root/'Arm64GC-count.c').write_text(c)
for kind in ['fast','count']:
 subprocess.run(['gcc','-arch','arm64','-O2','-g','-iquote','src/Runtime','-c',str(root/('Arm64GC-'+kind+'.c')),'-o',str(root/('Arm64GC-'+kind+'.o'))],check=True)
# Original unit-test checks static membership, unregister, and root relocation.
subprocess.run(['gcc','-arch','arm64','-O2','-iquote','src/Runtime',str(root/'Arm64GC-fast.c'),'src/Runtime/tests/arm64-gc-metadata.c','-o',str(root/'gc-metadata-fast')],check=True)
subprocess.run([str(root/'gc-metadata-fast')],check=True)

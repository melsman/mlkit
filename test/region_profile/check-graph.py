#!/usr/bin/env python3
"""Exercise actual HTML graph code with exact accounting/filtering fixtures (Node)."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('reader',root/'test/region_profile/reference-reader.py')
reader = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reader)
viewer = os.environ.get("RPVIEW",str(root/"bin/rpview"))
HUGE = 2**60+1

def region(thread, worker, cpu, unit, binding, large, finite=0):
    return dict(type='region', sample=1, thread=thread, worker=worker, cpu=cpu,
                unit=unit,binding=binding,name='same </script> name',kind='finite' if finite else 'infinite',
                pages=0,unused_tail=0,page_footprint=0,large_bytes=large,finite_bytes=finite,descriptor_bytes=0)

rows = [region(1,0,4,'A',1,HUGE),region(1,0,4,'A',1,3),  # recursive instances
        region(2,1,5,'B',1,16,8),region(1,0,4,'<global>',0,32)]
stack = [dict(type='stack',sample=1,thread=1,worker=0,cpu=4,active_bytes=64,finite_bytes=0,stack_bytes=64),
         dict(type='stack',sample=1,thread=2,worker=1,cpu=5,active_bytes=40,finite_bytes=8,stack_bytes=32)]
header = dict(type='header',format='mlkit-region-profile',version=3,page_bytes=8192)
records = [header]
for i in (1,2):
    records.append(dict(type='sample_begin',sample=i,time=i*1000000,reason='explicit'))
    for r in rows+stack:
        r = dict(r,sample=i)
        if i==2 and r['thread']==1: r['cpu']=5  # migration, not allocation origin
        records.append(r)
    records.append(dict(type='sample_end',sample=i,time=i*1000000+1,frames=2,pages_visited=0))
records.append(dict(type='session_end',time=3000000,max_pages=123))
wire = ''.join(json.dumps(r)+'\n' for r in records)
samples = list(reader.read_samples(wire.splitlines(keepends=True)))
assert len(samples)==2
assert len(list(reader.read_samples((wire+'{"type":').splitlines(keepends=True))))==2
assert len(list(reader.read_samples(wire[:wire.rfind('{"type": "sample_end"')].splitlines(keepends=True))))==1
bad = copy.deepcopy(records)
next(r for r in bad if r['type']=='stack')['stack_bytes']+=1
try:
    list(reader.read_samples([json.dumps(r)+'\n' for r in bad]))
    raise AssertionError('invalid stack accepted')
except ValueError as e:
    assert 'stack accounting' in str(e)
with tempfile.TemporaryDirectory(prefix='rp-graph-') as directory:
    profile = Path(directory)/'profile.rp'
    profile.write_text(wire)
    output = Path(directory)/'profile.html'
    subprocess.run([viewer,str(profile),'--output',str(output)],check=True,capture_output=True,
                   env=dict(os.environ,PATH='/nonexistent'))
    html = output.read_text()
    embedded = json.loads(html.split('const samples=')[1].split(';\n')[0])
    def exact(v):
        if type(v) is int: return str(v)
        if isinstance(v,list): return [exact(x) for x in v]
        if isinstance(v,dict): return {k:exact(x) for k,x in v.items()}
        return v
    assert [{k:v for k,v in s.items() if k!='marks'} for s in embedded] == exact(samples)
    assert html.count('<script>')==1 and '\\u003c/script>' in html
    script = html.split('<script>')[1].split('</script>')[0].replace('const samples=','let samples=',1)
    driver = r'''
const assert=require('assert');
class Element {
 constructor(tag=''){this.tag=tag;this.children=[];this.attrs={};this.style={};this.value='';this.textContent='';}
 setAttribute(k,v){this.attrs[k]=v;}
 append(...nodes){this.children.push(...nodes);}
 replaceChildren(...nodes){this.children=nodes;}
 addEventListener(){}
}
const elements=new Map();
const document={getElementById:id=>{if(!elements.has(id))elements.set(id,new Element());return elements.get(id);},createElement:tag=>new Element(tag),createElementNS:(ns,tag)=>new Element(tag),querySelectorAll:()=>[]};
for(const [id,value] of [['metric','total'],['scope','all'],['group','aggregate'],['sample','0']])document.getElementById(id).value=value;
for(const id of ['show-base','show-kind','show-peak'])document.getElementById(id).checked=true;
'''+script+r'''
assert.equal(samples[0].max_pages,'123');
samples[0].regions[0].pages='2';samples[0].regions[0].unused_tail='16384';
el('metric').value='pages';draw();
assert.deepStrictEqual(model().totals,[16384n,0n]);
samples[0].regions[0].pages='0';samples[0].regions[0].unused_tail='0';
assert(el('chart').children.some(n=>n.attrs['data-peak']==='pages'&&n.attrs.y1===n.attrs.y2));
assert(el('chart').children.some(n=>n.textContent==='Memory (KiB)'));
for(const metric of ['pages','page_footprint','total']){
 el('metric').value=metric;draw();
 const line=el('chart').children.find(n=>n.attrs['data-peak']==='pages');assert(line);
 assert.equal(line.attrs.y1,line.attrs.y2);
 if(metric!=='total'){assert.equal(line.attrs.y1,'65');assert(el('chart').children.some(n=>n.textContent.includes('984 KiB (123 pages)')));}
 assert(el('chart').children.some(n=>n.textContent.includes('123 pages')));
 el('scope').value='thread:1';draw();assert(!el('chart').children.some(n=>n.attrs['data-peak']));el('scope').value='all';
}
el('metric').value='large_bytes';draw();assert(!el('chart').children.some(n=>n.attrs['data-peak']));
el('scope').value='all';el('metric').value='total';el('limit').value='1';draw();
assert.equal(model().bands.length,3); // largest region, stack, Other
assert.equal(model().bands[0].key,'other');
assert.equal(model().bands[0].values[0],56n);
assert.deepStrictEqual(el('legend').children.map(n=>n.title),model().bands.map(b=>b.label).reverse());
el('limit').value='0';draw();
el('show-type').checked=true;
assert(label(samples[0].regions[0]).includes('type unavailable'));
for(const type of ['top','bot','pair','triple','string','array','ref'])assert(label({...samples[0].regions[0],region_type:type}).endsWith('(infinite, '+type+')'));
el('show-kind').checked=false;assert(label({...samples[0].regions[0],region_type:'pair'}).endsWith('(pair)'));
el('show-kind').checked=true;el('show-type').checked=false;
el('legend-right').checked=true;draw();assert.equal(el('graph-layout').className,'graph-layout legend-right');
assert(label({...samples[0].regions[0],name:'',binding:'5'}).startsWith('r5 · '));
el('legend-right').checked=false;draw();assert.equal(el('graph-layout').className,'graph-layout');
assert(label({...samples[0].regions[0],name:'',binding:'5'}).startsWith('Region #5 · '));
const beforeLabels=model().totals.slice(),beforeKeys=model().bands.map(b=>b.key);
assert(label(samples[0].regions[2]).endsWith('(finite)'));
assert(label({...samples[0].regions[2],finite_bytes:'0'}).endsWith('(finite)'));
assert(label(samples[0].regions[0]).endsWith('(infinite)'));
assert(label(samples[0].regions[3]).includes(' · global'));
el('show-base').checked=false;el('show-kind').checked=false;el('group').value='region';draw();
assert(!label(samples[0].regions[3]).includes('global'));
assert(!label(samples[0].regions[0]).includes('(infinite)'));
// A and B have identical display names with base names and kinds hidden.
assert.equal(label(samples[0].regions[0]),label(samples[0].regions[2]));
assert.equal(el('rows').children.length,4); // distinct bindings still have distinct rows
assert.deepStrictEqual(model().totals,beforeLabels);assert.deepStrictEqual(model().bands.map(b=>b.key),beforeKeys);
el('show-peak').checked=false;draw();assert(!el('chart').children.some(n=>n.attrs['data-peak']));assert.equal(el('peak-note').textContent,'');
el('show-peak').checked=true;draw();assert(el('chart').children.some(n=>n.attrs['data-peak']));
el('show-base').checked=true;el('show-kind').checked=true;el('group').value='aggregate';draw();
const huge=2n**60n+1n;
assert.deepStrictEqual(model().totals,[huge+155n,huge+155n]);
assert.equal(model().bands.length,4); // A, B, global, stack; duplicate names not merged
const ordered=model().bands;assert(ordered.every((b,i)=>!i||ordered[i-1].sum<=b.sum));
const colors=new Map(ordered.map(b=>[b.key,bandColor(b.key)]));
function check(scope,expected){el('scope').value=scope;draw();assert.deepStrictEqual(model().totals,expected);assert(model().bands.every(b=>colors.get(b.key)===bandColor(b.key)));}
check('thread:1',[huge+99n,huge+99n]);check('thread:2',[56n,56n]);
check('worker:0',[huge+99n,huge+99n]);check('worker:1',[56n,56n]);
check('cpu:4',[huge+99n,0n]);check('cpu:5',[56n,huge+155n]);
check('cpu:-1',[0n,0n]);check('all',[huge+155n,huge+155n]);
assert(el('chart').children.some(n=>n.textContent==='Elapsed time (s)'));
assert(el('chart').children.some(n=>n.textContent==='Memory (EiB)'));
assert(el('chart').children.filter(n=>n.tag==='polygon').length===4);
el('metric').value='large_bytes';draw();assert.deepStrictEqual(model().totals,[huge+51n,huge+51n]);
el('metric').value='total';samples=[samples[0]];draw();assert(!el('chart').children.some(n=>Object.values(n.attrs).some(v=>/NaN|Infinity/.test(v))));
samples[0].stacks=null;draw();assert(el('stack-note').textContent.includes('unavailable'));assert(!model().bands.some(b=>b.key==='stack'));
samples=[];draw();assert(el('caption').textContent.includes('No completed'));
console.log('Stacked graph: exact sums, ordering, colors, filters/migration, units, truncation, old/empty/single samples passed');
'''
    subprocess.run(['node','-'],input=driver,text=True,check=True)
# A runtime fixture validates bytes independently of the JS model.
if len(sys.argv)>1:
    with open(sys.argv[1]) as stream: actual=list(reader.read_samples(stream))
    assert [s['stacks'][0]['active_bytes'] for s in actual]==[1056,512,512,512]
    assert [s['stacks'][0]['stack_bytes'] for s in actual]==[1008,488,488,488]
    assert [s['stacks'][0]['finite_bytes'] for s in actual]==[48,24,24,24]
    print('Exact runtime frame spans, spilled-result gap, and finite subtraction passed')

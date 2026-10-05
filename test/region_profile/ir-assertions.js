// Run against the same viewer script as the report. Use byte offsets with a
// non-ASCII prefix and HTML-looking code to catch UTF-16 and escaping mistakes.
const irOriginal={allocations:profile.allocations,session:profile.allocation_session};
const irCode='val café = "</script><img>"\nval pair = record attop r42 (1,2)\nval other = record attop r42 (3,4)\n';
const irText='header\n'+irCode,irEncoder=new TextEncoder(),irStart=irEncoder.encode('header\n').length;
const irBytes=irEncoder.encode(irText),irNeedle=irEncoder.encode('attop r42');
const irOffsets=[irText.indexOf('attop r42'),irText.lastIndexOf('attop r42')].map(i=>irEncoder.encode(irText.slice(0,i)).length);
const irSpans=irOffsets.map((start,i)=>({start:String(start),length:String(irNeedle.length),line:String(i+3),column:'19'}));
irDocuments.set('fixture-ir',{identity:'fixture-ir',text:irText,code_start:String(irStart),code_bytes:String(irEncoder.encode(irCode).length)});
irSites.set('701',{definition:'701',identity:'fixture-ir',status:'available',spans:irSpans});
irSites.set('702',{definition:'702',status:'missing-or-mismatched-ir',spans:[]});
profile.allocation_session={enabled:'1',selector:'unit:42',build_id:'ir-test'};
const irRecord={definition:'701',unit:'unit',function:'build',source:'/source/main.sml',site:'11',location_kind:'0',count:'3',bytes:'48'};
profile.allocations=[irRecord,{...irRecord,thread:'1',count:'2',bytes:'32'},{...irRecord,definition:'702',site:'12',count:'1',bytes:'16'}];
el('allocation-group').value='function';el('show-base').checked=false;allocationTable();
const irGroup=el('allocation-rows').children[0];
assert.equal(irGroup.querySelectorAll('summary')[0].textContent,'build · 2 sites');
assert.equal(irGroup.querySelectorAll('button').length,2);
assert.equal(irGroup.children[2].textContent,'6');
assert.equal(irGroup.querySelectorAll('li')[0].children[1].textContent,'5 allocations · 80 bytes');
irGroup.querySelectorAll('button')[0].listeners.click();
assert.equal(el('ir-panel').hidden,false);assert(el('ir-title').focused);
assert.equal(el('ir-code').querySelectorAll('mark')[0].textContent,'attop r42');
assert.equal(el('ir-location').children.length,2);
assert.equal(el('ir-code').querySelectorAll('img').length,0);
el('ir-location').value='1';el('ir-location').listeners.change();
assert(el('ir-message').textContent.includes('line 4'));
assert.equal(el('ir-code').querySelectorAll('mark')[0].textContent,'attop r42');
el('ir-full').checked=true;el('ir-full').listeners.change();assert(el('ir-message').textContent.includes('Full IR'));
el('allocation-group').value='site';allocationTable();
assert.equal(el('allocation-rows').children.length,2);
el('allocation-rows').children[1].querySelectorAll('button')[0].listeners.click();
assert(el('ir-message').textContent.includes('missing, changed, or from another build'));
assert.equal(el('ir-code').children.length,0);assert.equal(el('ir-controls').hidden,true);
for(const [status,reason] of [['generated','generated'],['legacy-profile','without IR location'],['missing-mark','does not contain']]){
 irSites.set('702',{status});showIR({...irRecord,definition:'702'},null);assert(el('ir-message').textContent.includes(reason));
}
showIR({...irRecord,definition:'unknown'},null);assert(el('ir-message').textContent.includes('without IR location'));
showIR({...irRecord,location_kind:'1'},null);assert(el('ir-message').textContent.includes('Initiating foreign call'));
const irTrigger=irButton(irRecord,'test');irTrigger.listeners.click();el('ir-close').listeners.click();assert(el('ir-panel').hidden);assert(irTrigger.focused);
// Tampered embedded spans fail closed without rendering a misleading highlight.
irSites.set('703',{identity:'fixture-ir',status:'available',spans:[{start:'999999',length:'8'}]});
showIR({...irRecord,definition:'703'},null);assert(el('ir-message').textContent.includes('invalid'));
profile.allocations=irOriginal.allocations;profile.allocation_session=irOriginal.session;
showIR(irRecord,null);el('show-base').checked=true;el('show-base').listeners.input();assert(el('ir-title').textContent.includes('main.sml'));
el('show-base').checked=false;el('show-base').listeners.input();assert(!el('ir-title').textContent.includes('main.sml'));
// Surrounding context stays bounded; full IR includes every line.
const longCode=Array.from({length:60},(_,i)=>i===30?'val x = attop r9':'line '+i).join('\n');
const longStart=irEncoder.encode(longCode.slice(0,longCode.indexOf('attop'))).length;
irDocuments.set('long-ir',{identity:'long-ir',text:longCode,code_start:'0',code_bytes:String(irEncoder.encode(longCode).length)});
irSites.set('704',{identity:'long-ir',status:'available',spans:[{start:String(longStart),length:'8',line:'31',column:'9'}]});
el('ir-full').checked=false;showIR({...irRecord,definition:'704'},null);assert.equal(el('ir-code').children.length,17);
el('ir-full').checked=true;el('ir-full').listeners.change();assert.equal(el('ir-code').children.length,60);

showIR(irRecord,{isConnected:false});el('ir-close').listeners.click();assert(el('allocation-group').focused);

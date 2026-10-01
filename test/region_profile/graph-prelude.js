
function assert(value,message='Assertion failed'){if(!value)throw new Error(message);}
assert.equal=(a,b)=>assert(a==b,`${a} != ${b}`);
assert.deepStrictEqual=(a,b)=>assert(JSON.stringify(a,(_,v)=>typeof v==='bigint'?{bigint:v.toString()}:v)===JSON.stringify(b,(_,v)=>typeof v==='bigint'?{bigint:v.toString()}:v));
assert.throws=(f,pattern)=>{let error;try{f();}catch(e){error=e;}assert(error&&pattern.test(error.message));};
class Element {
 constructor(tag=''){this.tag=tag;this.children=[];this.attrs={};this.style={};this.value='';this.textContent='';}
 setAttribute(k,v){this.attrs[k]=v;}
 getAttribute(k){return this.attrs[k]??null;}
 removeAttribute(k){delete this.attrs[k];}
 querySelectorAll(tag){return this.children.flatMap(n=>[...(n.tag===tag?[n]:[]),...n.querySelectorAll(tag)]);}
 cloneNode(deep){const n=new Element(this.tag);n.attrs={...this.attrs};n.textContent=this.textContent;if(deep)n.children=this.children.map(c=>c.cloneNode(true));return n;}
 getContext(){return {font:'',measureText(text){return {width:[...text].length*parseFloat(this.font)*.7};}};}
 append(...nodes){this.children.push(...nodes);}
 replaceChildren(...nodes){this.children=nodes;}
 addEventListener(){}
}
const elements=new Map();
const document={getElementById:id=>{if(!elements.has(id))elements.set(id,new Element());return elements.get(id);},createElement:tag=>new Element(tag),createElementNS:(ns,tag)=>new Element(tag),querySelectorAll:()=>[]};
for(const [id,value] of [['metric','total'],['scope','all'],['group','aggregate'],['sample','0']])document.getElementById(id).value=value;
document.getElementById('chart').tag='svg';document.getElementById('chart').setAttribute('viewBox','0 0 1002 668');
for(const id of ['show-base','show-kind','show-peak'])document.getElementById(id).checked=true;

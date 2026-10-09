// Small, dependency-free DOM for testing the report's real markup and script.
function assert(value,message='Assertion failed'){if(!value)throw new Error(message);}
assert.equal=(a,b)=>assert(a==b,`${a} != ${b}`);
assert.deepStrictEqual=(a,b)=>assert(JSON.stringify(a,(_,v)=>typeof v==='bigint'?{bigint:v.toString()}:v)===JSON.stringify(b,(_,v)=>typeof v==='bigint'?{bigint:v.toString()}:v));
assert.throws=(f,pattern)=>{let error;try{f();}catch(e){error=e;}assert(error&&pattern.test(error.message));};
class Element {
 constructor(tag=''){this.tag=tag;this.children=[];this.attrs={};this.style={setProperty(k,v){this[k]=v;}};this._value='';this._text='';this.listeners={};this.isConnected=true;}
 get textContent(){return this._text+this.children.map(n=>n.textContent).join('');}
 set textContent(value){this.replaceChildren();this._text=String(value);}
 get firstChild(){return this.children[0]||null;}
 get nodeType(){return this.tag?1:3;}
 get id(){return this.attrs.id||'';}
 set id(value){this.attrs.id=value;}
 get className(){return this.attrs.class||'';}
 set className(value){this.attrs.class=value;}
 get type(){return this.attrs.type||'';}
 get options(){return this.tag==='select'?this.querySelectorAll('option'):[];}
 get value(){return this.tag==='select'?this.options.find(o=>o.selected)?.value||'':this._value;}
 set value(value){value=String(value);if(this.tag==='select'){for(const o of this.options)o.selected=o.value===value;}else this._value=value;}
 get selectedOptions(){return this.options.filter(o=>o.selected);}
 setAttribute(k,v){this.attrs[k]=String(v);if(k==='value')this.value=v;else if(['checked','selected','hidden','disabled'].includes(k))this[k]=true;}
 getAttribute(k){return this.attrs[k]??null;}
 removeAttribute(k){delete this.attrs[k];}
 matches(selector){const match=selector.match(/^([\w-]+)?(?:#([\w-]+))?(?:\.([\w-]+))?$/);return !!match&&(!match[1]||this.tag===match[1])&&(!match[2]||this.id===match[2])&&(!match[3]||this.className.split(/\s+/).includes(match[3]));}
 querySelectorAll(selector){return this.children.flatMap(n=>[...(n.matches(selector)?[n]:[]),...n.querySelectorAll(selector)]);}
 querySelector(selector){return this.querySelectorAll(selector)[0]||null;}
 closest(selector){for(let n=this;n;n=n.parentElement)if(n.matches(selector))return n;return null;}
 cloneNode(deep){const n=new Element(this.tag);n.attrs={...this.attrs};n._text=this._text;n._value=this._value;if(deep)n.append(...this.children.map(c=>c.cloneNode(true)));return n;}
 getContext(){return {font:'',measureText(text){return {width:[...text].length*parseFloat(this.font)*.7};}};}
 get tagName(){return this.tag.toUpperCase();}
 scrollIntoView(){this.scrolled=true;}
 remove(){if(this.parentElement){const children=this.parentElement.children;children.splice(children.indexOf(this),1);this.parentElement=null;}}
 append(...nodes){for(let n of nodes){if(typeof n==='string')n=document.createTextNode(n);n.remove();n.parentElement=this;this.children.push(n);if(this.tag==='select'&&n.tag==='option'&&this.options.length===1)n.selected=true;}}
 prepend(...nodes){this.append(...nodes);this.children.unshift(...this.children.splice(this.children.length-nodes.length));}
 before(...nodes){const p=this.parentElement;if(!p)return;for(const n of nodes){n.remove();n.parentElement=p;p.children.splice(p.children.indexOf(this),0,n);}}
 after(...nodes){const p=this.parentElement;if(!p)return;let previous=this;for(const n of nodes){n.remove();n.parentElement=p;p.children.splice(p.children.indexOf(previous)+1,0,n);previous=n;}}
 replaceChildren(...nodes){this._text='';for(const n of this.children)n.parentElement=null;this.children=[];this.append(...nodes);}
 addEventListener(type,listener){const previous=this.listeners[type];this.listeners[type]=(...args)=>{if(previous)previous(...args);listener(...args);};}
 focus(){this.focused=true;}
 showPopover(){this.popoverOpen=true;this.listeners.toggle?.({newState:'open'});}
 hidePopover(){this.popoverOpen=false;this.listeners.toggle?.({newState:'closed'});}
}
const document={body:new Element('body'),getElementById:id=>document.body.querySelector('#'+id),createTextNode:text=>{const n=new Element();n.textContent=text;return n;},createElement:tag=>new Element(tag),createElementNS:(ns,tag)=>new Element(tag),querySelectorAll:selector=>document.body.querySelectorAll(selector)};
// Parse static markup only; style and script contents are never DOM fixtures.
const decodeHTML=text=>text.replace(/&(?:amp|lt|gt|quot|apos|#(\d+)|#x([\da-f]+));/gi,(entity,decimal,hex)=>decimal||hex?String.fromCodePoint(parseInt(decimal||hex,hex?16:10)):({'&amp;':'&','&lt;':'<','&gt;':'>','&quot;':'"','&apos;':"'"}[entity]||entity));
const voidTags=new Set(['meta','input','br','hr','img','link']),stack=[document.body];
for(const token of viewerMarkup.replace(/<style\b[^>]*>[\s\S]*?<\/style>/gi,'').match(/<[^>]*>|[^<]+/g)||[]){
 if(token.startsWith('<!'))continue;
 if(token.startsWith('</')){const tag=token.slice(2,-1).trim();for(let i=stack.length-1;i>0;i--)if(stack[i].tag===tag){stack.length=i;break;}continue;}
 if(token.startsWith('<')){const match=token.match(/^<([\w-]+)([\s\S]*?)\/?\s*>$/);if(!match)throw Error('Invalid fixture markup: '+token);const n=new Element(match[1]);
  for(const attr of match[2].matchAll(/([^\s=]+)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s]+)))?/g))n.setAttribute(attr[1],decodeHTML(attr[2]??attr[3]??attr[4]??''));
  const selected=n.selected;stack.at(-1).append(n);if(selected&&n.parentElement.tag==='select'){n.parentElement.value=n.value;}
  if(!voidTags.has(n.tag)&&!token.endsWith('/>'))stack.push(n);
 }else stack.at(-1).append(document.createTextNode(decodeHTML(token)));
}
// The graph assertions deliberately exercise enabled display options.
for(const id of ['show-base','show-peak'])document.getElementById(id).checked=true;

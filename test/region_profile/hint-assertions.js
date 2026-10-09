// Initialization must move real report notes into accessible popup containers.
const hints=document.querySelectorAll('.hint-button');
assert(hints.length>=12,'Missing info buttons after report initialization');
for(const button of hints){
 const popup=document.getElementById(button.getAttribute('popovertarget'));
 assert(popup,'Info button must target an existing popup');
 assert.equal(popup.getAttribute('popover'),'auto');
 assert.equal(button.getAttribute('popovertargetaction'),'toggle');
 assert(popup.textContent.trim().length>0,'Info popup must retain its explanatory text');
}
assert(el('gc-note').closest('.hint-popup'),'GC note must remain available in its popup');
assert(el('limit-help').closest('.hint-popup'),'Limit help must move with its ID intact');
assert.equal(el('allocation-value').value,'bytes');
assert.equal(el('ir-context').type,'radio');assert.equal(el('ir-full').type,'radio');
console.log('Report markup and info popup initialization: PASS');

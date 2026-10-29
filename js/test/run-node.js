/* Execute compiler-generated local scripts in their HTML load order. */
const fs = require('fs'), path = require('path'), vm = require('vm');
const html = path.resolve(process.argv[2]);
global.window = global;
global.document = {write: s => process.stdout.write(String(s))};
if (process.env.SMLTOJS_NO_BIGINT === '1') global.BigInt = undefined;
for (const match of fs.readFileSync(html,'utf8').matchAll(/<script[^>]*src="([^"]+)"/g)) {
  const file = path.resolve(path.dirname(html),match[1]);
  vm.runInThisContext(fs.readFileSync(file,'utf8'),{filename:file});
}

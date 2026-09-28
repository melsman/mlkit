// Partition one compiler-generated manifest so shared runtime files execute once.
import { readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
const directory = process.argv[2];
const html = readFileSync(join(directory, 'index.html'), 'utf8');
const paths = [...html.matchAll(/<script[^>]+src="([^"]+)"[^>]*><\/script>/g)].map(match => match[1]);
const lastUI = paths.findLastIndex(path => /\/SmlToJsCompApp-sml(?:-code\d+)?\.js$/.test(path));
if (lastUI < 0 || lastUI === paths.length - 1) throw new Error('Missing UI/compiler boundary');
const ui = paths.slice(0, lastUI + 1);
const compiler = paths.slice(lastUI + 1);
if (ui.some(path => /\/SmlToJsComp-sml|\/SmlToJsCompiler-sml/.test(path))) {
  throw new Error('Compiler code precedes the UI boundary');
}
const concatenate = files => files.map(path => readFileSync(join(directory, path), 'utf8')).join('\n;\n');
writeFileSync(join(directory, 'bundle.unminified.js'), concatenate(ui));
writeFileSync(join(directory, 'bundle_compiler.unminified.js'), concatenate(compiler));
writeFileSync(join(directory, 'index-split.html'), '<!doctype html><meta charset="utf-8">\n' + html);
console.log(`IDE: ${ui.length} scripts; compiler: ${compiler.length} scripts`);

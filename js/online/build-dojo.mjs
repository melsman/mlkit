// Build the startup layer, retaining the full packages for on-demand features.
import {createHash} from 'node:crypto';
import {execFileSync} from 'node:child_process';
import {cp, mkdir, readFile, rm, writeFile} from 'node:fs/promises';
import {dirname, join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {minify} from 'terser';

const here = dirname(fileURLToPath(import.meta.url));
const version = '1.9.1';
const digest = 'ee67f54a8b8bc202bca8c61bb45e952760bab21b3f6f9307da7e968381deaa56';
const cache = resolve(here, '../.cache', `dojo-${version}`);
const archive = join(cache, 'source.tar.gz');
const source = join(cache, `dojo-release-${version}-src`);
const release = join(cache, 'release');
const profile = join(here, 'dojo.profile.js');
const destination = process.argv[2];
if (!destination) throw new Error('Usage: node online/build-dojo.mjs OUTPUT_DIRECTORY');
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const key = hash(Buffer.concat(await Promise.all([
  readFile(fileURLToPath(import.meta.url)), readFile(profile),
  readFile(resolve(here, '../package-lock.json'))
])));
await mkdir(cache, {recursive: true});
const stamp = join(cache, 'build-key');
if (await readFile(stamp, 'utf8').catch(() => '') !== key) {
  let bytes = await readFile(archive).catch(() => null);
  if (!bytes) {
    console.log(`Downloading Dojo ${version} source and build tools...`);
    const url = `https://download.dojotoolkit.org/release-${version}/dojo-release-${version}-src.tar.gz`;
    const response = await fetch(url);
    if (!response.ok) throw new Error(`Dojo download failed: HTTP ${response.status}`);
    bytes = Buffer.from(await response.arrayBuffer());
  }
  if (hash(bytes) !== digest) throw new Error(`Dojo archive checksum mismatch; remove ${archive} and retry`);
  await writeFile(archive, bytes);
  await rm(source, {recursive: true, force: true});
  await rm(release, {recursive: true, force: true});
  execFileSync('tar', ['-xzf', archive, '-C', cache]);
  console.log('Building Dojo startup layer...');
  // Keep the legacy builder's warnings in a log, but fail on build errors.
  let log;
  try {
    log = execFileSync(process.execPath, [join(source, 'dojo/dojo.js'),
      'load=build', '--profile', profile, `basePath=${source}`, `releaseDir=${release}`],
      {encoding: 'utf8', maxBuffer: 16 * 1024 * 1024});
  } catch (error) {
    await writeFile(join(cache, 'build.log'), String(error.stdout ?? '') + String(error.stderr ?? ''));
    throw new Error(`Dojo build failed; see ${cache}/build.log`, {cause: error});
  }
  await writeFile(join(cache, 'build.log'), log);
  if (!/errors: 0\b/.test(log)) throw new Error(`Dojo build failed; see ${cache}/build.log`);
  const layer = join(release, 'dojo/dojo.js');
  const result = await minify(await readFile(layer, 'utf8'), {
    compress: true, mangle: true, format: {comments: /copyright|license/i}
  });
  await writeFile(layer, result.code + '\n');
  await writeFile(stamp, key);
}
for (const name of ['dojo', 'dijit', 'dojox']) {
  await cp(join(release, name), join(resolve(destination), name), {recursive: true});
}
console.log(`Dojo ${version}: startup layer plus on-demand dojo/dijit/dojox modules`);

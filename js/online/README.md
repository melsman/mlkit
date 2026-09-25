# Building and measuring SMLtoJs Online

From the repository root:

```sh
make smltojs_basislibs
cd js
npm ci
make smltojsonline DOJO=/path/to/dojo-release-1.9.1
python3 -m http.server 8000 --directory smltojsonline
```

Open http://localhost:8000/. The `DOJO` directory must contain `dojo/` and
`dijit/`. Node.js/npm are build-time dependencies only. Terser and its
transitive dependencies are pinned by `package-lock.json`.

`index.html` is copied from this directory. It exposes stylesheets and
preloads the extra Basis bundle and Dojo loader before the main bundle
executes. Preloads do not execute scripts: the application still loads the
CodeMirror mode before Dojo, and loads the extra scripts in dependency
order. CodeMirror must precede Dojo because its UMD wrapper otherwise sees
Dojo's AMD loader.

The HTML body and loading message are preserved until the UI is attached.
Basis initialization waits for the UI and extra scripts, not `window.onload`
(which can also wait for documentation and images). The example index and
example contents load asynchronously with normal HTTP caching. Errors are
reported in the message log without blocking compiler startup.

The build retains `bundle.unminified.js`, `bundle_extra.unminified.js`, and
`index-split.html` for debugging. The deployed entry point uses the minified
`bundle.js` and `bundle_extra.js`. `terser.json` preserves global identifiers
and property names used across separately generated files, dynamically
compiled SML programs, and embedded JavaScript strings. Do not enable
module mode, top-level/property mangling, or unsafe compression without
checking those interfaces. ASCII-only output preserves serialized Basis byte
strings even in legacy generated pages without an explicit UTF-8 charset.

## Startup timing

The application records these User Timing entries:

- `smltojs-ui-ready`: the UI widgets are attached and started.
- `smltojs-basis-start`: Basis environment initialization begins.
- `smltojs-compiler-ready`: the compiler's environment is usable.
- `smltojs-startup`: navigation start to compiler ready.
- `smltojs-basis-init`: Basis initialization duration.

To read seconds in the browser console:

```js
performance.getEntriesByName('smltojs-startup').at(-1).duration / 1000
performance.getEntriesByName('smltojs-basis-init').at(-1).duration / 1000
performance.getEntriesByName('smltojs-ui-ready').at(-1).startTime / 1000
```

Compare five or more runs on the same browser/device and network settings.
Report cold-cache and warm-cache medians separately. Use the browser's
Disable cache setting for cold runs; a query parameter on the HTML alone
does not make its assets cold. In the Network waterfall, the preloaded
scripts should be requested alongside the main bundle, with each body
transferred once. Record transferred (compressed) bytes as well as time.
A localhost result is useful for checking execution and initialization,
but is not a measurement of GitHub Pages download performance.

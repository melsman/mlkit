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

`index.html` exposes stylesheets and preloads Dojo before the UI bundle
executes. CodeMirror loads before Dojo because its UMD wrapper otherwise
sees Dojo's AMD loader.

Startup has three stages:

1. `bundle.js` loads the shared runtime, editor, menus, file tree, and
   documentation pane. It has no dependency on the compiler implementation.
2. After the UI is attached and the browser has an opportunity to paint,
   `loader.js` fetches `bundle_compiler.js` and preloads `bundle_extra.js`.
   It executes the compiler first, then the extra Basis code.
3. The compiler unpickles the Basis environment, yielding between libraries.
   The status indicator changes from loading to initializing to ready.
   Compile clicks before readiness report that the compiler is still loading.
   Download and initialization failures leave the IDE usable and show an error.

The HTML loading message is preserved until the UI is attached. Startup does
not wait for `window.onload` or documentation/images. The example index and
example contents load asynchronously with normal HTTP caching.

`src/Compiler/smltojs-ui.mlb` is evaluated before the compiler in
`smltojs0.mlb`. The UI exports a small, typed callback bridge that the compiler
fills in when loaded. `package.mjs` splits the single generated script list
after the UI application. This preserves generated identifiers, execution
order, and a single copy of the shared runtime. Keep compiler dependencies
out of the UI entry point.

The build retains unminified versions of all three bundles and
`index-split.html` for debugging. The latter loads the individual generated
scripts and is not intended to provide the staged-loading performance of
`index.html`. The deployed entry point uses `bundle.js`,
`bundle_compiler.js`, and `bundle_extra.js`. `terser.json` preserves global identifiers
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
does not make its assets cold. In the Network waterfall, Dojo should be requested alongside the UI bundle.
The compiler and extra Basis bundle should start after `smltojs-ui-ready`,
with each body transferred once. Record transferred (compressed) bytes as well as time.
A localhost result is useful for checking execution and initialization,
but is not a measurement of GitHub Pages download performance.

## Checking the loading split

Delay or block `bundle_compiler.js` using browser network tools. Verify that
the File and Help menus, editing an opened file, and documentation navigation
work before the compiler is ready. An early Compile click should show a
loading message. After the download finishes, the same edited file should
compile normally. Repeat with a failed `bundle_extra.js` request and verify
that the status reports failure without removing the editor.

This change reduces the JavaScript required before the IDE appears. It does
not move the compiler into a worker: parsing/executing the compiler bundle
and unpickling each individual Basis library still use the main thread.

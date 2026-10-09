#!/bin/sh
# Seed the dependency-free DOM harness with the report's actual markup.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
node - "$1" <<'JS'
const fs=require('fs');
const html=fs.readFileSync(process.argv[2],'utf8').split('<script>')[0];
console.log('const viewerMarkup='+JSON.stringify(html).replace(/</g,'\\u003c')+';');
JS
cat "$ROOT/graph-prelude.js"

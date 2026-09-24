---
layout: page
title: SMLtoJs
header: SMLtoJs — Standard ML in the browser
group: navigation
---
{% include JB/setup %}

<img class="project-logo" src="{{BASE_PATH}}/images/smltojs_logo_transparent_small.png" alt="SMLtoJs logo">

SMLtoJs (pronounced “SML toys”) compiles Standard ML to JavaScript for
client-side web applications. It shares MLKit's frontend and compilation
management, bringing Standard ML's static types, pattern matching,
higher-order functions, and modules to browser programming.

<p><a class="btn btn-primary" href="https://diku-dk.github.io/sml-ide/">Try the Online SML IDE &raquo;</a></p>

The Online SML IDE runs the compiler in your browser: you can write,
compile, and execute Standard ML without installing a compiler locally.

### Language and browser support

SMLtoJs supports all of Standard ML and most of the Standard ML Basis
Library. Browser integration includes calls to JavaScript functions, DOM
access, and Standard ML functions used as event handlers and timer callbacks.
Modules are eliminated at compile time, and optimizations include function
inlining and specialization of higher-order functions.

### Compile a program locally

Get SMLtoJs from the [MLKit distribution or sources]({{BASE_PATH}}/download.html).
Save the following as `hello.sml`:

```sml
val _ = print "Hello from Standard ML!\n"
```

Then compile it:

```sh
smltojs hello.sml
```

Open the generated `run.html` in a browser. Keep the generated JavaScript
files alongside it. SMLtoJs also accepts an [ML Basis file]({{BASE_PATH}}/mlbasisfiles.html)
for programs split across several source files. Use `smltojs -o hello hello.sml`
to name the generated page `hello.html`.

### Further reading

* [SMLtoJs source documentation](https://github.com/melsman/mlkit/blob/master/README_SMLTOJS.md)
* [Browser examples and tests](https://github.com/melsman/mlkit/tree/master/js/test)
* [SMLtoJs: Hosting a Standard ML Compiler in a Web Browser](https://elsman.com/pdf/smltojs-final.pdf)

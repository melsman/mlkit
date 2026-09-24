---
layout: page
title: Download
header: Download
group: navigation
---
{% include JB/setup %}

### The Sources

The MLKit sources are [hosted at Github](https://github.com/melsman/mlkit) and contain the sources for

* The MLKit native compiler, including x64 and arm64 backends and the [interactive REPL]({{BASE_PATH}}/repl.html).
* [ReML]({{BASE_PATH}}/reml.html), Standard ML with explicit regions, effects, and effect constraints.
* [SMLtoJs]({{BASE_PATH}}/smltojs.html), an optimizing Standard ML to JavaScript compiler.
* [Online SML IDE](https://diku-dk.github.io/sml-ide), an online Standard ML compiler running in the user's browser with Dropbox used as storage.
* Tools, such as MLYacc, MLLex, Rp2ps (profile visualizer), and many others.

### Prerequisites

MLKit's x64 backend supports Linux and macOS. The new arm64 backend
supports native builds on Apple Silicon (macOS); see the brief [arm64 note](#arm64) below.

### Download

To get the latest version of the sources, issue the following `git` command:

    git clone https://github.com/melsman/mlkit.git

For older versions of the MLKit, you may browse the [list of releases](https://github.com/melsman/mlkit/releases).

### Installation

See the [installation instructions](https://github.com/melsman/mlkit#installation)
and [release downloads](https://github.com/melsman/mlkit/releases/latest)
for packaged versions. Check the architecture and features of the chosen
release; new development features may require building from source.

After installation, run `mlkit` to start the [REPL]({{BASE_PATH}}/repl.html),
`mlkit program.sml` to compile a native program, `reml program.sml` to use
[ReML]({{BASE_PATH}}/reml.html), or `smltojs program.sml` to compile for the
[browser]({{BASE_PATH}}/smltojs.html).

### Native arm64 on Apple Silicon {#arm64}

The new arm64 backend supports native MLKit, ReML, and the REPL on Apple
Silicon (macOS). It is currently available from the development sources.
See the [compiler documentation](https://github.com/melsman/mlkit/blob/master/doc/arm64-compiler.md)
for details.

### Very old versions of the MLKit

* [MLKit version 3, December 3, 1998](http://www.it.edu/research/mlkit/kit3/readme.html)
* [MLKit version 2 (i.e., The MLKit with Regions), 1997](http://www.it.edu/research/mlkit/kit2/readme.html)
* [MLKit version 1, 1993](http://www.it.edu/research/mlkit/kit2/readme.html)

---
layout: page
title: Interactive REPL
header: Working interactively with MLKit
---
{% include JB/setup %}

MLKit includes a read–eval–print loop (REPL) for exploring Standard ML,
trying functions, and loading projects. Start it by running `mlkit` without
a source-file argument:

```sh
mlkit
```

Enter declarations and expressions, ending each input with a semicolon:

```sml
val greeting = "Hello, MLKit!";
fun square x = x * x;
List.map square [1, 2, 3, 4];
```

The REPL reports values and types. Definitions remain available to later
inputs, so the last expression uses `square` and produces `[1, 4, 9, 16]`.

### Useful commands

Commands also end with a semicolon.

| Command | Action |
| --- | --- |
| `:help;` | Show the available commands. |
| `:load project.mlb;` | Load a project described by an ML Basis file. |
| `:flags;` | Describe compiler flags. |
| `:help flag;` | Show help for a particular flag. |
| `:reset;` | Reset the interactive session. |
| `:quit;` | Exit the REPL. |

See [ML Basis files]({{BASE_PATH}}/mlbasisfiles.html) for organizing a project
and [Download]({{BASE_PATH}}/download.html) for installation instructions.
The native arm64 backend also supports the REPL on Apple Silicon.

To try Standard ML directly in a browser, use the
[SMLtoJs Online SML IDE]({{BASE_PATH}}/smltojs.html).

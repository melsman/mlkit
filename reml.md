---
layout: page
title: ReML
header: ReML — programming with explicit regions
group: navigation
---
{% include JB/setup %}

<img class="project-logo" src="{{BASE_PATH}}/images/reml.svg" alt="ReML logo">

ReML extends Standard ML with explicit regions, effects, and effect
constraints. It combines MLKit's region inference with annotations that let
you express where values are allocated and constrain how functions use
memory. Standard ML programs are also ReML programs, so annotations can be
introduced where you need more control.

### Regions and effects

* **Explicit regions** let you name regions and specify allocation in source code.
* **Region and effect parameters** describe how functions use memory.
* **Effect constraints** express relationships between effects, including
  mutation effects, and support reasoning about parallel computations.

ReML supports parallel threads. It currently runs without reference-tracing
garbage collection; memory is managed through regions.

### Getting started

ReML is included in the [MLKit sources and distribution]({{BASE_PATH}}/download.html).
Compile a source file with the `reml` executable:

```sh
reml example.sml
./run
```

For example, save this program as `example.sml`:

```sml
fun down `r (n : int) : int list`r =
    case n of
        0 => nil
      | _ => n :: down (n - 1)

val first =
    let with r
    in hd (down `r 5)
    end

val _ = print (Int.toString first ^ "\n")
```

The list is allocated in the local region `r`. Only its first element, an
integer, leaves the scope; the region holding the list can be reclaimed
when that scope ends. The program prints `5`.

### Learn more

* Martin Elsman. [Explicit Effects and Effect Constraints in ReML]({{BASE_PATH}}/pdf/popl24-final.pdf). POPL 2024.

* [ReML examples in the compiler test suite](https://github.com/melsman/mlkit/tree/master/test/explicit_regions)
* [Compiler options](https://github.com/melsman/mlkit/blob/master/man/man1/mlkit.1), including `--reml`
* [Programming with regions]({{BASE_PATH}}/doc.html) for the foundations of MLKit's region inference
* [Native arm64 builds on Apple Silicon]({{BASE_PATH}}/download.html#arm64), including ReML

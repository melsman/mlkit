# Explicit storage modes in ReML

ReML accepts optional storage modes after the backquote at allocation sites
and at explicit region arguments:

```sml
fun make `r () : real = 5.4`sat r

fun example () =
    let with r
        val x = make `atbot r ()
        val y = 6.4`attop r
    in x + y
    end
```

Use `infix +` when compiling this example without the Basis library.

- `atbot r` requests resetting a locally declared region before allocation.
  The storage mode analysis must infer `atbot` at this site.
- `sat r` uses a function's region parameter with its runtime storage mode.
  The analysis must infer `sat` at this site.
- `attop r` requests allocation without resetting the region. This can override
  an inferred `atbot` or `sat`, and can refer to a local region or a region parameter.

Neither `atbot` nor `sat` overrides the analysis. A conflicting annotation is a
compilation error identifying its source location. Checking uses the program
that reaches storage mode analysis, after the earlier compiler passes.

The same syntax works with strings, records, tuples, `ref`, and datatype
constructors. For multiple explicit region arguments, annotations are optional
on each argument:

```sml
f `[atbot r1, sat r2] ()
f `[r1, attop r2] ()
```

The existing whitespace-separated region argument lists remain valid. Storage
modes are not permitted in region bindings, region annotations in types, or
ordinary Standard ML mode. The words are contextual: `val atbot = 34`,
functions named `sat`, record labels named `attop`, and region names with these
spellings remain valid. In an allocation annotation, `atbot r` means a mode
followed by a region name; a single name such as `f` followed by `` `[atbot] ``
uses the region named `atbot` without specifying a mode.
Existing ReML programs need no new annotations.

Run the focused regression checks after building the compilers and runtime:

```sh
make -C test/explicit_regions test_storage_modes
```

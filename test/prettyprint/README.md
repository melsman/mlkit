# Pretty-printer span checks

Run after building MLKit and its Basis libraries:

```sh
SML_LIB="$PWD" sh test/prettyprint/check.sh
```

`MLKIT` can select another compiler; `SML_LIB` must select its matching libraries.
The test uses a minimal Report adapter to isolate PrettyPrint from unrelated
compiler and pickling dependencies. CI runs it on the native compiler.

The checks cover byte offsets and lengths, repeated and empty markers, UTF-8,
embedded newlines, omitted text, failed flat layouts, horizontal nodes, both
separator directions, prefix placement and clipped whitespace, and abbreviated
deep indentation. Across several widths and both ragged-right settings, marked
and unmarked trees must produce identical output through the existing APIs.
Every reported span in the layout corpus must select its original marked text.

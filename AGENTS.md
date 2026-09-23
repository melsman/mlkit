# Standard ML style

Follow these rules when adding or modifying Standard ML code in this repository:

- Always separate `=>` from surrounding code with spaces, for example
  `fn x => x + 1`.
- Always separate `=` in `val` and `fun` declarations with spaces, for example
  `val a = 5`.
- A `let ... in ... end` expression may occupy a single line. If it spans
  multiple lines, put `let`, `in`, and `end` at the start of separate lines,
  aligned in the same column. Indent declarations and the body within them.
  Declarations may occur aligned to the right of `let`. An expression may occur
  immediately after `in`. These two squeezing rules may save vertical space.
- Do not put multiple `val` declarations on the same line.

For example:

```sml
let val x = computeX ()
    val y = computeY x
in List.map (fn z => z + y) xs
end
```

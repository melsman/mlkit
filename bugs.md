---
layout: page
title: Bugs
---
{% include JB/setup %}

If you find a bug, not listed below, please contribute with an issue
on the [Github page](http://github.com/melsman/mlkit) containing
information about how the bug can be reproduced.

### Known Bugs and Limitations

Arithmetic and Trigonometric Functions in Math Module.
: The implementation uses the C functions specified in
math.h. Unfortunately, these C functions are platform dependent and do
not always return NaN precisely when the Basis Library stipulates that
they should.

Polymorphic Equality.
: Polymorphic equality is implemented only for "regular" data types. A datatype binding
    tyvarseq_1 tycon_1 = conbind_1 AND ... AND tvseq_n tycon_n = conbind_n
is regular iff all occurrences of tycon_i, i \in {1,...,n} in {conbind_1, ..., conbind_n} are on the form
`tyvarseq_i tycon_i`.
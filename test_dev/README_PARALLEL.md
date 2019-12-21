## Region Inference and Pthread Parallelism

Compile as follows:

    $ SML_LIB=.. ../bin/mlkit -Ppse -par -no_gc -no_basislib --no_delete_target_files --no_specialize_recursive_functions --maximum_inline_size 0 spawn_simple8.sml

The critical flag is the `-par` flag, which makes the MLKit choose a
version of the runtime system that supports threading.  The flags
`--no_specialize_recursive_functions` and `--maximum_inline_size 0`
are there to make pretty printing of the intermediate language program
nicer.

You may want to nuke the local `MLB` directory occationally:

    $ rm -rf MLB

To run the generated program, simply do as follows:

    $ time ./run

The program `spawn_simple8.sml` will spawn 10000 threads each of which
will add one to each of the numbers in the list [0,...,99]. The
resulting 10000 lists (each being [1,...,100]) are passed to the main
thread to be added up.

## Parallel computation of fib numbers

For a more interesting example, consider the program
`spawn_simple7.sml`, which will calculate `fib 38` 20 times in
parallel and sum the results. On my MacBook Pro 2016, here is the result:

    $ SML_LIB=.. ../bin/mlkit -Ppse -par -no_gc -no_basislib --no_specialize_recursive_functions --maximum_inline_size 0 spawn_simple7.sml
    $ time ./run
    starting...
    Num: 1264919720
    goodbye.
    real	0m1.525s
    user	0m11.803s
    sys	0m0.012s

It seems we're utilising the 8 cores quite well...

## Todo

Here are a couple of issues we still need to deal with:

- Two different threads should not be allowed to allocated into the
  same regions, which would cause an allocation-pointer race. We can
  have the region type system ensure that this is not happening by
  checking that the put-effects for different thread functions are
  disjoint.

- Exceptions raised by a thread should be captured by the thread
  object so that it can be raised when the get function is
  applied. Moreover, if the scope function raises an exception, we
  still need to wait for (or cancel) subthreads. There are some
  semantic choices we need to make here. Also, if both a subthread and
  the scope function raises an exception, which should take
  precedence?

- Somehow allow the user to configure stack sizes. Also, see if we can
  somehow capture stack overflow and somehow treat it by raising an
  exceotion.

- Profiling is not yet supported.

- Garbage collection is not yet supported.

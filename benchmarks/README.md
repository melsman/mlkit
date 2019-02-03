## Benchmark tool

To run a number of benchmarks, execute the following command:

    $ make clean bench

This command will compile and execute a number of benchmark programs
(with and without generational garbage collection enabled). Each
executable will be executed 10 times and the report `bench.html` will
show average numbers over the 10 runs.

For details about the use of the `kitbench` tool, execute:

    $ ../bin/kitbench -help

1.prof
------

Profile after naive mapping:
Array ~> Unboxed Vector of V3 ~> list ~> Vector of Word8.

Generated using:
stack install --executable-profiling --no-library-profiling --ghc-options="-fprof-auto -rtsopts"

Executed using:
test-exe +RTS -p

2.prof
------

Profile after writing screen data to ByteString, then blitting the bytestring
directly into the surface buffer. Still too many allocations - probably could
save by allocating the memory buffer directly and reusing?

3.prof
------

Took out an argument from the color computation, so the color mapping table
computation can be floated out by the compiler.

Next
----

When down to the byte-copy level, compiling with profiling adds too much noise,
so we go unprofiled and rather measure the CPU / allocation rate while modifying
the code. Eventually the DWARD-based sampling would be nice.

Run a few times to get a sense of CPU usage:
    /usr/bin/time -f '%P' test-exe +RTS -s

Window surface access: 1%
Blitting: 3%
Update window surface: 1%

Criterion
---------

For more accurate measurements, criterion can be used. Some interesting stuff:

 - Banging all the let-bound vars in the 'screenToBytes' tight loop slowed
   things? It seems it prevented some optimizations (~20x speedup).

 - Version 3, which uses a single loop var instead of the separate x/y is
   slightly faster, if the color routine is commented.

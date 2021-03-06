
This doc captures the profiling process used to ensure performant rendering.
Try to see changes to this file along with other changes in that commit.

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

5.crit
------

Benchmark done with criterion, on blitting the RGB info for the bit-packed pixel
representation. One blit is under a millisecond now.

6.crit
------
Color cache data got a Storable instance, letting it stored in a Storable Vec,
gainging a few hundred microseconds per blit.

7.crit
------
Make sprite drawing blit directly to a Word8 vector, without roundtripping to a
set. Also have a specific aligned and unaligned routine (the aligned one being
slightly faster), mostly for fun.

8.*
---
All of the following are needed for a tight loop (ad-hoc):
- Storable, unpacked datatypes.
- Banging the loop args.
- Using unsafeIndex or indexM instead of !.
- Moving a function from where-clause to top-level (Wat?).
  Maybe this prevented creating a closure capturing some stuff.

Core was dumped using
    stack install --ghc-options="-ddump-simpl -ddump-to-file" --force-dirty
, need to do
    stack clean
before to get it generated. Search for 'simpl' in local tree after.

To get a sense about allocation, run the generated binary with the `+RTS -s` or
even `+RTS -S` flags. The latter gives immediate feedback while running, as more
allocation tends to manifest in faster appearing lines.

Note: test-exe was renamed to zxhs-demo.

Resources
---------

- http://johantibell.com/files/HE2015.pdf
- http://www.serpentine.com/criterion/tutorial.html


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

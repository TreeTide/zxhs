ZxHs
====

ZxHs emulates the graphics features of ZX Spectrum home computers, letting
enthusiasts and hobbyists write ZX Spectrum apps (probably games) on modern
platforms using the Haskell language.

Features:

  * Attribute blocks - the pixel depth is 1 bit, so a pixel is either on or off.
    The color is controlled per so-called attribute block (8x8 pixel blocks),
    and each block has a given background and foreground color.

  * Print text with the original Spectrum font.

Note
----

The library doesn't aim to emulate all aspects of the ZX Spectrum, or to emulate
them precisely. Neither does it aim to generate programs runnable on actual ZX
Spectrum hardware.

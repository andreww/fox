# Installing FoX

FoX requires a Fortran-95 compiler. Fortran 90 is *not* sufficient. No non-standard features are required, though.

It is primarily tested using NAG f95 (version 5.1) and g95, on both of which compilers it works correctly.

Some problems are known to occur with older versions of some compilers - if you encounter such problems please upgrade.

The following compilers have been tested.

Intel version 8.x(linux-ia32): works partly with `-DBROKEN_COMPILER` (will compile without, but fail later)
      version 9.1(linux-ia32): works correctly, passes all tests
PGI version 6.1(linux-ia32): fails - bug report made.
XLF version 9.1(powerpc-aix): works entirely, passes all tests
NAG version 5.1(linux-ia32,osx-ppc): works entirely
g95 2006-08-01(linux-ia32,osx-ppc): works entirely (with the exception of two bugs in str, under investigation)
Pathscale 2.4(linux-x86_64): fails completely

If you have difficulty compiling, try adding
`-DBROKEN_COMPILER` to `FPPFLAGS` in `arch.make`
This will give you reduced functionality in the area of 
floating point output, but should otherwise work.


Go into top-level directory:

Type

     `config/configure`

This will create a file `arch.make` with hopefully the correct settings for your compiler.

If you have more than one compiler installed, you can direct the configure script to find the correct compiler by doing:

     config/configure --FC=/path/to/compiler

You ought not to need to, but you may wish to adapt the compiler flags; this can be done by editing `arch.make` by hand.

To actually compile the library, simply type `make`

Assuming successful compilation, you can run the test-suite by doing

    make check




#Using FoX in your own project.

The recommended way to use FoX is to embed the full source code into an existing project.

(It would be possible to extract portions of the code, and embed just the ones that you need, but I recommend against it; it would be easy to lose parts of the code which are essential for generating good XML.)

In order to do this, you need to do something like the following:

1. Put the full source code as a top-level subdirectory of the tree, called FoX.
(you can of course delete the DoX/ and examples/ subdirectories if you wish to 
save space)
2. Incorporate calls to FoX into the program.
3. Incorporate building FoX into your build process.

##To incorporate into the program

There is an example of suggested use in the `examples/` subdirectory.

The easiest, and least intrusive way is probably to create a F90 module for your program, looking something like `example_xml_module.f90`

Then you must somewhere (probably in your main program), use this module, and call `initialize_xml_output()` at the start; and then `end_xml_output()` at the end of the program.

In any of the subroutines where you want to output data to the xml file, you should then insert `use example_cml_moule` at the beginning of the subroutine. You can then use any of the cml output routines with no further worries, as shown in the examples.

It is easy to make the use of FoX optional, by the use of preprocessor defines. This can be done simply by wrapping each call to your XML wrapper routines in `#ifdef XML`, or similar.

##To incorporate into the build process:

If you have some sort of automatic Makefile configuration; for picking up which compiler to use, etc. then within whatever script you use to do this, you should insert a sequence of commands like:

	(cd FoX; config/configure; cd ..)

This will instruct FoX to perform its own automatic configuration process.

Within the Makefile itself, you need to alter your compiler flags in the following fashion. Assuming that you have some sort of FFLAGS Makefile variable, then it should be amended like so:

	FFLAGS="$(FFLAGS) `FoX/FoX-config --fcflags`"

You must also alter the linking step to include the FoX subroutines Again, assuming that you have some sort of variable LDFLAGS holding your linking flags, then it should be amended like so:

	LDFLAGS="$(LDFLAGS) `FoX/FoX-config --libs`"

If you don't have any automatic Makefile configuration, and rely on the user making hand-edited changes to Makefiles, then you must add to your documentation how to configure & build FoX.


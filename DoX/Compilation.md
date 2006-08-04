#Configuration and compilation

You will have received the FoX source code as a tar.gz file.

Unpack it as normal, and change directory into the top-level directory, FoX.


### Requirements for use

FoX requires a Fortran 95 compiler - not just Fortran 90. All currently available versions of Fortran compilers claim to support F95. If your favoured compiler is not listed as working below, I recommend the use of [g95](www.g95.org), which is free to download and use.

The following compilers are tested and are known to work:

* Intel Fortran version 9.0 and above (previous versions may compile, but do not work correctly.)
* XLF version 9.1 (previous versions untested)
* NAG version 5.0 (patch 391 and later) or 5.1
* g95 (version of 2006-08-01, earlier versions untested)

The following compilers are tested and known to fail

* Intel Fortran prior to version 9.0
* NAG versions prior to 5.0-391
* Pathscale, all versions (bug reported #6259)
* PGI, all versions (bug reported)
* gfortran prior to and including version 4.1

Other compilers are untested; reports of their success or failure are welcomed.

##Configuration

* In order to generate the Makefile, make sure that you have a Fortran compiler in your `PATH`, and do:

	config/configure

This should suffice for most installations. However:

1. If you have more than one Fortran compiler available, or it is not on your `PATH`, you can force the choice by doing:

	config/configure FC=/path/to/compiler/of/choice

2. It is possible that the configuration fails. In this case
	*  please tell me about it so I can fix it
  	* all relevant compiler details are placed in the file arch.make; you may be able to edit that file to allow compilation. Again, if so, please let me know what you need to do.

3. By default the resultant files are installed under the objs directory. If you wish them to be installed elsewhere, you may do

	config/configure --prefix=/path/to/installation

Note that the configure process encodes the current directory location in several
places.  If you move the FoX directory later on, you will need to re-run configure.

##Compilation

In order to compile the full library, now simply do:

	make

This will build all the FoX modules, and all the examples.
However, you may only be interested in building the libraries, or perhaps a subset of the libraries. In that case, the following targets are available:

        wxml_lib
        wcml_lib

##Linking to an existing program

* The files all having been compiled and installed, you need to link them into your program.

A script is provided which will provide the appropriate compiler and linker flags for you; this will be created after configuration, in the top-level directory, and is called `FoX-config`. It may be taken from there and placed anywhere.

FoX-config takes the following arguments:

* `--fcflags`: return flags for compilation
* `--libs`: return flags for linking
* `--wxml`: return flags for compiling/linking against wxml
* `--wcml`: return flags for compiling/linking against wcml


For compiling files against FoX, do the following:

 	f95 -c `FoX-config --fcflags` sourcefile.f90

For linking objects to the FoX library, do:

  	f95 -o program `FoX-config --libs` *.o

or similar, according to your compilation scheme. 

Note that by default, `FoX-config` assumes you are using all modules of the library. If you are only using part, then this can be specified by also passing the name of each module required, like so:

	FoX-config --fcflags --wcml

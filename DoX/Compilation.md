#Configuration and compilation

You will have received the FoX source code as a tar.gz file.

Unpack it as normal, and change directory into the top-level directory, FoX.


### Requirements for use

FoX requires a Fortran 95 compiler - not just Fortran 90. All currently available versions of Fortran compilers claim to support F95. If your favoured compiler is not listed as working below, I recommend the use of [g95](www.g95.org), which is free to download and use. And if your favourite compiler is listed as not working, then please send a bug report to your compiler vendor.

The list below shows results as tested at the time of release with the 3.0 release of the FoX code. An up-to-date list of tested compiler version is maintained [here](http://uszla.me.uk/space/software/FoX/compat/)

* g95 as of 25/5/7
* gfortran as of version 4.2
* Intel version 8.1, 9.1, 10.1
* NAG version 5.1
* PGI version 6.1, 6,2, 7.0
* XLF version 10.01

Known failures:

* g95 prior to 25/5/7  
* Intel version 7.1  
* NAG version 5.0  
* PGI version 5.1  

Results from other compilers are welcome.

As of version 2.0.2, the following other compilers had been tested and are known to work:

* Lahey version 6.20
* Pathscale, version 2.4
* XLF version 9.1

##Configuration

* In order to generate the Makefile, make sure that you have a Fortran compiler in your `PATH`, and do:

    `config/configure`

This should suffice for most installations. However:

1. You may not be interested in all of the modules that FoX supplies. For example, you may only be interested in output, not input. If so, you can select which modules you want using `--enable-MODULENAME` where MODULENAME is one of `wxml`, `wcml`, `wkml`, `sax`, `dom`. If none are explicitly enabled, then all will be built. (Alternatively, you can exclude modules one at a time with `--disable-MODULENAME`)

2. If you have more than one Fortran compiler available, or it is not on your `PATH`, you can force the choice by doing:

   `config/configure FC=/path/to/compiler/of/choice`

3. It is possible that the configuration fails. In this case
	* please tell me about it so I can fix it
  	* all relevant compiler details are placed in the file arch.make; you may be able to edit that file to allow compilation. Again, if so, please let me know what you need to do.

4. By default the resultant files are installed under the objs directory. If you wish them to be installed elsewhere, you may do

    `config/configure --prefix=/path/to/installation`

Note that the configure process encodes the current directory location in several
places.  If you move the FoX directory later on, you will need to re-run configure.

##Compilation

In order to compile the full library, now simply do:

    make

This will build all the FoX modules, and all the examples.
However, you may only be interested in building the libraries, or perhaps a subset of the libraries. In that case, the following targets are available:

    wxml_lib
    wcml_lib
    wkml_lib
    sax_lib
    dom_lib

##Testing

Three test-suites are supplied; in `common/test`, `wxml/test`, and `wcml/test`. In each case, `cd` to the relevant directory and then run `./run_tests.sh`.

(The sax testsuite is available separately. Please contact the author for details.)

The tests will run and then print out the number of passes and fails. Details of failing tests may be found in the file `failed.out`.

Known failures:     
* `test_xml_Close_2` sometimes unexpectedly fails - this is not a problem, ignore it.  

If any other failures occur, please send a message to the mailing list (<FoX@lists.uszla.me.uk>) with details of compiler, hardware platform, and the nature of the failure.

##Linking to an existing program

* The files all having been compiled and installed, you need to link them into your program.

A script is provided which will provide the appropriate compiler and linker flags for you; this will be created after configuration, in the top-level directory, and is called `FoX-config`. It may be taken from there and placed anywhere.

FoX-config takes the following arguments:

* `--fcflags`: return flags for compilation
* `--libs`: return flags for linking
* `--wxml`: return flags for compiling/linking against wxml
* `--wcml`: return flags for compiling/linking against wcml
* `--sax`: return flags for compiling/linking against sax

If it is called with no arguments, it will expand to compile & link flags, thusly:

       f95 -o program program.f90 `FoX-config`

For compiling only against FoX, do the following:

 	f95 -c `FoX-config --fcflags` sourcefile.f90

For linking only to the FoX library, do:

  	f95 -o program `FoX-config --libs` *.o

or similar, according to your compilation scheme. 

Note that by default, `FoX-config` assumes you are using all modules of the library. If you are only using part, then this can be specified by also passing the name of each module required, like so:

	FoX-config --fcflags --wcml

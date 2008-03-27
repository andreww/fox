#FoX documentation.

[All in one page](FoX_DoX.html)

[Separate pages](FoX.html)
## Introduction

This document is the primary API documentation for FoX, the Fortan/XML library.

* [Versioning](|Versioning|)

* [Configuration and compilation](|Compilation|)

* [Embedding into an existing project](|Embedding|)

## Other documentation

The documentation here is largely reference in nature. For new users it is best to start elsewhere:

### iFaX workshops

Two workshops, entitled iFaX (Integrating Fortran and XML) have been run teaching the use of FoX, one in January 2007 (organized by [NIEeS](http://www.niees.ac.uk)), and one in January 2008 (organized jointly by [NIEeS](http://www.niees.ac.uk) and [eSI](http://www.nesc.ac.uk/esi/)). The full documentation and lectures for these may be found at:

* [iFaX I](http://buffalo.niees.group.cam.ac.uk/archive2.php?event_details=ifax)
* [iFaX II](http://www.nesc.ac.uk/action/esi/contribution.cfm?Title=841)

### Tutorials

Out of the above workshops, some [tutorial material](http://www.uszla.me.uk/FoX/iFaX/) has been written, focussing on different use cases. Currently two are available:

* [SAX input](http://www.uszla.me.uk/FoX/iFaX/iFaX.4/iFaX.4.html)
* [DOM input](http://www.uszla.me.uk/FoX/iFaX/iFaX.5/iFaX.5.html)

## API documentation

* FoX has six sets of publically exported interfaces. These are documented here:

### COMMON interfaces

* [FoX_common](|FoX_common|)
* [FoX_utils](|FoX_utils|)

### OUTPUT interfaces

* [FoX_wxml](|FoX_wxml|)
* [FoX_wcml](|FoX_wcml|)

### INPUT interface

* [FoX_sax](|FoX_sax|)
* [FoX_dom](|FoX_dom|)

These documents describe all publically usable APIs.

If a subroutine or function, or indeed one of its arguments, is not mentioned above, it is not to be considered part of the stable API, even if it is accessible.

The astute developer is reminded that all times the final reference documentation is the source, which is publically available.

Worked examples of the use of these APIs may be found in the `examples/` subdirectory.

## Other things

* [Hints for debugging](|Debugging|)

* [Further information](|Information|)

* [Licensing](|Licensing|)

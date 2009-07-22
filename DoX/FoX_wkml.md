#WKML

WKML is a library for outputting [KML](http://code.google.com/apis/kml/documentation/) data. It wraps all the necessary XML calls, such that you should never need to touch any [WXML](|FoX_wxml|) calls when outputting KML.

The available functions and their intended use are listed below.

This section of the manual will detail the available KML output subroutines.

## Use of WCML

wkml subroutines can be accessed from within a module or subroutine by inserting

     use FoX_wkml

at the start. This will import all of the subroutines described below, plus the derived type `xmlf_t` needed to manipulate a CML file.

*No* other entities will be imported; public/private Fortran namespaces are very carefully controlled within the library.

###Conventions used below.

* Function names are in `monospace`
* argument names are in **bold**
* optional argument names are in (**parenthesized bold**)
* argument types are in *italic* and may consist of:
* *string*: string of arbitrary (unless otherwise specified) length
* *integer*: default integer
* *real(sp)*: single precision real number
* *real(dp)*: double precision real number
* *logical*: default logical 
* *real*: either of *real(sp)* or *real(dp)*

* *scalar*: single item
* *array*: one-dimensional array of items
* *matrix*: two-dimensional array of items
* *anydim*: any of *scalar*, *array*, *matrix*

All functions take as their first argument an XML file object, whose keyword is always `xf`. This file object is initialized by a `kmlBeginFile` function.

It is *highly* recommended that subroutines be called with keywords specified rather than relying on the implicit ordering of arguments. This is robust against changes in the library calling convention; and also stepsides a significant cause of errors when using subroutines with large numbers of arguments.

## Functions for manipulating the KML file:

* `kmlBeginFile`   
**filename**: *string* *scalar*: Filename to be opened.  
**unit**: *integer* *scalar*: what unit number should the file be opened on? If you don't
care, you may specify `-1` as the unit number, in which case wcml will make a guess  
(**replace**): *logical* *scalar*: should the file be replaced if it already exists? *default: yes*
(**docName**): *string* *scalar*: an optional name for the outermost document element. If absent, WKML output will be used

This takes care of all calls to open a KML output file.

* `kmlFinishFile`

This takes care of all calls to close an open KML output file, once you have finished with it. It is compulsory to call this - if your program finished without calling this, then your KML file will be invalid.


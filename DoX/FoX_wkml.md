#WKML

WKML is a library for creating [KML](http://www.opengeospatial.org/standards/kml/) 
documents. These documents are intended to be used for "expressing geographic 
annotation and visualization" for maps and Earth browsers such as 
[Google Earth](http://earth.google.co.uk/) or [Marble](http://edu.kde.org/marble/).
WKML wraps all the necessary XML calls, such that you should never need to touch any
[WXML](|FoX_wxml|) calls when outputting KML from a Fortran application.

WKML is intended to produce XML documents that conform to version 2.2 of the Open
Geospatial Consortium's [schema](http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd).
However, the library offers no guarantee that documents produced will be valid as 
only a small subset of the constraints are enforced. The API is designed to minimize 
the possibilty of producing invalid KML in common use cases, and well-formdness is
maintained by the underlying WXML library.

The available functions and their intended use are listed below. One useful reference to
the use of KML is Google's [KML documentation](http://code.google.com/apis/kml/documentation/).

## Use of WCML

wkml subroutines can be accessed from within a module or subroutine by inserting

     use FoX_wkml

at the start. This will import all of the subroutines described below, plus the derived type `xmlf_t` needed to manipulate a KML file.

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

* arguments may be:
* *scalar*: single item
* *array*: one-dimensional array of items
* *matrix*: two-dimensional array of items
* *anydim*: any of *scalar*, *array*, *matrix*

All functions take as their first argument an XML file object, whose
keyword is always `xf`. This file object is initialized by a `kmlBeginFile` function.

It is *highly* recommended that subroutines be called with keywords
specified rather than relying on the implicit ordering of arguments.
This is robust against changes in the library calling convention; and 
also stepsides a significant cause of errors when using subroutines
with large numbers of arguments.

## Functions for manipulating the KML file:

* `kmlBeginFile`   
**fx**: *xmlf_t*: An XML file object
**filename**: *string* *scalar*: Filename to be opened.  
**unit**: *integer* *scalar*: what unit number should the file be opened on? If you don't
care, you may specify `-1` as the unit number, in which case wkml will make a guess  
(**replace**): *logical* *scalar*: should the file be replaced if it already exists? *default: yes*
(**docName**): *string* *scalar*: an optional name for the outermost document element. If absent, "WKML output" will be used

This takes care of all calls to open a KML output file.

* `kmlFinishFile`  
**fx**: *xmlf_t*: An XML file object 

This takes care of all calls to close an open KML output file, once
you have finished with it. It is compulsory to call this - if your
program finished without calling this, then your KML file will be
invalid.

* `kmlOpenDocument` 
**fx**: *xmlf_t*: An XML file object
**name**: *string* *scalar*: a name for the new document element.
(**id**): *string* *scalar*: an optional xml id for the new document element.

This starts a new document element at this point in the output. Note that no
checks are currently performed to ensure that this is permitted, for example
only one document is permitted to be a child of the kml root element. Most
users should not need to use this subroutine.

* `kmlCloseDocument`
**fx**: *xmlf_t*: An XML file object 

This closes the current document element. Do not close the outermose document 
element created with `kmlBeginFile`, this must be closed with `kmlFinishFile`.
Most users should not need to use this subroutine.


## Functions for producing geometrical objects:

* `kmlCreatePoints`  
**fx**: *xmlf_t*: An XML file object  
(**extrude**): *logical* *scalar*: If altitude is non-zero, should the point be connected to the ground?  
(**altitudeMode**): *logical* *scalar*: If altitude is specified, is it relativeToGround or absolute?  
(**name**): *string* *scalar*: A name for the collection of points  
(**color**): *color_t*: Line colour as a kml color type (See Colours)  
(**colorname**): *string* *scalar*: Line colour as a name (See Colours)  
(**colorhex**): *string(len=8)* *scalar*: Line colour in hex (See Colours)  
(**scale**): *real* *scalar*: Scaling size for the point icon.    
(**description**): *string* *array*: A description for each point.  
(**description_numbers**): *real* *array*: Numeric description for each point.
(**styleURL**): *string* *scalar*: Location of style specification (see Style Handling)  
*and:*  
**longitude**: *real* *array*: longitude of each point in degrees  
**latitude**: *real* *array*: latitude of each point in degrees  
(**altitude**): *real* *array*: altitude of each point in metres  
*or:*  
**location**: *real* *matrix*: rank-two 2xN array with the longitude of each point in the first row, and the latitude in the second row. In degrees.  
(**altitude**): *real* *array*: altitude of each point in metres  
*or:*  
**location**: *real* *matrix*: rank-two 3xN array with the longitude of each point in the first row, the latitude in the second row, and the altitude in the third row. Longitude and latitude in degrees and altitude in metres.  

A single function, kmlCreatePoints accepts various combinations of arguments, and will generate a series of individual points to be visualized in Google Earth. In fact, the KML produced will consist of a Folder, containing Placemarks, one for each point. The list of points may be provided in any of the three ways specified above.

* `kmlCreateLine`  
**fx**: *xmlf_t*: An XML file object  
(**closed**): *logicl* *scalar*: Should the last point be joined to the first point?  
(**extrude**): *logical* *scalar*: If altitude is non-zero, should the point be connected to the ground?    
(**tessellate**): *logical* *scalar*: If altitude is not specified, should the line produced follow the altitude of the ground below it?   
(**altitudeMode**): *logical* *scalar*: If altitude is specified, is it relativeToGround or absolute?    
(**name**): *string* *scalar*: A name for the collection of points    
(**color**): *color_t*: Line colour as a kml color type (See Colours)  
(**colorname**): *string* *scalar*: Line colour as a name (See Colours)  
(**colorhex**): *string(len=8)* *scalar*: Line colour in hex (See Colours)  
(**width**): *integer* *scalar*: Width of the lines.
(**scale**): *real* *scalar*: Scaling size for the point icon.    
(**description**): *string* *array*: A description for each point.  
(**styleURL**): *string* *scalar*: Location of style specification (see Style Handling)  
*and:*  
**longitude**: *real* *array*: longitude of each point in degrees  
**latitude**: *real* *array*: latitude of each point in degrees  
(**altitude**): *real* *array*: altitude of each point in metres  
*or:*  
**location**: *real* *matrix*: rank-two 2xN array with the longitude of each point in the first row, and the latitude in the second row. In degrees.  
(**altitude**): *real* *array*: altitude of each point in metres  
*or:*  
**location**: *real* *matrix*: rank-two 3xN array with the longitude of each point in the first row, the latitude in the second row, and the altitude in the third row. Longitude and latitude in degrees and altitude in metres.  

A single function, kmlCreateLine accepts various combinations of arguments, and will generate a series of individual points to be visualized as a (closed or open) path in Google Earth. In fact, the KML produced will consist of a LineString, or LinearRing, containing a list of coordinates. The list of points may be provided in any of the three ways specified above.

* `kmlStartRegion`  
**fx**: *xmlf_t*: An XML file object  
(**extrude**): *logical* *scalar*: If altitude is non-zero, should the point be connected to the ground?    
(**tessellate**): *logical* *scalar*: If altitude is not specified, should the line produced follow the altitude of the ground below it?   
(**altitudeMode**): *logical* *scalar*: If altitude is specified, is it relativeToGround or absolute?    
(**name**): *string* *scalar*: A name for the region
(**fillcolor**): *color_t*: Region colour as a kml color type (See Colours)  
(**fillcolorname**): *string* *scalar*: Region  colour as a name (See Colours)  
(**fillcolorhex**): *string(len=8)* *scalar*: Region colour in hex (See Colours)  
(**linecolor**): *color_t*: Line colour as a kml color type (See Colours)  
(**linecolorname**): *string* *scalar*: Line colour as a name (See Colours)  
(**linecolorhex**): *string(len=8)* *scalar*: Line colour in hex (See Colours)  
(**linewidth**): *integer* *scalar*: Width of the line.
(**description**): *string* *scalar*: A description for the region.  
(**styleURL**): *string* *scalar*: Location of style specification (see Style Handling)  
*and:*  
**longitude**: *real* *array*: longitude of each point in degrees  
**latitude**: *real* *array*: latitude of each point in degrees  
(**altitude**): *real* *array*: altitude of each point in metres  
*or:*  
**location**: *real* *matrix*: rank-two 2xN array with the longitude of each point in the first row, and the latitude in the second row. In degrees.  
(**altitude**): *real* *array*: altitude of each point in metres  
*or:*  
**location**: *real* *matrix*: rank-two 3xN array with the longitude of each point in the first row, the latitude in the second row, and the altitude in the third row. Longitude and latitude in degrees and altitude in metres.  

Creates a filled region with the outer boundary described by the list of points. May be followed by one or more calls to `kmlAddInnerBoundary` and these must be followed by a call to `kmlAddInnerBoundary`.

* `kmlEndRegion`  
**fx**: *xmlf_t*: An XML file object  

Ends the specification of a region with or without inner boundaries.

* `kmlAddInnerBoundary`  
**fx**: *xmlf_t*: An XML file object  
*and:*  
**longitude**: *real* *array*: longitude of each point in degrees  
**latitude**: *real* *array*: latitude of each point in degrees  
(**altitude**): *real* *array*: altitude of each point in metres  
*or:*  
**location**: *real* *matrix*: rank-two 2xN array with the longitude of each point in the first row, and the latitude in the second row. In degrees.  
(**altitude**): *real* *array*: altitude of each point in metres  
*or:*  
**location**: *real* *matrix*: rank-two 3xN array with the longitude of each point in the first row, the latitude in the second row, and the altitude in the third row. Longitude and latitude in degrees and altitude in metres.  

Introduces an internal area that is to be excluded from the enclosing region.

## Colours

KML natively handles all colours as 32-bit values, expressed as 8-digit 
hexadecimal numbers in ABGR (alpha-blue-green-red) channel order. However, 
this is not very friendly. WKML provides a nicer interface to this, and all 
WKML functions which accept colour arguments will accept them in three ways:

### 

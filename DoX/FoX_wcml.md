#WCML

WCML is a library for outputting [CML](http://www.xml-cml.org) data. It wraps all the necessary XML calls, such that you should never need to touch any [WXML](|FoX_wxml|) calls when outputting CML.

The CML output is conformant to version 2.4 of the CML schema.

The available functions and their intended use are listed below. Quite deliberately, no reference is made to the actual CML output by each function. 

Wcml is *not* intended to be a generalized Fortran CML output layer. rather it is intended to be a library which allows the output of a limited set of well-defined syntactical fragments.

Further information on these fragments, and on the style of CML generated here, is available at <http://www.uszla.me.uk/CML/eminerals.html>.

This section of the manual will detail the available CML output subroutines.

## Use of WCML

wcml subroutines can be accessed from within a module or subroutine by inserting

     use FoX_wcml

at the start. This will import all of the subroutines described below, plus the derived type `xmlf_t` needed to manipulate a CML file.

*No* other entities will be imported; public/private Fortran namespaces are very carefully  controlled within the library.

### General naming conventions for functions.

Functions are named in the following way:

* All functions begin 
`cml`

* To begin and end a section of the CML file,
a pair of functions will exist:
	* `cmlStart`something
	* `cmlEnd`something

* To output a given quantity/property/concept etc. a function will exist
`cmlAdd`something


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
* *anytype*: any of *logical*, *integer*, *real(sp)*, *real(dp)*, *string*

Note that where *strings* are passed in, they will be passed through entirely unchanged to the output file - no truncation of whitespace will occur.

Also note that wherever a *real* number can be passed in (including through *anytype*) then the formatting can be specified using the conventions described in [StringFormatting](|StringFormatting|)

* *scalar*: single item
* *array*: one-dimensional array of items
* *matrix*: two-dimensional array of items
* *anydim*: any of *scalar*, *array*, *matrix*

Where an *array* is passed in, it may be passed either as an assumed-shape array; that is, as an F90-style array with no necessity for specifying bounds; thusly:

	integer :: array(50)
	call cmlAddProperty(xf, 'coords', array)

or as an assumed-size array; that is, an F77-style array, in which case the length must be passed as an additional parameter:

	integer :: array(*)
	call cmlAddProperty(xf, 'coords', array, nitems=50)

Similarly, when a *matrix* is passed in, it may be passed in both fashions:

	integer :: matrix(50, 50)
	call cmlAddProperty(xf, 'coords', matrix)

or 

	integer :: array(3, *)
	call cmlAddProperty(xf, 'coords', matrix, nrows=3, ncols=50)

All functions take as their first argument an XML file object, whose keyword is always `xf`. This file object is initialized by a `cmlBeginFile` function.

It is *highly* recommended that subroutines be called with keywords specified rather than relying on the implicit ordering of arguments. This is robust against changes in the library calling convention; and also stepsides a significant cause of errors when using subroutines with large numbers of arguments.

### Units

Note below that the functions `cmlAddParameter` and `cmlAddProperty` both *require* that units be specified for any numerical quantities output.

If you are trying to output a quantity that is genuinely dimensionless, then you should specify `units="units:dimensionless"`; or if you are trying to output a countable quantity (eg number of CPUs) then you may specify `units="units:countable"`.

For other properties, all units should be specified as namespaced quantities. If you are using
a very few common units, it may be easiest to borrow definitions from the provided dictionaries;

(These links do not resolve yet.)

`cmlUnits:` <http://www.xml-cml.org/units/units>  
`siUnits:` <http://www.xml-cml.org/units/siUnits>  
`atomicUnits:` <http://www.xml-cml.org/units/atomic>  

Otherwise, you should feel at liberty to construct your own namespace;
declare it using `cmlAddNamespace`, and markup all your units as:

     units="myNamespace:myunit"

## Functions for manipulating the CML file:

* `cmlBeginFile`  
**filename**: *string* *scalar*: Filename to be opened.

This takes care of all calls to open a CML output file.

* `cmlFinishFile`

This takes care of all calls to close an open CML output file, once you have finished with it. It is compulsory to call this - if your program finished without calling this, then your CML file will be invalid.

* `cmlAddNamespace`  
**prefix** *string* *scalar*: prefix to be used  
**nsURI** *string* *scalar*: namespace URI to be used  

This adds a namespace to a CML file.  
NB This may only ever be called immediately after a `cmlBeginCml` call, before any
output has been performed.
Attempts to do otherwise will result in a runtime error.

This will be needed if you are adding dictionary references to your output. Thus for siesta, we do:

    call cmlAddNamespace(xf, 'siesta', 'http://www.uam.es/siesta')

and then output all our properties and parameters with `dictRef="siesta:something"`.

* `cmlStartCml`  
(**fileId**) *string* *scalar*: name of originating file.  (default: current filename)
(**version**) *string* *scalar*: version of CML in use.  (default: 2.4)

* `cmlEndCml`

This pair of functions begin and end the CML output to an existing CML file. It takes care of namespaces.

Note that unless specified otherwise, there will be a `convention` attribute added to the `cml` tag specifying `FoX_wcml-2.0` as the convention. (see <http://www.uszla.me.uk/FoX> for details)

## Start/End sections

* `cmlStartMetadataList`  
(**name**) *string* *scalar*: name for the metadata list    
(**role**) *string* *scalar* role which the element plays 

* `cmlEndMetadataList`

This pair of functions open & close a metadataList, which is a wrapper for metadata items.

* `cmlStartParameterList`  
(**ref**) *string* *scalar*: Reference an `id` attribute of another element (generally deprecated)  
(**role**) *string* *scalar* role which the element plays 

* `cmlEndParameterList`

This pair of functions open & close a parameterList, which is a wrapper for input parameters.

* `cmlStartPropertyList`  
(**ref**) *string* *scalar*: Reference an `id` attribute of another element (generally deprecated)  
(**role**) *string* *scalar* role which the element plays 

* `cmlEndPropertyList`

This pair of functions open & close a propertyList, which is a wrapper for output properties.

* `cmlStartBandList`
* `cmlEndBandList`

Start/end a list of bands (added using `cmlAddBand` below)

* `cmlStartKpointList`
* `cmlEndKpointList`

Start/end a list of k-points (added using `cmlAddKpoint` below)

* `cmlStartModule`  
(**serial**) *string* *scalar*: serial id for the module  
(**role**) *string* *scalar* role which the element plays 

Note that in most cases where you might want to use a serial number, you should probably be using the `cmlStartStep` subroutine below.

* `cmlEndModule`

This pair of functions open & close a module of a computation which is unordered, or loosely-ordered. For example, METADISE uses one module for each surface examined.

* `cmlStartStep`  
(**index**) *integer* *scalar*: index number for the step. In the absence of an index, steps will be assumed to be consecutively numbered. Specifying this is useful if you wish to output *eg* every hundredth step.   
(**type**) *string* *scalar*: what sort of step is this? This should be a namespaced string, for example: `siesta:CG` is a Conjugate Gradient step in siesta.

* `cmlEndStep`

This pair of functions open and close a module of a computation which is strongly ordered. For example, DLPOLY uses steps for each step of the simulation.

## Adding items.

* `cmlAddMetadata`  
**name**: *string* *scalar*: Identifying string for metadata  
**content**: *anytype* *scalar*: Content of metadata  

This adds a single item of metadata. It takes the following arguments:

* `cmlAddParameter`  
**title**: *string* *scalar*: Identifying title for parameter  
**value**:*anytype* *anydim*: value of parameter  
**units**: *string* *scalar*: units of parameter value  (optional for logical/character **value**s, compulsory otherwise; see note above)
(**constraint**) *string* *scalar*: Constraint under which the parameter is set (this can be an arbitrary string)  
(**ref**) *string* *scalar*: Reference an `id` attribute of another element (generally deprecated)  
(**role**) *string* *scalar* role which the element plays 

This function adds a tag representing an input parameter

* `cmlAddProperty`
**name**: *string* *scalar*  
**value**: *any* *anydim*  
**units**: *string* *scalar* units of property value  (optional for logical/character **value**s, compulsory otherwise; see note above)  
(**ref**) *string* *scalar*: Reference an `id` attribute of another element (generally deprecated)  
(**role**) *string* *scalar* role which the element plays 

This function adds a tag representing an output property


* `cmlAddMolecule`  
**coords**: *real*: a 3xn matrix of real numbers representing atomic coordinates (either fractional or Cartesian) . These *must* be specified in Angstrom or fractional units (see **style** below.)
**OR**  
**x**, **y**, **z**: *real*: 3 one-dimensional arrays containing the *x*, *y*, and *z* coordinates of the atoms in the molecule.  These *must* be specified in Angstrom or fractional units (see **style** below.)  
**elements**: *string* *array*: a length-n array of length-2 strings containing IUPAC chemical symbols for the atoms    
(**natoms**) *integer* *scalar*: number of atoms in molecule (default: picked up from length of **coords** array)  
(**occupancies**): *real* *array* : a length-n array of the occupancies of each atom.  
(**atomRefs**): *string* *array*: a length-n array of strings containing references which may point to IDs elsewhere of, for example, pseudopotentials or basis sets defining the element's behaviour.  
(**atomIds**): *string* *array*: a length-n array of strings containing IDs for the atoms.  
(**style**): *string* *scalar*: `xyz3` - the coordinates are Cartesian, or `xyzFract` - the coordinates are fractional. The default is Cartesian.  
(**ref**) *string* *scalar*: Reference an `id` attribute of another element (generally deprecated)  
(**formula**) *string* *scalar*: An IUPAC chemical formula  
(**chirality**) *string* *scalar*: The chirality of the molecule. No defined vocabulary.
(**role**) *string* *scalar*: Role of molecule. No defined vocabulary.

Outputs an atomic configuration. 

* `cmlAddLattice`   
**cell**: *real* *matrix* a 3x3 matrix of the unit cell  
(**spaceType**): *string* *scalar*: `real` or `reciprocal` space.  
(**latticeType**): *string* *scalar* Space group of the lattice. No defined vocabulary  
(**units**): *string* *scalar* units of (reciprocal) distance that cell vectors is given in;
*default: Angstrom* 


Outputs information about a unit cell, in lattice-vector form

* `cmlAddCrystal`  
**a**: *real* *scalar* the 'a' parameter (must be in Angstrom)  
**b**: *real* *scalar* the 'b' parameter  
**c**: *real* *scalar* the 'c' parameter  
**alpha**: *real* *scalar* the 'alpha' parameter  
**beta**: *real* *scalar* the 'beta' parameter  
**gamma**: *real* *scalar* the 'gamma' parameter  
(**z**): *integer* *scalar* the 'z' parameter: number of molecules per unit cell.  
(**lenunits**): *string* *scalar*: Units of length: default is `units:angstrom`    
(**angunits**): *string* *scalar*: Units of angle: default is `units:degrees`  
(**lenfmt**): *string* *scalar*: format for crystal lengths  
(**angfmt**): *string* *scalar*: format for crystal angles  
(**spaceGroup**): *string* *scalar* Space group of the crystal. No defined vocabulary.

Outputs information about a unit cell, in crystallographic form

* `cmlAddBand`  
**kptref**: *string* *scalar*:  Reference id of relevant kpoint.  
**bands**: *real* *array*: array of eigenvalues  
**units**: * string* *scalar*: energy units of eigenvalues  
(**fmt**): *string* *scalar*: format to output eigenvalues
(**label**): *string* *scalar*: label for band.

Output eigenvalues for a band.

* `cmlAddKpoint`  
**kpoint**: *real* *array* length-3 array defining k-point  
(**weight**): *real* *scalar*: weight of k-point  
(**kptfmt**): *string* *scalar*: format for outputting k-point (default: `"r3"`)  
(**weightfmt**): *string* *scalar*: format for outputting weight (default: `"r3"`)  
(**label**): *string* *scalar*: label for k-point

Output a k-point

* `cmlAddEigen`  
**eigvec**: *real* *matrix* nxn array of eigenvectors  
**eigval**: *real* *array* length-n array of eigenvalues  
(**n**): number of eigenvalues/eigenvectors (default: picked up from length of **eigval**)  
**units**: units of eigenvalues  
**eigenOrientationType**: *string* *scalar*: is the eigenvector matrix `column` or `row` oriented?  
(**vecfmt**): *string* *scalar*: format for outputting eigenvectors  
(**valfmt**): *string* *scalar*: format for outputting eigenvalues  
(**type**): *string* *scalar*: what sort of thing are these eigenvectors? No defined vocabulary.

Output a set of eigenvalues and eigenvectors

##Common arguments

All `cmlAdd` and `cmlStart` routines take the following set of optional arguments:

* `id`: Unique identifying string for element. (Uniqueness is not enforced, though duplicated ids on output are usually an error and may cause later problems)  
* `title`: Human-readable title of element for display purposes  
* `dictRef`: reference to disambiguate element. Should be a QName; a namespaced string. An actual dictionary entry may or may not exist. It is not an error for it not to.  
* `convention`: convention by which the element is to be read.  
(The wording of the definitions for `convention` is deliberately loose.)

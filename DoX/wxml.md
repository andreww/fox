# WXML

Wxml is a general Fortran XML output library. It offers a Fortran interface, in the form of a number of subroutines,  to generate well-formed XML documents. Almost all of the XML features described in XML 1.1 and XML Namespaces 1.1 [ref] are available, and Wxml will diagnose almost all attempts to produce in invalid document. Section [REF] below describes where wxml falls short of these aims

The following functions are available, arranged in three sections.

Firstly, the very few functions necessary to create the simplest XML document, containing only elements, attributes, and text.

Secondly, those functions concerned with XML Namespaces, and how Namespaces affect the behaviour of the first tranche of functions.

Thridly, a set of more rarely used functions required to access some of the more esoteric corners of the XML specification.


##Conventions and notes:

####Conventions used below.

* Function names are in `monospace`
* argument names are in **bold**
* optional argument names are in (**parenthesized bold**)
* argument types are in /italic/ and may consist of:
* /string/: string of arbitrary (unless otherwise specified) length
* /integer/: default integer
* /real(sp)/: single precision real number
* /real(dp)/: double precision real number
* /logical/: default logical 
* /real/: either of /real(sp)/ or /real(dp)/
* /anytype/: any of /logical/, /integer/, /real(sp)/, /real(dp)/, /string/

Note that where /strings/ are passed in, they will be passed through entirely unchanged to the output file - no truncation of whitespace will occur.

It is strongly recommended that the functions be used with keyword arguments rather than replying on implicit ordering.


####Derived type: `xmlf_t`
This is an opaque type representing the XML file handle. Each function requires this as an argument, so it knows which file to operate on. (And it is an output of the xml_OpenFile subroutine) Since all subroutines require it, it is not mentioned below.

-----

### Frequently used functions

* `xml_OpenFile`

Open a file for writing XML

**filename**: /string/: Filename to be opened  
**xf**: /xmlf_t/: XML File handle  
(**indent**): /logical/:  Should the output XML be indented according to element depth?
  *default: false*  
(**channel**): /integer/: What Fortran file handle should the XML file be attached to? 
  *default: picked by the library at runtime*  
(**replace**): /logical/: Should the file be replaced if it already exists? 
  default: no, stop at runtime if file already exists*  
(**addDecl**): /logical/: Should an XML declaration be added at the start of the file?
  default: yes


* `xml_Close` 

Close an opened XML file, closing all still-opened tags so that it is well-formed.

**xf**: /xmlf_t/: XML File handle

* `xml_NewElement`

Open a new element tag

**name**: /string/:
 Name of tag (for namespaced output, you need to include the prefix)

* `xml_EndElement`

Close an open tag
**name**: /string/: 
 Name of tag to be closed (if it doesn't match currently open tag, you'll get an error)

* `xml_AddAttribute`

Add an attribute to the currently open tag

**name**: /string/: Name of attribute  
**value**: /anytype/: Value of attribute  
(**escape**): /logical/: if the attribute value is a string, should the attribute value be escaped?
  default: true

By default, if the attribute value contains markup characters, they will be escaped automatically by
wxml before output.

However, in some cases you may not wish this to happen - for example if you wish to output Unicode
characters, or entity references. In this case, you should set `escape=.false.` for the relevant
subroutine call.

* `xml_AddCharacters`

Add text data. The data to be added may be of any type; they will be converted to text according to the rules in [REF],
and if they are an array, the elements will all be output, separated by spaces (except if it is a character array, in which
case the delimiter may be changed to any other single character using an optional argument).

**data** /anytype/:
 The text to be output
(**parsed**): /logical/: Should the output characters be parsed (ie should the library replace '&' with '&amp;' etc?) or unparsed (in which case
the characters will be surrounded by CDATA tags.
 *default: yes*
(**delimiter**): /character(1)/: If **data** is a character array, what should the delimiter between elements be on output?
 *default: a single space*

----------
### Namespace-aware functions:

* `xml_AddNamespace`

Add an XML Namespace declaration. This function may be called at any time, and its precise effect depends on when it is called.
If it is called immediately prior to an xml_NewElement call, then the namespace will be declared in that next element, and will therefore take effect in all child elements.

If it is called prior to an xml_NewElement call, but that element has namespaced attributes 

To explain by means of example: In order to generate the following XML output:
    <cml:cml xmlns:cml="http://www.xml-cml.org/schema"/>
then the following two calls are necessary, in the prescribed order:
     xml_AddNamespace(xf, 'cml', 'http://www.xml-cml.org')
     xml_NewElement(xf, 'cml:cml')

However, to generate XML input like so:
     <cml xhtml:class="symbol" xmlns:xhtml="http://www.w3.org/1999/xhtml"/>
that is, where the namespace refers to an attribute at the same level,
then as long as the `xml_AddNamespace` call is made before the element tag is closed (either by xml_EndElement, or by a new element tag being opened, or some text being added etc.) the correct XML will be generated.

**nsURI** /string/: The URI of the namespace
(**prefix**) /prefix/: The namespace prefix to be used in the document. If absent, then the default namespace is affected.

Two previously mentioned functions are affected when used in a namespace-aware fashion.

* `xml_NewElement`, `xml_AddAttribute`

The element or attribute name is checked, and if it is a QName (ie if it is of the form prefix:tagName) then wxml will check that prefix is a
registered namespace prefix, and generate an error if not.

----------

### More rarely used functions:

If you don't know the purpose of any of these, then you don't need to. 


* `xml_AddXMLDeclaration`
(**encoding**) /string/: character encoding of the document
  default: absent
(**standalone**) /logical/: is this document standalone [REF]?
  default: absent

Add XML declaration to the first line of output. If used, then the file must have been opened with `addDecl = .false.`, and this must be the first wxml call to the document.

* `xml_AddDOCTYPE`
**name** /string/: DOCTYPE name
**system** /string/: DOCTYPE SYSTEM ID
(**public**) /string/: DOCTYPE PUBLIC ID

Add an XML document type declaration. If used, this must be used prior to first xml_NewElement call, and only one such call must be made.

* `xml_AddInternalEntity`
**name** /string/: name of internal entity
**value** /string/: value of internal entity

Define an internal entity for the document. If used, this call must be made after `xml_AddDOCTYPE` and before the first xml_NewElement call.

* `xml_AddExternalEntity`
**name** /string/: name of external entity
**system** /string/: SYSTEM ID of external entity
(**public**) /string/: PUBLIC ID of external entity
  default: absent
(**notation**) /string/: notation for external entity
  default: absent

Define an external entity for the document. If used, this call must be made after `xml_AddDOCTYPE` and before the first xml_NewElement call.

* `xml_AddParameterEntity`
**name** /string/: name of parameter entity
(**PEdef**) /string/: definition of parameter entity
  default: absent
(**system**) /string/: SYSTEM ID of parameter entity
  default: absent
(**public**) /string/: PUBLIC ID of parameter entity
  default: absent

Define a parameter entity for the document. If used, this call must be made after `xml_AddDOCTYPE` and before the first xml_NewElement call.

* `xml_AddNotation`
**name** /string/: name of notation
  default: absent
(**system**) /string/: SYSTEM ID of notation
  default: absent
(**public**) /string/: PUBLIC ID of notation
  default: absent

Define a notation for the document. If used, this call must be made after `xml_AddDOCTYPE` and before the first xml_NewElement call.

* `xml_AddXMLStylesheet`

Add XML stylesheet processing instruction, as described in [Stylesheets]. If used, this call must be made before the first xml_NewElement call.

**href** :/string/: 
 address of stylesheet  
**type**: /string/:
 type of stylesheet (generally "text/xsl")  
**title**: /string/:
 title of stylesheet  
**media**: /string:/
 output media type  
**charset**: /string/
 charset of media type  
 **alternate**: /string/:
 alternate

* `xml_AddXMLPI`

Add an XML Processing Instruction.

**name**: /string/:
name of PI  
(**data**): /string/:
data for PI 
(**xml**): /string
  default: false

If data is present, nothing further can be added to the PI. If it is *not* present, then pseudoattributes may be added using the call below.
Normally, the **name** is checked to ensure that it is XML-compliant. This requires that PI targets not start with '[Xx]Mm][Ll]', because such names are reserved. However, some are defined by later W3 specificataions. If you wish to use such PI targets, then set `xml=.true.` when outputting them.

The output PI will look like:
`<?name data?>`

* `xml_AddPseudoAttribute`

Add a pseudoattribute to the currently open PI

**name**: /string/:
 Name of pseudoattribute  
**value**: /anytype/:
 Value of pseudoattribute

* `xml_AddComment`

Add an XML comment

**comment**: /string/
 Contents of comment

* `xml_AddEntityReference`

**entityref**: Entity reference.

This may be used anywhere that xml_AddCharacters may be, and will insert an entity reference into the contents of the XML document at that point.


---------------
###Exceptions

Below is a list of areas where wxml fails to implement the whole of XML 1.1; numerical references below are to the sections in [XML11]

1: XML documents which are not namespace-valid may not be produced; that is, attempts to produce documents which are well-formed according to [XML11] but not namespace-well-formed according to [Namespaces] will fail. 

2: Unicode support[2.2] is practically non-existent. Due to the limitations of Fortran, wxml will directly only emit whatever characters are allowed by the Fortran processor; in general this amounts to only ASCII. Some unicode output is possible through the use of character entities, but only where character data is allowed. No means is offered for output of unicode in attribute values or in XML Names. Unicode character references are checked according to XML-1.1 (2.2)

3: Entity support is not complete[4.1, 4.2, 4.3, 4.4]. All XML entities (parameter, internal, external) may be defined; however, general entities may only be referenced from within a character data section between tags generated with `xml_NewElement` (In principle it should be possible to start the root element from within an entity reference). Furthermore, when an entity reference is added to the document, no check is made of its validity or its contents. (In general, validating all entity references is impossible, but even where possible wxml does not attempt it.) This means that if an entity reference is output, wxml offers no guarantees on the well-formedness of the document, and it will emit a warning to this effect.

4: DTD support is not complete. While a DTD may be output, and entities defined in the internal subset, there is no support for adding Element[3.2] or Attlist[3.3] declarations; nor is there any support for Conditional Sections.[3.4]

---------------
###References

[XML11]: W3C Recommendation, http://www.w3.org/TR/xml11

[Namespaces]: W3C Recommendation, http://www.w3.org/TR/xml-names11

[Stylesheets]: W3C Recommendation, http://www.w3.org/TR/xml-stylesheet

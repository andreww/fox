# SAX

SAX is the "Simple API for XML", and was originally a Java interface for parsing XML.

It is stream-oriented, and based on event callbacks. That is to say, the parser reads the XML document from beginning to end, issueing callbacks when it encounters various XML constructions.

The SAX specification relies on features of the Java language, and can only be implemented in full by Java. However, SAX-style implementations exist for many other languages, and FoX contains one, including those features which are implementable in Fortran.

Specifically, FoX offers all of the callback interfaces offered by the SAX-2 specification, though with no support for excpetions (since Fortran has no exception handling.) Documentation for all of the original Java callback functions may be found at:

javadoc.

The following subroutines are implemented in Fortran, organized in the same order as found in the Java documentation. (all function/subroutine names are appended by `_handler` for ease of comprehension)

Note that for the vast majority of common uses, you will need only
`startElementHandler`
`endElementHandler`
`characters_handler`
and the rest of the callbacks can be ignored.

When reading in documents, FoX understands both XML-1.0 and 1.1; and namespaces 1.0 and 1.1, with only a few exceptions (noted below)

FoX is a non-validating parser - portions of the DTD are therefore ignored - but any failures to correctly diagnose ill-formedness within the constraints of a non-validating parser may be considered a bug in FoX.

<a name="Callbacks"/>

## Listing of available callback functions.

org.cml.sax:
SAX ContentHandler:

characters_handler
endDocument_handler
endElement_handler
endPrefixMapping_handler
ignorableWhitespace_handler
processingInstruction_handler
skippedEntity_handler
startDocument_handler
startElement_handler
startPrefixMapping_handler

SAX DTDHandler
notationDecl_handler
unparsedEntityDecl_handler

SAX ErrorHandler
error_handler
fatalError_handler
warning_handler

org.xml.sax.ext:
SAX DeclHandler
attributeDecl_handler
elemetnDecl_handler
externalEntityDecl_handler
interalEntityDecl_handler

SAX LexicalHandler
comment_handler
endCdata_handler
endDTD_handler
endEntity_handler
startDTD_handler
startEntity_handler


What of SAX is not included in FoX?

The difference betweek Java & Fortran means that none of the SAX APIs can be copied directly. However, FoX offers data types, subroutines, and interfaces covering a large proportion of the facilities offered by SAX. Where it does not, this is mentioned below.


org.sax.xml:
* Querying/setting of feature flags/property values for the XML parser.
* XML filters - Java SAX makes it possible to write filters to intercept the
flow of events. FoX does not support this.
* Namespace configuration - SAX 2 allows changing the ways in which namespaces are interpreted by the parser. FoX supports only the SAX 2 default.
* Entity resolution - SAX 2 exports an interface to the application for entity resolution, but FOX does not - all entities are resolved within the parser.
* Locator - SAX 2 offers an interface to export information regarding object locations within the document, FoX does not.
* XMLReader - FoX only offers the parse() method - no other methods really make sense in Fortran.
* AttributeList/DocumentHandler/Parser - FoX only offers namespace aware attributes, not the pre-namespace versions.

org.sax.xml.ext:
* Attributes2 - FoX does not implement these attribute-declaration querying functions
* EntityResolver2 - see above
* Locator2 - not implemented

org.sax.xml.helpers:
* None of these helper methods are implemented.

What of XML does FoX-SAX  not understand?|

The only standard-compromising exception (other than as-yet-undiscovered bugs) is:

Unicode. Currently, there I haven't written a UTF-8 transcoder for FoX, so it only understands ASCII characters between 0 and 127. Documents containing only these characters will be read correctly, regardless of their encoding.  
If any other characters are encountered, FoX will issue a warnin,g but continue.


The following are optional areas of the XML standard which FoX does not implement:

2: External entities are completely ignored.

3: FoX is non-validating and will not compare the DTD specification to the document contents. However, the DTD is correctly read.  
* entities are treated correctly.  
The DTD is non-exhaustively validated (any validation errors detected can be optionally emitted), and the following aspects are universally implemented.  
* attribute declarations read, and non-CDATA attributes normalized.  
* element declarations are read, and elements of non-mixed type with whitespace text will emit `ignorableWhitespace()` events.  

...


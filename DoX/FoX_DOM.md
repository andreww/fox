# DOM

## Overview

DOM stands for Document Object Model. Any interface which represents the structure of an XML document in memory as a linked structure of objects is a DOM.

In particular, there is a series of W3C specifications ... which describe one particular set of DOM interfaces.

FoX implements essentially all of DOM Core Levels 1 and 2. (There are a number of minor exceptions which are listed  ... ) and a substantial portion of DOM Core Level 3.

* Quick overview of how to map the DOM interface to Fortran: (|DomQuickOverview|)
* More detailed explanation of Fortran interface: (|DomDetailedInterface|)
* Additional (non-DOM) utility functions (|DomUtilityFunctions|)
* String handling: (|DomString|)
* Exception handling: (|DomException|)
* Live nodelists: (|LiveNodelists|)
* DOM Configuration: (|DomConfiguration|)
* Miscellanea: (|DomMiscellanea|)

## Interface Mapping
<a name="DomQuickOverview"/>

The mapping of the specified DOM interface is as follows:

The testsuite assumes the following of a Fortran DOM interface:

1. All DOM objects are available as Fortran types, and should be referenced only as pointers (though see 7 and 8 below). Thus, to use a Node, it must be declared first as:

    type(Node), pointer :: aNode

2. A flat (non-inheriting) object hierarchy is used. All DOM objects which inherit from Node are represented as Node types.

3. All object method calls are modelled as functions or subroutines with the same name, whose first argument is the object. Thus:

    aNodelist = aNode.getElementsByTagName(tagName)

should be converted to Fortran as:

    aNodelist => getElementsByTagName(aNode, tagName)

4. All object method calls whose return type is void are modelled as subroutines. Thus

    aNode.normalize()

becomes

    call normalize(aNode)

5. All object attributes are modelled as a pair of get/set calls (or only get where the attribute is readonly), with the naming convention being merely to prepend get or set to the attribute name. Thus:

    name = node.nodeName
    node.nodeValue = string

should be converted to Fortran as

    name = getnodeName(node)
    call setnodeValue(string)

6. Where an object method or attribute getter returns a DOM object, the relevant Fortran function must always be used as a pointer function. Thus:

    aNodelist => getElementsByTagName(aNode, tagName)

7. No special DOMString object is used - all string operations are done on the standard Fortran character strings, and all functions that return DOMStrings return Fortran character strings

8. Exceptions are modelled by every DOM subroutine/function allowing an optional additional argument, of type DOMException. For further information see (|DomExceptions|) below.



### String handling
<a name="DomString"/>

The W3C DOM requires that a `DOMString` object exist, capable of holding Unicode strings; and that all DOM functions accept and emit DOMString objects when string data is to be transferred.

FoX does not follow this model. Since (as mentioned elsewhere) it is impossible to perform Unicode I/O in standard Fortran, it would be obtuse to require users to manipulate additional objects merely to transfer strings. Therefore, wherever the DOM mandates use of a `DOMString`, FoX merely uses standard Fortran character strings.

All functions or subroutines which expect DOMString input arguments should be used with normal character strings.  
All functions which should return DOMString objects will return Fortran character strings.

## Documenting DOM functions

<a name="DomDetailedInterface"/>

This manual will not exhaustively document the functions available through the `Fox_DOM` interface. Primary documentation may be found in ...

However, in translating these calls to Fortran, certain systematics need to be understood.

### Object model

The DOM is written in terms of an object model involving inheritance, but also permits a flattened model. FoX implements this flattened model - all objects descending from the Node are of the opaque type `Node`. Nodes carry their own type, and inappropriate calls may not be made.

The other types available through the DOM are:

 * `DOMException`  
 * `DOMImplementation`  
 * `NodeList`  
 * `NamedNodeMap`  

### Object attributes and methods

The DOM objects have attributes (some of which are readonly) and methods. These cannot be directly translated into Fortran 95. The following translations should be performed.

 * Attribute access. All access to attributes is through get/set functions. Reading attributes is done by a function `get`Attribute, while writing is done by a subroutine called `set`Attribute.

Thus, for example

* AttribSince Fortran does not permit method calls on objects, nor readonly members of
* Att types, all attribute accesses and methods are done through subroutines or functions which take the requisite object as their first argument. Thus:  
`el.

### FoX DOM and pointers

All DOM objects exposed to the user may only be manipulated through pointers. Attempts to access them directly will reulst in either compile-time or run-time failures according to your environment.

This should have little effect on your programs, except that you must always remember, when calling a DOM function, to perform pointer assignment, not direct assignment, thus:

    child => getFirstChild(parent)

and *not*

    child = getFirstChild(parent)

### Memory handling

Fortran offers no garbage collection facility, so unfortunately a small degree of memory
handling is necessarily exposed to the user.

However, this has been kept to a minimum. FoX keeps track of all memory allocated and used when calling DOM routines, and keeps references to all DOM objects created.

The only memory handling that the user needs to take care of is destroying any
DOM Documents (whether created manually, or by the parse() routine. All other nodes or node structures created will be destroyed automatically by the relevant destroyDocument() call.

As a consequence of this, all DOM objects which are part of a given document will become inaccessible after the document object is destroyed.


## Additional functions.
<a name="DomUtilityFunctions"/>

Several additional utility functions are provided by FoX.

Firstly, to construct a DOM tree, from either a file or a string containing XML data.

* `parseFile`  
**filename**: *string*  
(**configuration**): *string*  
(**ex**): *DOMException*  

**filename** should be an XML document. It will be opened and parsed into a DOM tree. The parsing is performed by the FoX SAX parser; if the XML document is not well-formed, a `PARSE_ERR` exception will be raised. **configuration** is an optional argument - see (|DomConfiguration| for its meaning.

* `parseString`
**XMLstring**: *string*  
(**configuration**): *string*  
(**ex**): *DOMException*  

**XMLstring** should be a string containing XML data. It will be parsed into a DOM tree. The parsing is performed by the FoX SAX parser; if the XML document is not well-formed, a `PARSE_ERR` exception will be raised. **configuration** is an optional argument - see (|DomConfiguration| for its meaning.

Both `parseFile` and `parseString` return a pointer to a `Node` object containing the Document Node.

Secondly, to output an XML document:

* `serialize`
**arg**: *Node, pointer*
**fileName**: *string*

This will open `fileName` and serialize the DOM tree by writing into the file. If `fileName` already exists, it will be overwritten. If an problem arises in serializing the document, then a fatal error will result. (Future versions of FoX will allow trapping this as an exception)


### Exception handling
<a name="DomException"/>

Exception handilng is important to the DOM. The W3C DOM standards provide not only interfaces to the DOM, but also specify the error handling that should take place when invalid calls are made.

The DOM specifies these in terms of a `DOMException` object, which carries a numeric code whose value reports the kind of error generated. Depending upon the features available in a particular computer language, this DOMException object should be generated and thrown, to be caught by the end-user application.

Fortran of course has no mechanism for throwing and catching exceptions. However, the behaviour of an exception can be modelled using Fortran features.

FoX defines an opaque `DOMException` object.
Every DOM subroutine and function implemented by FoX will take an optional argument, 'ex', of type `DOMException`. 

If the optional argument is not supplied, any errors within the DOM will cause an immediate abort, with a suitable error message. However, if the optional argument *is* supplied, then the error will be captured within the `DOMException` object, and returned to the caller for inspection. It is then up to the application to decide how to proceed.

Functions for inspecting and manipulating the `DOMException` object are described below:

* `inException`:  
**ex**: *DOMException*

A function returning a logical value, according to whether `ex` is in exception - that is, whether the last DOM function or subroutine, from which `ex` returned, caused an error.

* `getExceptionCode`  
** ex**: *DOMException*

A function returning an integer value, describing the nature of the exception reported in `ex`. If the integer is 0, then `ex` does not hold an exception. If the integer is less than 200, then the error encountered was of a type specified by the DOM standard; for a full list, see below, and for explanations, see the various DOM standards. If the integer is 200 or greater, then the code represents a FoX-specific error. See the list below.  
Note that calling `getExceptionCode` will clean up all memory associated with the DOMException object.

#### Exception handling and memory usage.

Note that when an Exception is thrown, memory is allocated within the DOMException object. Calling `getExceptionCode` on a DOMEXception will clean up this memory. If you use the exception-handling interfaces of FoX, then you must check every exception, and ensure you check its code, otherwise your program will leak memory.

#### FoX exceptions.

The W3C DOM interface allows the creation of unserializable XML document in various ways. For example, it permits characters to be added to a text node which would be invalid XML. FoX performs multiple additional checks on all DOM calls to prevent the creation of unserializable trees. These are reported hthrough the DOMException mechanisms noted above. However, if for some reason, you want to create such trees, then it is possible to switch off all FoX-only checks. (DOM-mandated checks may not be disabled.) To do this, use the `setFoXChecks` function described in (|DomUtilityFunctions|)

FoX does not currently check for all ways that a tree may be made non-serializable. In particular, it is possible to create non-serializable DOM trees in the following ways.

* `setXMLVersion` (DOM 3 only)  
When changing the XML version, no checking is made that on the charset of all the document nodes, nor of any XML names, to ensure that they are compliant with the standard. Since FoX does not support Unicode, the only relevant incompatibilities between XML 1.0 and 1.1 are in having characters BLAH in text sections, which is permitted in XML 1.0 but not 1.1.

#### DOM Error codes

    INDEX_SIZE_ERR                 = 1;
    DOMSTRING_SIZE_ERR             = 2;
    HIERARCHY_REQUEST_ERR          = 3;
    WRONG_DOCUMENT_ERR             = 4;
    INVALID_CHARACTER_ERR          = 5;
    NO_DATA_ALLOWED_ERR            = 6;
    NO_MODIFICATION_ALLOWED_ERR    = 7;
    NOT_FOUND_ERR                  = 8;
    NOT_SUPPORTED_ERR              = 9;
    INUSE_ATTRIBUTE_ERR            = 10;
    INVALID_STATE_ERR              = 11; // Introduced in DOM Level 2;
    SYNTAX_ERR                     = 12; // Introduced in DOM Level 2;
    INVALID_MODIFICATION_ERR       = 13; // Introduced in DOM Level 2;
    NAMESPACE_ERR                  = 14; // Introduced in DOM Level 2;
    INVALID_ACCESS_ERR             = 15; // Introduced in DOM Level 2;
    VALIDATION_ERR                 = 16; // Introduced in DOM Level 3;
    TYPE_MISMATCH_ERR              = 17; // Introduced in DOM Level 3;
    INVALID_EXPRESSION_ERR         = 51; // Introduced in DOM Level 3 XPath;
    TYPE_ERR                       = 52; // Introduced in DOM Level 3 XPath;
    PARSE_ERR                      = 81; // Introduced in DOM Level 3 Load and Save;
    SERIALIZE_ERR                  = 82; // Introduced in DOM Level 3 Load and Save;

#### FoX Error codes




### Live nodelists
<a name="DomLiveNodelists"/>

The DOM specification requires that all NodeList objects are *live* - that is, that any change in the document structure is immediately reflected in the contents of any nodelists.

For example, any nodelists returned by getElementsByTagName or getElementsByTagNameNS must be updated whenever nodes are added to or removed from the document; and the order of nodes in the nodelists must be changed if the document structure changes.

Though FoX does keep all nodelists live, this can impose a significant performance penalty when manipulating large documents. Therefore, FoX can be instructed to inly use 'dead' nodelists - that is, nodelists which reflect a snapshot of the document structure at the point they were created. To do this, call `setLiveNodeLists` (see API documentation).

However, note that the nodes within the nodelist remain live - any changes made to the nodes will be reflected in accessing them through the nodelist.

Furthermore, since the nodelists are still associated with the document, they and their contents will be rendered inaccessible when the document is destroyed.



## DOM Configuration
<a name="DomConfiguration"/>

Multiple valid DOM trees may be produced from a single document. When parsing input, some of these choices are made availablae to the user.

By default, the DOM tree presented to the user will be canonical (according to BLAH). That is:  
* there will be no adjacent text nodes  
* no Cdata nodes will appear in the DOM tree (they will be represented as Text nodes, and subsumed into neighbouring text nodes)  
* no EntityReference nodes will appear in the DOM tree (they will be fully expanded and their contents appear in the tree), except where the contents of the entity reference are unknown to the parser.

However, if a non-canonical tree is desired, the user may change this. The mechanism for doing this is the optional `configuration` argument to `parseFile` and `parseString`. `configuration` should be a space-separated list of options which affect the DOM tree. Currently, the following options are available:

* `cdata`: If this is specified, the parse tree will preserve any CData sections from the original document.
* `entities`: If this is specified, the parse tree will preserve entity reference nodes (their contents are made available as children of the entity reference node.)

### External entities

The FoX SAX processor does not read external entities,  therefore these will not be made available through the DOM interface, nor will the document represented be affected by any declarations made in referenced external entities (thus, for example, any externally-declared default attributes will not be made available.)


## DOM Miscellanea
<a name="DomMiscellanea"/>

 Other issues

* As mentioned in the documentation for WXML, it is impossible within Fortran to reliably output lines longer than 1024 characters. While text nodes containing such lines may be created in the DOM, on serialization newlines will be inserted as described in the documentation for WXML.

* All caveats with regard to the FoX SAX processor apply to reading documents through the DOM interface. In particular, note that documents containing characters beyond the US-ASCII set will not be readable.




It was decided to implement W3C DOM interfaces primarily because they are specified in a language-agnostic fashion, and thus made Fortran implementation possible. A number of criticisms have been levelled at the W3C DOM (...), but may apply only from the perspective of Java developers. However, also, the W3c DOM suffers from a lack of sufficient rror checking so it is very easy to create a DOM tree, or manipulate an existing DOM treeinto a state, that cannot be serialized into a legal XML document.

Therefore, FoX will by default produce errors about any attempts to manipulate the DOM in such a way as would result in invalid XML. These errors can be switched off if standards-compliant behaviour is wanted. Although extensive, there are a some areas where the nature of the DOM API makes complete checking impractical. These are noted below.

Due to the nature of Fortran, there are a few other areas where the FoX implementation differs from the W3C DOM. These are largely unimportant, but are noted in ...

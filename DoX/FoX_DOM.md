# DOM

## Overview

DOM stands for Document Object Model. Any interface which represents the structure of an XML document in memory as a linked structure of objects is a DOM.

In particular, there is a series of W3C specifications ... which describe one particular set of DOM interfaces.

FoX implements essentially all of DOM Core Levels 1 and 2. (There are a number of minor exceptions which are listed  ... ) and a substantial portion of DOM Core Level 3.

It was decided to implement W3C DOM interfaces primarily because they are specified in a language-agnostic fashion, and thus made Fortran implementation possible. A number of criticisms have been levelled at the W3C DOM (...), but may apply only from the perspective of Java developers. However, also, the W3c DOM suffers from a lack of sufficient rror checking so it is very easy to create a DOM tree, or manipulate an existing DOM treeinto a state, that cannot be serialized into a legal XML document.

Therefore, FoX will by default produce errors about any attempts to manipulate the DOM in such a way as would result in invalid XML. These errors can be switched off if standards-compliant behaviour is wanted. Although extensive, there are a some areas where the nature of the DOM API makes complete checking impractical. These are noted below.

Due to the nature of Fortran, there are a few other areas where the FoX implementation differs from the W3C DOM. These are largely unimportant, but are noted in ...

### Error handling

Error handilng is important to the DOM. The W3C DOM standards provide not only interfaces to the DOM, but also specifies the error handling that should take place when invalid calls are made.

The DOM specifies these in terms of a `DOMException` object, which carries a numeric code whose value reports the kind of error generated. Depending upon the features available in a particular computer language, this DOMException object should be generated and thrown, to be caught by the end-user application.

Fortran of course has no mechanism for throwing and catching exceptions. However, the behaviour of an exception can be modelled using Fortran features.

FoX defines an opaque`DOMException` object.
Every DOM subroutine and function implemented by FoX will take an optional argument, 'ex', of type `DOMException`. 

If the optional argument is not supplied, any errors within the DOM will cause an immediate abort, with a suitable error message. However, if the optional argument *is* supplied, then the error will be captured within the `DOMException` object, and returned to the caller for inspection. It is then up to the application to decide how to proceed.

Functions for inspecting and manipulating the `DOMException` object are described in (|ExceptionHandling|).

### String handling

The W3C DOM requires that a `DOMString` object exist, capable of holding Unicode strings; and that all DOM functions accept and emit DOMString objects when string data is to be transferred.

FoX does not follow this model. Since (as mentioned elsewhere) it is impossible to perform Unicode I/O in standard Fortran, it would be obtuse to require users to manipulate additional objects merely to transfer strings. Therefore, wherever the DOM mandates use of a `DOMString`, FoX merely uses standard Fortran strings.

## Documenting DOM functions

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

### Memory handling

Fortran offers no garbage collection facility, so unfortunately a small degree of memory
handling is necessarily exposed to the user.

However, this has been kept to a minimum. FoX keeps track of all memory allocated and used when calling DOM routines, and keeps references to all DOM objects created.

The only memory handling that the user needs to take care of is destroying any
DOM Documents (whether created manually, or by the parse() routine. All other nodes or node structures created will be destroyed automatically by the relevant destroyDocument() call.

As a consequence of this, all DOM objects which are part of a given document will become inaccessible after the document object is destroyed.

### Live nodelists

The DOM specification requires that all NodeList objects are *live* - that is, that any change in the document structure is immediately reflected in the contents of any nodelists.

For example, any nodelists returned by getElementsByTagName or getElementsByTagNameNS must be updated whenever nodes are added to or removed from the document; and the order of nodes in the nodelists must be changed if the document structure changes.

Though FoX does keep all nodelists live, this can impose a significant performance penalty when manipulating large documents. Therefore, FoX can be instructed to inly use 'dead' nodelists - that is, nodelists which reflect a snapshot of the document structure at the point they were created. To do this, call `setLiveNodeLists` (see API documentation).

However, note that the nodes within the nodelist remain live - any changes made to the nodes will be reflected in accessing them through the nodelist.

Furthermore, since the nodeslists are still associated with the document, they and their contents will be rendered inaccessible when the document is destroyed.

### FoX DOM and pointers

All DOM objects exposed to the user may only be manipulated through pointers. Attempts to access them directly will reulst in either compile-time or run-time failures according to your environment.

This should have little effect on your programs, except that you must always remember, when calling a DOM function, to perform pointer assignment, not direct assignment, thus:

    child => getFirstChild(parent)

and *not*

    child = getFirstChild(parent)

## DOM Particulars:

Note that the FoX Dom is built on top of the FoX SAX processor, which makes certain choices in reporting events. This has repercussions on the DOM tree made available.

### Entity References

The SAX processor fully expands all entity references before reporting events.

Therefore, the DOM tree provided directly by FoX will never contain any EntityReference nodes, regardless of the syntax of the document. However, all entities declared within the document are still accessible through the standard W3C DOM API, and EntityReference nodes may be created referencing them.

### CDATA Sections

The SAX processor converts all CDATA sections to character data before reporting events.

Therefore, the DOM tree provided directly by FoX will never contain any CDATASection nodes, regardless of the syntax of the document. However, CDATASection nodes may be created and appended to the document.

### Multiple text nodes

XML APIs permit processors to represent character data as multiple adjacent items, which may need normalization. The FoX SAX processor reports all character data as single nodes, requiring no normalization, except in the presence of CDATA Sections, which are (although converted to normal text nodes first) always represented as separate text nodes.

Normalization may therefore be required on a DOM tree iif the initial XML document contained CDATA sections.

### External entities

The FoX SAX processor does not read external entities,  therefore these will not be made available through the DOM interface, nor will the document represented be affected by any declarations made in referenced external entities (thus, for example, any externally-declared default attributes will not be made available.)


## Creating non-serializable DOM trees.

It is possible to create non-serializable DOM trees in the following ways.

* `setXMLVersion` (DOM 3 only)  
When changing the XML version, no checking is made that on the charset of all the document nodes, nor of any XML names, to ensure that they are compliant with the standard. Since FoX does not support Unicode, the only relevant incompatibilities between XML 1.0 and 1.1 are in having characters blah blah blah in text sections, which is permitted in XML 1.0 but not 1.1

## Other issues

* As mentioned in the documentation for WXML, it is impossible within Fortran to reliably output lines longer than 1024 characters. While text nodes containing such lines may be created in the DOM, on serialization newlines will be inserted as described in the documentation for WXML.

* All caveats with regard to the FoX SAX processor apply to reading documents through the DOM interface. In particular, note that documents containing characters beyond the US-ASCII set will not be readable.




# DOM

## Overview

DOM stands for Document Object Model. Any interface which represents the structure of an XML document in memory as a linked structure of objects is a DOM.

In particular, there is a series of W3C specifications ... which describe one particular set of DOM interfaces.

FoX implements essentially all of DOM Core Levels 1 and 2. (There are a number of minor exceptions which are listed  ... ) and a substantial portion of DOM Core Level 3.

It was decided to implement W3C DOM interfaces primarily because they are specified in a language-agnostic fashion, and thus made Fortran implementation possible. A number of criticisms have been levelled at the W3C DOM (...), but may apply only from the perspective of Java developers. However, also, the W3c DOM suffers from a lack of sufficient rror checking 0 it is very easy to create a DOM tree, or manipulate an existing DOM tree, that cannot be serialized into a legal XML document.

Therefore, FoX will by default produce errors about any attempts to manipulate the DOM in such a way as would result in invalid XML. These errors can be switched off if standards-compliant behaviour is wanted.

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


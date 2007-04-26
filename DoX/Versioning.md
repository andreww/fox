# FoX versioning

This documentation describes version 2.1 of the FoX library

FoX was originally based on the version 1.2 of the [xmlf90](http://lcdx00.wm.lc.ehu.es/ag/xml/) library, but has since evolved heavily.

This release version includes output modules for general XML, and for CML, and also a Fortran version of the SAX2 input parser interface

This is a stable branch, which will be maintained with important bugfixes, but on which no further major development will occur.

Version 2.1 has support for outputting complete XML documents, with support for all XML objects described in XML11, and XML Namespaces. A detailed description of its precise conformance level is in the WXML documentation.

In addition, there is a large suite of routines available for outputting valid [CML](http://www.xml-cml.org) documents.

There is also a SAX input module, compatible with the SAX 2 standard - pprecise conformance details listed in the SAX documentation.

Input modules are under development for DOM and XPath, and will be released with a later version.

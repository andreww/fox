# FoX_common

FoX_common is a module exporting interfaces to a set of convenience functions common to all of the FoX modules, which are of more general use.

Currently, there are two publically available functions:

*  `str` converts primitive datatypes into strings in a consistent fashion, conformant with the expectations of XML processors.

It is fully described in [StringFormatting](|StringFormatting|)

* `rts` performs the reverse function, taking a string (obtained from an XML document) and converts it into a primitive Fortran datatype.

It is fully described in [StringConversion](|StringConversion|)

# String handling in FoX

Many of the routines in wxml, and indeed in wcml which is built on top of wxml, are overloaded so that data may be passed to the same routine as string, integer, logical or real data.

In such cases, a few notes on the conversion of non-textual data to text is on order. The
standard Fortran I/O formatting routines do not offer the control required for useful XML output, so FoX performs all its own formatting.

### Logical

Logical data is output such that True values are converted to the string 'true', and False to the string 'false'.

### Integer

Integer data is converted to the standard decimal representation.

### Real numbers

Real numbers, both single and double precision, are converted to strings in one of two ways, with some control offered to the user. The output will conform to the real number formats specified by XML Schema Datatypes.

This may be done in one of two ways:

 1. Exponential notation, with variable number of significant figures. Format strings of the form "s*number*"  are accepted, where the *number* is the number of significant figures.

 Thus the number 111, when output with the format "s2" will produce the string "1.1e2".

 The number of significant figures should lie between 1 and the number of digits precision provided by the real kind. If a larger or smaller number is specified, output will be truncated accorsingly. If unspecified, then a sensible default will be chosen.

  This format is not permitted by XML Schema Datatypes 1.0, though it is in 2.0

 2. Non-exponential notation, with variable number of digits after the decimal point. Format strings of the form "r*number*", where *number* is the number of digits after the decimal point.

 Thus the number 111, when output with the format "r2" will produce the string "111.000"

 The number of decimal places must lie between 0 and whatever would output the maximum digits precision for that real kind.  If a larger or smaller number is specified, output will be truncated accorsingly. If unspecified, then a sensible default will be chosen.

 This format is the only one permitted by XML Schema Datatypes 1.0

 If no format is specified, then a default of exponential notation will be used.

 If a format is specified not conforming to either of the two forms above, a run-time error will be generated.


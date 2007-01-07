# Attributes dictionaries.

When parsing XML using the FoX SAX module, attributes are returned contained within a dictionary object.

All of the attribute dictionary objects and functions are exported through FoX_common and FoX_sax - you must USE the module to enable them. The dictionary API is described here.

An attribute dictionary consists of a list of entries, one for each attribute. The entries all have the following pieces of data:

* key - the attribute's full name  
* value - the attribute's value

and for namespaced attributes:

* nsURI - the namespace URI (if any) of the attribute
* localname - the local name of the attribute

------

## Derived types

There is one derived type of interest, `dictionary_t`.

It is opaque - that is, it should only be manipulated through the functions described here.

## Functions

* `len  
   type(dictionary_t), intent(in) :: dict`

Returns an integer with the length of the dictionary, ie the number of dictionary entries.

* `getLength  
   type(dictionary_t), intent(in) :: dict`

Returns an integer with the length of the dictionary, ie the number of dictionary entries. Identical to the `len` function.


* `hasKey  
    type(dictionary_t), intent(in) :: dict  
    character(len=*), intent(in) :: key`

Returns a logical value according to whether the dictionary contains an attribute named `key` or not.


* `hasKey  
    type(dictionary_t), intent(in) :: dict  
    character(len=*), intent(in) :: uri  
    character(len=*), intent(in) :: localname`

Returns a logical value according to whether the dictionary contains an attribute with the correct `URI` and `localname`.

The following functions may be used to retrieve data from a dictionary

* `getQName  
    type(dictionary_t), intent(in) :: dict   
    integer, intent(in) :: i`

Return the full name of the `i`th dictionary entry.

* `getValue  
    type(dictionary_t), intent(in)  
    integer, intent(in) :: i`

If an integer is passed in - the value of the `i`th attribute. 

* `getValue  
    type(dictionary_t), intent(in)  
    character(len=*), intent(in) :: key`

If a single string is passed in, the value of the attribute with that name.

* `getValue  
    type(dictionary_t), intent(in)  
    character(len=*), intent(in) :: uri, localname`

If two strings are passed in, the value of the attribute with that uri and localname.

* `getURI  
    type(dictionary_t), intent(in)  
    integer, intent(in), optional :: i`

Returns a string containing the nsURI of the `i`th attribute.

* `getlocalName  
    type(dictionary_t), intent(in)  
    integer, intent(in), optional :: i`

Returns a string containing the localName of the `i`th attribute.

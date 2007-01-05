# Attributes dictionaries.

When parsing XML using the FoX SAX module, attributes are returned contained within a dictionary object.

Any subroutines that have to deal the dictionary object must use the FoX_common module, and it is manipulated using the API described below.

Conceptually, the dictionary is comprised of a series of entries, one for each attribute, each of which has the following associated data:

* key - the attribute's name  
* value - the attribute's value

and for namespaced attributes:

* prefix - the prefix (if any) of the attribute
* nsURI - the namespace URI (if any) of the attribute
* localname - the local name of the attribute

------

## Derived types

There is one derived type of interest, `dictionary_t`.

It is opaque - that is, it should only be manipulated through the functions described here.

## Functions

* `len`
   type(dictionary_t), intent(in) :: dict

Returns an integer with the length of the dictionary, ie the number of dictionary entries.

* `has_key`
    type(dictionary_t), intent(in) :: dict
    character(len=*), intent(in) :: key

Returns a logical value according ot whether the dictionary contains an attribute named `key` or not.

The following functions may be used to retrieve data from a dictionary

* `getKey`
    type(dictionary_t), intent(in) :: dict  
    integer, intent(in) :: i

Return the key of the `i`th dictionary entry.

* `getValue`
    type(dictionary_t), intent(in)
    integer, intent(in), optional :: i
    character(len=*), intent(in), optional :: value

If an integer is passed in - the value of the `i`th attribute. If a string is passed in, the value of the attribute with that key.

* `getPrefix`
    type(dictionary_t), intent(in)
    integer, intent(in), optional :: i

Returns a string containing the prefix of the `i`th attribute.

* `getnsURI`
    type(dictionary_t), intent(in)
    integer, intent(in), optional :: i

Returns a string containing the nsURI of the `i`th attribute.

* `getlocalName`
    type(dictionary_t), intent(in)
    integer, intent(in), optional :: i

Returns a string containing the localName of the `i`th attribute.

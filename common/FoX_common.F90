module FoX_common

  use m_common_array_str
  use m_common_parse_input
  use m_common_format
#ifndef DUMMYLIB
  use m_common_attrs
#endif

  implicit none
  private

#ifdef DUMMYLIB
  character(len=*), parameter :: FoX_version = '3.1.2-dummy'
#else
  character(len=*), parameter :: FoX_version = '3.1.2'
#endif

  public :: FoX_version

  public :: str
  public :: operator(//)

  public :: rts

#ifndef DUMMYLIB
  public :: str_vs
  public :: vs_str
  public :: alloc
  public :: concat
  public :: vs_logical
  public :: vs_int
  public :: vs_real
  public :: logical
  public :: int
  public :: real

!These are all exported through SAX now
  public :: dictionary_t
!SAX functions
  public :: getIndex 
  public :: getLength
  public :: getLocalName
  public :: getQName
  public :: getURI
  public :: getValue
  public :: getType
! Additional non-sax function that's pretty useful
  public :: getSpecified
!For convenience
  public :: len
  public :: hasKey

  public :: print_dict
#endif

end module FoX_common

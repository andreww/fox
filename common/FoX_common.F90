module FoX_common

  use fox_m_fsys_array_str
#ifndef DUMMYLIB
  use m_common_attrs
  use fox_m_fsys_format
#endif

  implicit none
  private

#ifdef DUMMYLIB
  character(len=*), parameter :: FoX_version = '3.2.0-dummy'
#else
  character(len=*), parameter :: FoX_version = '3.2.0'
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

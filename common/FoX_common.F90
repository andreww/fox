module FoX_common

  use fox_m_fsys_array_str
  use fox_m_fsys_format
  use m_common_parse_input
  use m_common_attrs

  implicit none
  private

#ifdef DUMMYLIB
  character(len=*), parameter :: FoX_version = '3.2.0-dummy'
#else
  character(len=*), parameter :: FoX_version = '3.2.0'
#endif

  public :: FoX_version

  public :: rts
  public :: str
  public :: operator(//)

#ifndef DUMMYLIB
  public :: str_vs
  public :: vs_str
  public :: alloc
  public :: concat
#endif

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

end module FoX_common

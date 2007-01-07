module FoX_common

  use m_common_attrs
  use m_common_format

  implicit none
  private

  character(len=*), parameter :: FoX_version = '2.1.0'

  public :: FoX_version

  public :: str
  public :: operator(//)

  public :: dictionary_t
  public :: getLength
  public :: print_dict 
  public :: get_key
  public :: get_value
  public :: has_key
  public :: get_nsURI
  public :: get_localName

end module FoX_common

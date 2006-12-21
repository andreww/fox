module FoX_common

  use m_common_attrs
  use m_common_format

  implicit none
  private

  character(len=*), parameter :: FoX_version = '2.0.1'

  public :: FoX_version

  public :: str

  ! FIXME we should rename everythting below.
  ! only necessary for SAX and maybe DOM anyway.

  public :: dictionary_t
  public :: len
  public :: print_dict 
  public :: get_key
  public :: get_value
  public :: has_key
  public :: get_nsURI
  public :: get_localName

end module FoX_common

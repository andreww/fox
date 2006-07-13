module FoX_common

  use m_common_attrs

  implicit none
  private

  character(len=*), parameter :: FoX_version = '2.0'

  public :: FoX_version

  public :: dictionary_t
  public :: len
  public :: print_dict 
  public :: get_key
  public :: get_value
  public :: has_key
  public :: get_nsURI
  public :: get_localName
  public :: parse_string_to_dict

end module FoX_common

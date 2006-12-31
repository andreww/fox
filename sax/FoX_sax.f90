module FoX_sax

  use FoX_common
  use m_sax_operate

  implicit none
  private

  public :: open_xml_file
  public :: open_xml_string
  public :: close_xml_t
  public :: sax_parse_go

  public :: xml_t

end module FoX_sax

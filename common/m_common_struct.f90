module m_common_struct

  ! Common parts of an XML document. Shared by both SAX & WXML.

  use m_common_entities, only: entity_list, init_entity_list, destroy_entity_list
  use m_common_notations, only: notation_list, init_notation_list, destroy_notation_list

  implicit none
  private

  type xml_doc_state
    integer :: xml_version
    logical :: standalone
    type(entity_list) :: entityList
    type(entity_list) :: PEList
    type(notation_list) :: nList
  end type xml_doc_state

  public :: xml_doc_state
  
  public :: init_xml_doc_state
  public :: destroy_xml_doc_state

contains
  
  subroutine init_xml_doc_state(xds)
    type(xml_doc_state), intent(inout) :: xds
    call init_entity_list(xds%entityList)
    call init_entity_list(xds%PEList)
    call init_notation_list(xds%nList)
  end subroutine init_xml_doc_state

  subroutine destroy_xml_doc_state(xds)
    type(xml_doc_state), intent(inout) :: xds
    call destroy_entity_list(xds%entityList)
    call destroy_entity_list(xds%PEList)
    call destroy_notation_list(xds%nList)
  end subroutine destroy_xml_doc_state

end module m_common_struct

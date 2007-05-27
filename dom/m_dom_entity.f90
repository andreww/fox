module m_dom_entity

  use m_dom_types, only: Node, createNode
  implicit none
  private
  
  public :: createEntity

contains

  ! Internal function, not part of API

  function createEntity(doc, publicId, systemId, notationName) result(np)
    type(Node), intent(in) :: doc
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    character(len=*), intent(in) :: notationName
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      print*,'internal error in createEntity'
      stop
    endif

    np => createNode(doc, '#entity')
    np%publicId => vs_str_alloc(publicId)
    np%systemId => vs_str_alloc(systemId)
    np%notationName => vs_str_alloc(notationName)

  end function createEntity

end module m_dom_entity

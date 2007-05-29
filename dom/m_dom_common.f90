module m_dom_common

  use m_common_array_str, only: str_vs

  use m_dom_types, only: Node, NamedNodeMap, ATTRIBUTE_NODE, DOCUMENT_TYPE_NODE, &
  NOTATION_NODE, ENTITY_NODE

  implicit none
  private

! Assorted functions with identical signatures despite belonging to different types.

  public :: getName

contains
  
  function getName(arg) result(c)
    type(Node), intent(in) :: arg
    character(size(arg%nodeName)) :: c
    
    if (arg%nodeType/=ATTRIBUTE_NODE .and. &
      arg%nodeType/=DOCUMENT_TYPE_NODE) then
      c = '' ! FIXME error
    endif
    c = str_vs(arg%nodeName)
    
  end function getName

    function getPublicId(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%publicId)) :: c

    if (arg%nodeType/=DOCUMENT_TYPE_NODE .and. &
      arg%nodeType/=NOTATION_NODE .and. &
      arg%nodeType/=ENTITY_NODE) then
      ! FIXME error
      continue
    endif
    c = str_vs(arg%publicId)

  end function getPublicId

  function getSystemId(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%systemId)) :: c

    if (arg%nodeType/=DOCUMENT_TYPE_NODE .and. &
      arg%nodeType/=NOTATION_NODE .and. &
      arg%nodeType/=ENTITY_NODE) then
      ! FIXME error
      continue
    endif
    c = str_vs(arg%systemId)

  end function getSystemId

end module m_dom_common

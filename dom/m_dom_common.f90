module m_dom_common

  use m_common_array_str, only: str_vs

  use m_dom_types, only: Node, NamedNodeMap, ATTRIBUTE_NODE, DOCUMENT_TYPE_NODE

  implicit none
  private

! Assorted functions with identical signatures despite belonging to different types.

  public :: getName

contains
  
  function getName(arg) result(c)
    type(Node), intent(in) :: arg
    character(size(arg%nodeName)) :: c
    
    if (arg%nodeType == ATTRIBUTE_NODE .or. &
      arg%nodeType == DOCUMENT_TYPE_NODE) then
      c = str_vs(arg%nodeName)
    else
      c = '' ! FIXME error
    endif
    
  end function getName

end module m_dom_common

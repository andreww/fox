module m_dom_entity

  use m_common_array_str, only: str_vs

  use m_dom_types, only: Node, ENTITY_NODE

  implicit none
  private
  
  public :: getNotationName

contains

  function getNotationName(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%notationName)) :: c

    if (arg%nodeType/=ENTITY_NODE) then
      ! FIXME error
      continue
    endif
    c = str_vs(arg%notationName)

  end function getNotationName


end module m_dom_entity

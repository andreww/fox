module m_dom_attribute

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_dom_types, only : Node, ATTRIBUTE_NODE
  
  implicit none
  private

  public :: getName
  public :: getSpecified
  public :: getValue
  public :: setValue
  public :: getOwnerElement
  
contains
  
  function getName(attribute) result(c)
    type(Node), intent(in) :: attribute
    character(size(attribute%nodeName)) :: c
    
    if (attribute%nodeType == ATTRIBUTE_NODE) then
      c = str_vs(attribute%nodeName)
    else
      c = '' ! FIXME error
    endif
    
  end function getName


  function getSpecified(attribute) result(p)
    type(Node), intent(in) :: attribute
    logical :: p

    p = attribute%specified
  end function getSpecified


  function getValue(attribute) result(c)
    type(Node), intent(in) :: attribute
    character(size(attribute%nodeValue)) :: c 

    if (attribute%nodeType == ATTRIBUTE_NODE) then
       c = str_vs(attribute%nodeValue)
    else
       c = '' ! FIXME error
    endif

  end function getValue


  subroutine setValue(attribute, value)
    type(Node), intent(inout) :: attribute
    character(len=*), intent(in) :: value

    if (attribute%nodeType == ATTRIBUTE_NODE) then
      deallocate(attribute%nodeValue)
      attribute%nodeValue => vs_str_alloc(value)
    else
      ! FIXME error
    endif

  end subroutine setValue


  function getOwnerElement(attribute) result(np)
    type(Node), intent(in) :: attribute
    type(Node), pointer :: np

    if (attribute%nodeType == ATTRIBUTE_NODE) then
      np => attribute%ownerElement
    else
       ! FIXME error
    endif

  end function getOwnerElement

end module m_dom_attribute

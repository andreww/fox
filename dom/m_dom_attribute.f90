module m_dom_attribute

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_dom_types, only : Node, destroyNode, ATTRIBUTE_NODE, &
    TEXT_NODE, ENTITY_REFERENCE_NODE
  
  implicit none
  private

  !public :: getName
  public :: getSpecified
  public :: getValue
  public :: setValue
  public :: getOwnerElement

  public :: destroyAttribute
  
contains
  
  ! function getName(attribute) result(c) See m_dom_common


  function getSpecified(attribute) result(p)
    type(Node), intent(in) :: attribute
    logical :: p

    p = attribute%specified
  end function getSpecified

  ! FIXME this below is all wrong, should be dealing with text and entityreference children

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

    ! FIXME should resolve any entity references
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


  subroutine destroyAttribute(attr)
    type(Node), pointer :: attr

    type(Node), pointer :: np, np_next

    if (attr%nodeType/=ATTRIBUTE_NODE) then
       ! FIXME error
    endif

    np => attr%firstChild
    do while (associated(np))
      np_next => np%nextSibling
      select case (np%nodeType)
      case (TEXT_NODE)
        call destroyNode(np)
      case (ENTITY_REFERENCE_NODE)
        call destroyNode(np)
        ! Its children will be taken care of elsewhere
        continue
      case default
         !FIXME internal error
        continue
      end select
      call destroyNode(np)
      np => np_next
    enddo

    call destroyNode(attr)
  end subroutine destroyAttribute

end module m_dom_attribute

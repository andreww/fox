TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc

')`'dnl
dnl
TOHW_m_dom_publics(`

  !public :: getName
  public :: getSpecified
  public :: getValue
  public :: setValue
  public :: getOwnerElement

')`'dnl
dnl
TOHW_m_dom_contents(`
  
  ! function getName(attribute) result(c) See m_dom_common


  function getSpecified(attribute) result(p)
    type(Node), intent(in) :: attribute
    logical :: p

    p = attribute%specified
  end function getSpecified
    

  pure function getValue(attribute) result(c)
    type(Node), intent(in) :: attribute
    character(size(attribute%nodeValue)) :: c 

    if (attribute%nodeType/=ATTRIBUTE_NODE) then
       c = "" ! FIXME error
    endif
    c = str_vs(attribute%nodeValue)

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

')`'dnl

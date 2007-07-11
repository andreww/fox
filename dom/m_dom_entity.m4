TOHW_m_dom_publics(`
  
  public :: getNotationName

')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_subroutine(setIllFormed, (np, p))
    type(Node), pointer :: np
    logical, intent(in) :: p

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    np%illFormed = p
  end subroutine setIllFormed

  pure function getNotationName_len(np, p) result(n)
    type(Node), intent(in) :: np
    logical, intent(in) :: p
    integer :: n

    if (p) then
      n = size(np%notationName)
    else
      n = 0
    endif

  end function getNotationName_len

  TOHW_function(getNotationName, (np), c)
    type(Node), pointer :: np
    character(len=getNotationName_len(np, associated(np))) :: c

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (np%nodeType/=ENTITY_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = str_vs(np%notationName)

  end function getNotationName

')`'dnl

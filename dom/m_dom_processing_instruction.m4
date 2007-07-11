TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs

')`'dnl
dnl
TOHW_m_dom_publics(`

  public :: getTarget

')`'dnl
dnl
TOHW_m_dom_contents(`

  pure function getTarget_len(np, p) result(n)
    type(Node), intent(in) :: np
    logical, intent(in) :: p
    integer :: n

    if (p) then
      n = size(np%nodeName)
    else
      n = 0
    endif

  end function getTarget_len

  TOHW_function(getTarget, (np), c)
    type(Node), pointer :: np
    character(len=getTarget_len(np, associated(np))) :: c

    if (np%nodeType/=PROCESSING_INSTRUCTION_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = str_vs(np%nodeName)
  end function getTarget

')`'dnl

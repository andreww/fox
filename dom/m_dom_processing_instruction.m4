TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs

')`'dnl
dnl
TOHW_m_dom_publics(`

  public :: getTarget

')`'dnl
dnl
TOHW_m_dom_contents(`

  function getTarget(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%nodeName)) :: c

    if (arg%nodeType/=PROCESSING_INSTRUCTION_NODE) then
      ! FIXME error
    endif

    c = str_vs(arg%nodeName)
  end function getTarget

')`'dnl

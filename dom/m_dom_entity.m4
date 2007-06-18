TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs

')`'dnl
dnl
TOHW_m_dom_publics(`
  
  public :: getNotationName

')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_function(getNotationName, (arg), c)
    type(Node), intent(in) :: arg
    character(len=size(arg%notationName)) :: c

    if (arg%nodeType/=ENTITY_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = str_vs(arg%notationName)

  end function getNotationName

')`'dnl

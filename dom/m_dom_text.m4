TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc

')`'dnl
dnl
TOHW_m_dom_publics(`  
  public :: splitText
')`'dnl
dnl
TOHW_m_dom_contents(`

  pure function isTextNode(nodeType) result(p)
    integer, intent(in) :: nodeType
    logical :: p

    p = (nodeType==TEXT_NODE.or.nodeType==CDATA_SECTION_NODE)
  end function isTextNode

  TOHW_function(splitText, (arg, offset), np)
    type(Node), pointer :: arg
    integer, intent(in) :: offset

    type(Node), pointer :: np

    character, pointer :: tmp(:)

    if (.not.isTextNode(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (offset<0 .or. offset>size(arg%nodeValue)) then
      TOHW_m_dom_throw_error(INDEX_SIZE_ERR)
    endif

    tmp => arg%nodeValue
    if (arg%nodeType==TEXT_NODE) then
      np => createTextNode(arg%ownerDocument, str_vs(tmp(:offset)))
    elseif (arg%nodeType==CDATA_SECTION_NODE) then
      np => createCdataSection(arg%ownerDocument, str_vs(tmp(:offset)))
    endif
    arg%nodeValue => vs_str_alloc(str_vs(tmp(offset+1:)))     
    deallocate(tmp)
    np => insertBefore(arg%parentNode, np, arg)
   
  end function splitText
                                     
')`'dnl

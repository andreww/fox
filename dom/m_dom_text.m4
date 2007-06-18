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

  TOHW_subroutine(splitText, (arg, offset))
    type(Node), pointer :: arg
    integer, intent(in) :: offset

    type(Node), pointer :: newNode

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
      newNode => createTextNode(arg%ownerDocument, str_vs(tmp(:offset)))
    elseif (arg%nodeType==CDATA_SECTION_NODE) then
      newNode => createCdataSection(arg%ownerDocument, str_vs(tmp(:offset)))
    endif
    arg%nodeValue => vs_str_alloc(str_vs(tmp(offset+1:)))     
    deallocate(tmp)
    newNode => insertBefore(arg%parentNode, newNode, arg)
   
  end subroutine splitText
                                     
')`'dnl

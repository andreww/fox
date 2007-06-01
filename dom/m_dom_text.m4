TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc

')`'dnl
TOHW_m_dom_publics(`  
  public :: splitText
')`'dnl
TOHW_m_dom_contents(`

  pure function isTextNode(nodeType) result(p)
    integer, intent(in) :: nodeType
    logical :: p

    p = (nodeType==TEXT_NODE.or.nodeType==CDATA_SECTION_NODE)
  end function isTextNode

  subroutine splitText(arg, offset)
    type(Node), pointer :: arg
    integer, intent(in) :: offset

    type(Node), pointer :: newNode

    character, pointer :: tmp(:)

    if (isTextNode(arg%nodeType)) then
      tmp => arg%nodeValue
      if (arg%nodeType==TEXT_NODE) then
        newNode => createTextNode(arg%ownerDocument, str_vs(tmp(:offset)))
      elseif (arg%nodeType==CDATA_SECTION_NODE) then
        newNode => createCdataSection(arg%ownerDocument, str_vs(tmp(:offset)))
      endif
      arg%nodeValue => vs_str_alloc(str_vs(tmp(offset+1:)))
      deallocate(tmp)
      newNode => insertBefore(arg%parentNode, newNode, arg)
    else
      ! FIXME error
      continue
    end if
   
  end subroutine splitText
                                     
')`'dnl

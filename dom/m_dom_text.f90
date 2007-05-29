module m_dom_text

  use m_common_array_str, only: str_vs, vs_str_alloc

  use m_dom_document, only: createTextNode
  use m_dom_node, only: insertBefore
  use m_dom_types, only: Node, TEXT_NODE

  implicit none
  private
  
  public :: splitText

contains

  subroutine splitText(arg, offset)
    type(Node), pointer :: arg
    integer, intent(in) :: offset

    type(Node), pointer :: newNode

    character, pointer :: tmp(:)

    if (arg%nodeType == TEXT_NODE) then
      tmp => arg%nodeValue
      newNode => createTextNode(arg%ownerDocument, str_vs(tmp(:offset)))
      arg%nodeValue => vs_str_alloc(str_vs(tmp(offset+1:)))
      deallocate(tmp)
      newNode => insertBefore(arg%parentNode, newNode, arg)
    else
      ! FIXME error
      continue
    end if
   
  end subroutine splitText
                                     
end module m_dom_text

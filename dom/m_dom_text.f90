module m_dom_text

  use m_common_array_str, only: str_vs

  use m_dom_types, only: Node, TEXT_NODE
  !use m_dom_namednodemap
  !use m_dom_nodelist
  !use m_dom_attribute
  !use m_dom_document
  !use m_dom_debug
  !use m_dom_node

  implicit none
  private
  
!  public :: splitText
!contains



!  subroutine splitText(node, offset)
!    type(Node), pointer :: node
!    integer,     intent(in) :: offset
!
!    type(Node), pointer :: parent
!    type(Node), pointer :: newNode
!
!    character(len=(len(node%nodeValue)-offset)) :: tmp
!
!    if (node % nodeType == TEXT_NODE) then
!       tmp = remove(node%nodeValue, offset, len(node%nodeValue)) 
!       newNode => createTextNode(node%ownerDocument, tmp)
!       parent  => node%parentNode
!       newNode => insertBefore(parent, newNode, node)
!    end if
!    
!  end subroutine splitText
                                     
end module m_dom_text

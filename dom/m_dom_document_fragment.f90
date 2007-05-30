module m_dom_document_fragment

  use m_common_array_str, only: str_vs
  use m_dom_nodelist, only: append, pop_nl, destroyNodeList
  use m_dom_types, only: Node, NodeList, destroyNode

  implicit none
  private

  public :: destroyDocumentFragment

contains

  subroutine destroyDocumentFragment(arg)
    type(Node), pointer :: arg

    type(Node), pointer :: np, np_next
    type(NodeList) :: np_stack
    logical :: ascending
    
    ! Use iteration, not recursion, to save stack space.
    call append(np_stack, np)
    ascending = .false.
    do
      print*, 'iterating ...', associated(np), ascending, np_stack%length
      if (ascending) then
        np => pop_nl(np_stack)
        if (np_stack%length==0) then
          exit
        else
          ascending = .false.
        endif
      else if (associated(np%firstChild)) then
        call append(np_stack, np)
        np => np%firstChild
        cycle
      endif
      np_next => np%nextSibling
      print*, 'destroying Node', np%nodeType, str_vs(np%nodeName)
      call destroyNode(np)
      if (associated(np_next)) then
        np => np_next
        cycle
      else
        ascending = .true.
      endif
    enddo
    call destroyNodeList(np_stack)

  end subroutine destroyDocumentFragment

end module m_dom_document_fragment

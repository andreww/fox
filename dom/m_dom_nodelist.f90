module m_dom_nodelist

  use m_dom_types, only: Node, ListNode, NodeList

  implicit none
  private

  public :: item
  public :: append
  
  interface append
    module procedure append_nl
  end interface
  
  interface item
    module procedure item_nl
  end interface
  
contains

  function item_nl(list, index) result(np)
    integer, intent(in)             :: index
    type(NodeList), pointer        :: list
    type(Node), pointer            :: np
    
    np => null()
    if (.not. associated(list)) return

!    if (index > list%length) &
!      FIXME raise an error

    np => list%nodes(index)%this

  end function item_nl

  subroutine append_nl(list, arg)
    type(NodeList), pointer :: list
    type(Node), pointer :: arg

    type(ListNode), pointer :: temp_nl(:)
    integer :: i

    temp_nl => list%nodes
    allocate(list%nodes(size(temp_nl)+1))
    do i = 1, size(temp_nl)
      list%nodes(i)%this => temp_nl(i)%this
    enddo
    deallocate(temp_nl)
    list%nodes(size(list%nodes))%this => arg

    list%length = size(list%nodes)
    
  end subroutine append_nl

end module m_dom_nodelist


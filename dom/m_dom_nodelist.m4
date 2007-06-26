TOHW_m_dom_publics(`

  public :: item
  public :: append
  public :: pop_nl
  public :: remove_nl
  public :: destroyNodeList
  
  interface append
    module procedure append_nl
  end interface
  
  interface item
    module procedure item_nl
  end interface

  interface getLength
    module procedure getLength_nl
  end interface getLength
')`'dnl
dnl
TOHW_m_dom_contents(`

  function item_nl(list, index) result(np)
    type(NodeList), intent(in) :: list
    integer, intent(in) :: index
    type(Node), pointer :: np

    if (index>=0.and.index<list%length)  then
      np => list%nodes(index+1)%this
    else
      np => null()
    endif

  end function item_nl

  subroutine append_nl(list, arg)
    type(NodeList), intent(inout) :: list
    type(Node), pointer :: arg

    type(ListNode), pointer :: temp_nl(:)
    integer :: i

    if (.not.associated(list%nodes)) then
      allocate(list%nodes(1))
      list%nodes(1)%this => arg
      list%length = 1
    else
      temp_nl => list%nodes
      allocate(list%nodes(size(temp_nl)+1))
      do i = 1, size(temp_nl)
        list%nodes(i)%this => temp_nl(i)%this
      enddo
      deallocate(temp_nl)
      list%nodes(size(list%nodes))%this => arg
      list%length = size(list%nodes)
    endif
    
  end subroutine append_nl

  function pop_nl(list) result(np)
    type(NodeList), intent(inout) :: list
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)
    integer :: i

    if (list%length==0) then
      ! FIXME internal error
      continue
    endif

    np => list%nodes(size(list%nodes))%this

    if (list%length==1) then
      deallocate(list%nodes)
      list%length = 0
    else
      temp_nl => list%nodes
      allocate(list%nodes(size(temp_nl)-1))
      do i = 1, size(temp_nl)-1
        list%nodes(i)%this => temp_nl(i)%this
      enddo
      deallocate(temp_nl)
      list%length = size(list%nodes)
    endif
    
  end function pop_nl


  function remove_nl(nl, index) result(np)
    type(NodeList), intent(inout) :: nl
    integer, intent(in) :: index
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)

    integer :: i

    np => nl%nodes(index)%this
! FIXME what if index is too small/too big
    temp_nl => nl%nodes
    allocate(nl%nodes(size(temp_nl)-1))
    do i = 1, index - 1
      nl%nodes(i)%this => temp_nl(i)%this
    enddo
    do i = index + 1, nl%length
      nl%nodes(i-1)%this => temp_nl(i)%this
    enddo
    deallocate(temp_nl)

  end function remove_nl


  function getLength_nl(nl) result(n)
    type(NodeList), intent(in) :: nl
    integer :: n

    n = size(nl%nodes)
  end function getLength_nl

  subroutine destroyNodeList(nl)
    type(NodeList), pointer :: nl
    
    if (associated(nl%nodes)) deallocate(nl%nodes)
    deallocate(nl)
  end subroutine destroyNodeList

')`'dnl

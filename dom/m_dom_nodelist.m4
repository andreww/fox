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

  subroutine updateNodeLists(doc, oldName, newName, oldLocalName, newLocalName, oldNamespaceURI, newNamespaceURI)
    ! When triggered, (by addition or movement of an Element Node, or a change of its nodeName, localName, or namespaceURI)
    ! then see if any nodelists need updated.
    type(Node), pointer :: doc
    character, pointer :: oldName(:), newName(:)
    character, pointer :: oldLocalName(:), newLocalName(:)
    character, pointer :: oldNamespaceURI(:), newNamespaceURI(:)

    type(NodeList), pointer :: nl, temp_nll(:)
    integer :: i, i_t
! FIXME FIXME FIXME

    if (.not.associated(doc%nodelists)) return

    temp_nll => doc%nodelists
    i_t = 0
    do i = 1, size(temp_nll)
      ! A nodelist will need updated if it was keyed to the old or new Names.
      if (temp_nll(i)%element%nodeType==ELEMENT_NODE) then
        if (.not.associated(temp_nll(i)%element%parentNode)) then
          ! We have just removed this element from the tree
          deallocate(temp_nll(i)%nodes)
        endif
      endif
      if (associated(temp_nll(i)%nodeName)) then ! this was made by getElementsByTagName
        if (str_vs(temp_nll(i)%nodeName)==str_vs(oldName) &
          .or. str_vs(temp_nll(i)%nodeName)==str_vs(newName)) then
          ! destroy oldNL
          nl => getElementsByTagName(temp_nll(i)%element, str_vs(temp_nll(i)%nodeName)) ! FIXME mutating nodelist
        endif
      elseif (associated(temp_nll(i)%namespaceURI)) then ! FIXME is this enough - what is nsURI of ns-less nodes?
        ! This was made by getElementsByTagNameNS
        if (str_vs(temp_nll(i)%localName)==str_vs(oldLocalName) &
          .or. str_vs(temp_nll(i)%localName)==str_vs(newLocalName) &
          .or. str_vs(temp_nll(i)%namespaceURI)==str_vs(oldNamespaceURI) &
          .or. str_vs(temp_nll(i)%namespaceURI)==str_vs(newNamespaceURI)) then
          ! destroy newNL
          nl => getElementsByTagNameNS(temp_nll(i)%element, str_vs(temp_nll(i)%localName), str_vs(temp_nll(i)%namespaceURI)) ! FIXME mutating nodelist
        endif
      else
        nl => temp_nll(i)
      endif
      i_t = i_t + 1
    enddo

    allocate(doc%nodelists(i_t))
    doc%nodelists => temp_nll(:i_t)

  end subroutine updateNodeLists

  

')`'dnl

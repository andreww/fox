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

  TOHW_function(item_nl, (list, index), np)
    type(NodeList), pointer :: list
    integer, intent(in) :: index
    type(Node), pointer :: np

    if (.not.associated(list)) then
      TOHW_m_dom_throw_error(FoX_LIST_IS_NULL)
    endif

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

  TOHW_function(pop_nl, (list), np)
    type(NodeList), pointer :: list
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)
    integer :: i

    if (list%length==0) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
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


  TOHW_function(remove_nl, (nl, index), np)
    type(NodeList), intent(inout) :: nl
    integer, intent(in) :: index
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)

    integer :: i

    if (index>nl%length) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    np => nl%nodes(index)%this
    temp_nl => nl%nodes
    allocate(nl%nodes(size(temp_nl)-1))
    nl%length = nl%length - 1 
    do i = 1, index - 1
      nl%nodes(i)%this => temp_nl(i)%this
    enddo
    do i = index, nl%length
      nl%nodes(i)%this => temp_nl(i+1)%this
    enddo
    deallocate(temp_nl)

  end function remove_nl


  subroutine remove_node_nl(nl, np)
    type(NodeList), intent(inout) :: nl
    type(Node), pointer :: np

    integer :: i

    do i = 1, nl%length
      if (associated(nl%nodes(i)%this, np)) exit
    enddo
    np => remove_nl(nl, i)

  end subroutine remove_node_nl


  TOHW_function(getLength_nl, (nl), n)
    type(NodeList), pointer :: nl
    integer :: n

    if (.not.associated(nl)) then
      TOHW_m_dom_throw_error(FoX_LIST_IS_NULL)
    endif

    n = size(nl%nodes)
  end function getLength_nl

  subroutine destroyNodeList(nl)
    type(NodeList), pointer :: nl
    
    if (associated(nl%nodes)) deallocate(nl%nodes)
    if (associated(nl%nodeName)) deallocate(nl%nodeName)
    if (associated(nl%localName)) deallocate(nl%localName)
    if (associated(nl%namespaceURI)) deallocate(nl%namespaceURI)
    deallocate(nl)
  end subroutine destroyNodeList

  subroutine updateNodeLists(doc, oldName, newName, oldLocalName, newLocalName, oldNamespaceURI, newNamespaceURI)
    ! When triggered, (by addition or movement of an Element Node, or a change of its nodeName, localName, or namespaceURI)
    ! then see if any nodelists need updated.
    type(Node), pointer :: doc
    character(len=*) :: oldName, newName
    character(len=*) :: oldLocalName, newLocalName
    character(len=*) :: oldNamespaceURI, newNamespaceURI

    type(NodeList), pointer :: nl, nl_orig
    type(NodeListPtr), pointer :: temp_nll(:)
    integer :: i, i_t
! FIXME FIXME FIXME for DOM level 2

    print*,"...UPDATING NODELISTS"

    if (.not.doc%docExtras%liveNodeLists) return
    if (.not.associated(doc%docExtras%nodelists)) return

    allocate(temp_nll(size(doc%docExtras%nodelists)))
    i_t = 0
    do i = 1, size(doc%docExtras%nodelists)
      ! A nodelist will need updated if it was keyed to the old or new Names.
      nl_orig => doc%docExtras%nodelists(i)%this
      if (nl_orig%element%nodeType==ELEMENT_NODE) then
        if (.not.associated(nl_orig%element%parentNode)) then
          ! We have just removed this element from the tree
          deallocate(nl_orig%nodes)
          cycle
        endif
      endif
      ! we definitely keep this nodelist.
      i_t = i_t + 1
      ! Although all nodes should be searched whatever the result, we should only do the
      ! appropriate sort of search for this list - according to namespaces or not.
      if (associated(nl_orig%nodeName)) then ! this was made by getElementsByTagName
        if (str_vs(nl_orig%nodeName)=="*" &
          .or. str_vs(nl_orig%nodeName)==oldName &
          .or. str_vs(nl_orig%nodeName)==newName) then
          nl => getElementsByTagName(nl_orig%element, str_vs(nl_orig%nodeName))
          ! That appended a nodelist to the end of doc%nodelists. But it does not matter,
          ! the whole of the original nodelists will be thrown away anyway. We do have to do:
          deallocate(nl_orig)
          ! and then grab the new list for our new list of lists.
          temp_nll(i_t)%this => nl
        endif
      elseif (associated(nl_orig%namespaceURI)) then
        ! This was made by getElementsByTagNameNS
        if ((str_vs(nl_orig%localName)=="*".or. &
             str_vs(nl_orig%localName)==oldLocalName.or. &
             str_vs(nl_orig%localName)==newLocalName) &
          .and. &
            (str_vs(nl_orig%namespaceURI)=="*".or. &
             str_vs(nl_orig%namespaceURI)==oldNamespaceURI.or. &
             str_vs(nl_orig%namespaceURI)==newNamespaceURI)) then
          nl => getElementsByTagNameNS(nl_orig%element, str_vs(nl_orig%localName), str_vs(nl_orig%namespaceURI))
          ! That appended a nodelist to the end of doc%nodelists. But it does not matter,
          ! the whole of the original nodelists will be thrown away anyway. We do have to do:
          deallocate(nl_orig)
          ! and then grab the new list for our new list of lists.
          temp_nll(i_t)%this => nl
        endif
      else
        temp_nll(i_t)%this => doc%docExtras%nodelists(i)%this
      endif
    enddo

    !Now, destroy all nodelist pointers from old list:
    do i = 1, size(doc%docExtras%nodelists) !Note, this size may be different if we have done more searches above.
      deallocate(doc%docExtras%nodelists(i)%this)
    enddo
    deallocate(doc%docExtras%nodelists)
    ! Now put everything back from temp_nll, but discard any lost nodelists
    allocate(doc%docExtras%nodelists(i_t))
    do i = 1, i_t
      doc%docExtras%nodelists(i)%this => temp_nll(i)%this
    enddo
    ! And finally, get rid of the temporary list of lists
    deallocate(temp_nll)

  end subroutine updateNodeLists

  

')`'dnl

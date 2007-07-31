TOHW_m_dom_publics(`

  public :: getNamedItem
  public :: getNamedItem_Value
  public :: getNamedItem_Value_length
  public :: setNamedItem
  public :: removeNamedItem
!  public :: item
!  public :: getLength
  public :: getNamedItemNS
  public :: getNamedItemNS_Value
  public :: getNamedItemNS_Value_length
  public :: setNamedItemNS
  public :: removeNamedItemNS

!  public :: append
  public :: setReadOnlyMap
  public :: destroyNamedNodeMap


  interface item
    module procedure item_nnm
  end interface

  interface getLength
    module procedure getLength_nnm
  end interface

')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_function(getNamedItem, (map, name), np)
    type(NamedNodeMap), pointer :: map
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    integer :: i

    if (.not.associated(map)) then
      TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%nodeName)==name) then
        np => map%nodes(i)%this
        return
      endif
    enddo

    np => null()

  end function getNamedItem


  pure function getNamedItem_Value_length(map, name) result(n)
    type(NamedNodeMap), intent(in) :: map
    character(len=*), intent(in) :: name
    integer :: n

    integer :: i

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%nodeName)==name) then
        ! FIXME this needs changed I think to cope with
        ! attributes nodeValue/=value
        n = size(map%nodes(i)%this%nodeValue)
        exit
      endif
    enddo
    n = 0

  end function getNamedItem_Value_length


  TOHW_function(getNamedItem_Value, (map, name), c)
    type(NamedNodeMap), pointer :: map
    character(len=*), intent(in) :: name
    character(len=getNamedItem_Value_length(map, name)) :: c

    integer :: i

    if (.not.associated(map)) then
      TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    c = ""
    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%nodeName)==name) then
        c = getNodeValue(map%nodes(i)%this)
        return
      endif
    enddo

  end function getNamedItem_Value


  TOHW_function(setNamedItem, (map, arg), np)
    type(NamedNodeMap), pointer :: map
    type(Node), pointer :: arg
    type(Node), pointer :: np

    integer :: i

    if (.not.associated(map)) then
      TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (map%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (map%ownerElement%nodeType==ELEMENT_NODE) then
      if (.not.associated(map%ownerElement%ownerDocument, arg%ownerDocument)) then
        TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
      elseif (getNodeType(arg)/=ATTRIBUTE_NODE) then
        !Additional check from DOM 3
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
      endif
    endif
    ! Note that the user can never add to the Entities/Notations
    ! namedNodeMaps, so we do not have any checks for that.

    if (getNodeType(arg)==ATTRIBUTE_NODE) then
      call setSpecified(arg, .true.)
      ! This is the normal state of affairs. Always the
      ! case when a user calls this routine. But m_dom_parse
      ! will call with notations & entities
      if (associated(map%ownerElement, getOwnerElement(arg))) then
        np => arg
        return
        ! Nothing to do, this attribute is already in this element
      elseif (associated(getOwnerElement(arg))) then
        TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)    
      endif
    endif

    np => null()
    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%nodeName)==str_vs(arg%nodeName)) then
        np => map%nodes(i)%this
        map%nodes(i)%this => arg
        arg%elExtras%ownerElement => map%ownerElement
        exit
      endif
    enddo

    !   If not found, insert it at the end of the linked list
    if (.not.associated(np)) call append_nnm(map, arg)

    if (map%ownerElement%nodeType==ELEMENT_NODE) then
      if (getGCstate(getOwnerDocument(map%ownerElement))) then
        ! We need to worry about importing this node
        if (map%ownerElement%inDocument) then
          if (.not.arg%inDocument) &
            call putNodesInDocument(getOwnerDocument(map%ownerElement), arg)
          if (associated(np)) &
            call removeNodesFromDocument(getOwnerDocument(map%ownerElement), np)
        else
          if (arg%inDocument) &
            call removeNodesFromDocument(getOwnerDocument(map%ownerElement), arg)
          endif
      endif
    endif
    ! Otherwise we only ever setNNM when building the doc, so we know this
    ! does not matter

  end function setNamedItem


  TOHW_function(removeNamedItem, (map, name), np)
    type(NamedNodeMap), pointer :: map
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i2

    if (.not.associated(map)) then
      TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    if (map%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%nodeName)==name) then
        ! Grab this node
        np => map%nodes(i)%this
        ! and shrink the node list
        temp_nl => map%nodes
        allocate(map%nodes(size(temp_nl)-1))
        do i2 = 1, i - 1
          map%nodes(i2)%this => temp_nl(i2)%this
        enddo
        do i2 = i + 1, map%length
          map%nodes(i2-1)%this => temp_nl(i2)%this
        enddo
        map%length = size(map%nodes)
        deallocate(temp_nl)
        if (np%inDocument.and.getGCstate(getOwnerDocument(map%ownerElement))) &
          call removeNodesFromDocument(getOwnerDocument(map%ownerElement), np)
        !otherwise we are only going to destroy these nodes anyway,
        ! and finish
        return
      endif
    enddo

    !FIXME if this was an attribute we may have to replace with default value

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function removeNamedItem


  TOHW_function(item_nnm, (map, index), np)
    type(NamedNodeMap), pointer :: map
    integer, intent(in) :: index
    type(Node), pointer :: np
    
    if (.not.associated(map)) then
      TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    if (index<0 .or. index>map%length-1) then
      np => null()
    else
      np => map%nodes(index+1)%this
    endif

   end function item_nnm

  TOHW_function(getLength_nnm, (map), n)
    type(namedNodeMap), pointer :: map
    integer :: n

    if (.not.associated(map)) then
       TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    n = map%length
    
  end function getLength_nnm


  TOHW_function(getNamedItemNS, (map, namespaceURI, localName), np)
    type(NamedNodeMap), pointer :: map
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(Node), pointer :: np

    integer :: i

    if (.not.associated(map)) then
      TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    elseif (map%ownerElement%nodeType/=ELEMENT_NODE) then
      np => null()
      return
    endif

    do i = 1, getLength(map)
      if ((getNamespaceURI(item(map, i-1))==namespaceURI &
        .and. getLocalName(item(map, i-1))==localName) &
! FIXME the DOM standard is contradictory on this point ...
        .or. (namespaceURI=="" .and.getNodeName(item(map, i-1))==localName)) then
        np => item(map, i-1)
        return
      endif
    enddo
    
    np => null()

  end function getNamedItemNS


  pure function getNamedItemNS_Value_length(map, p, namespaceURI, localName) result(n)
    type(NamedNodeMap), intent(in) :: map
    logical, intent(in) :: p
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    integer :: n

    integer :: i

    n = 0
    if (.not.p) return
    if (map%ownerElement%nodeType/=ELEMENT_NODE) return

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%elExtras%namespaceURI)==namespaceURI &
        .and. str_vs(map%nodes(i)%this%elExtras%localName)==localName) then
        n = getValue_len(map%nodes(i)%this, .true.)
        exit
      endif
    enddo

  end function getNamedItemNS_Value_length


  TOHW_function(getNamedItemNS_Value, (map, namespaceURI, localName), c)
    type(NamedNodeMap), pointer :: map
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    character(len=getNamedItemNS_Value_length(map, associated(map), namespaceURI, localName)) :: c

    integer :: i

    if (.not.associated(map)) then
       TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    c = ""
    if (map%ownerElement%nodeType/=ELEMENT_NODE) return
    do i = 1, getLength(map)
      if (getNamespaceURI(item(map, i-1))==namespaceURI &
        .and. getLocalName(item(map, i-1))==localName) then
        c = getValue(item(map, i-1))
        return
      endif
    enddo

  end function getNamedItemNS_Value


  TOHW_function(setNamedItemNS, (map, arg), np)
    type(NamedNodeMap), pointer :: map
    type(Node), pointer :: arg
    type(Node), pointer :: np

    integer :: i

    if (.not.associated(map)) then
      TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (map%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (map%ownerElement%nodeType==ELEMENT_NODE) then
      if (.not.associated(map%ownerElement%ownerDocument, arg%ownerDocument)) then
        TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
      elseif (getNodeType(arg)/=ATTRIBUTE_NODE) then
        !Additional check from DOM 3
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
      endif
    endif
    ! Note that the user can never add to the Entities/Notations
    ! namedNodeMaps, so we do not have any checks for that.

    if (getNodeType(arg)==ATTRIBUTE_NODE) then
      call setSpecified(arg, .true.)
      ! This is the normal state of affairs. Always the
      ! case when a user calls this routine. But m_dom_parse
      ! will call with notations & entities
      if (associated(map%ownerElement, getOwnerElement(arg))) then
        np => arg
        return
        ! Nothing to do, this attribute is already in this element
      elseif (associated(getOwnerElement(arg))) then
        ! FIXME unless arg is being set to itself
        TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)    
      endif
    endif

    np => null()
    do i = 1, getLength(map)
      if (getNamespaceURI(item(map, i-1))==getNamespaceURI(arg) &
        .and. getLocalName(item(map, i-1))==getLocalName(arg)) then
        np => item(map, i-1)
        map%nodes(i)%this => arg
        arg%elExtras%ownerElement => map%ownerElement
        exit
      endif
    enddo

    !   If not found, insert it at the end of the linked list
    if (.not.associated(np)) call append_nnm(map, arg)

    if (map%ownerElement%nodeType==ELEMENT_NODE) then
      if (getGCstate(getOwnerDocument(map%ownerElement))) then
        ! We need to worry about importing this node
        if (map%ownerElement%inDocument) then
          if (.not.arg%inDocument) &
            call putNodesInDocument(getOwnerDocument(map%ownerElement), arg)
        else
          if (arg%inDocument) &
            call removeNodesFromDocument(getOwnerDocument(map%ownerElement), arg)
        endif
      endif
    endif
    
  end function setNamedItemNS


  TOHW_function(removeNamedItemNS, (map, namespaceURI, localName), np)
    type(NamedNodeMap), pointer :: map
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i2

    if (.not.associated(map)) then
      TOHW_m_dom_throw_error(FoX_MAP_IS_NULL)
    endif

    if (map%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    do i = 1, getLength(map)
      if (getNamespaceURI(item(map, i-1))==namespaceURI &
        .and. getLocalName(item(map, i-1))==localName) then
        ! Grab this node
        np => item(map, i-1)
        ! and shrink the node list
        temp_nl => map%nodes
        allocate(map%nodes(size(temp_nl)-1))
        do i2 = 1, i - 1
          map%nodes(i2)%this => temp_nl(i2)%this
        enddo
        do i2 = i + 1, map%length
          map%nodes(i2-1)%this => temp_nl(i2)%this
        enddo
        map%length = size(map%nodes)
        deallocate(temp_nl)
        if (np%inDocument.and.getGCstate(getOwnerDocument(map%ownerElement))) &
          call removeNodesFromDocument(getOwnerDocument(map%ownerElement), np)
        !otherwise we are only going to destroy these nodes anyway,
        ! and finish
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

    ! FIXME if this was attribute we may have to replace by default

  end function removeNamedItemNS


  subroutine append_nnm(map, arg)
    type(namedNodeMap), pointer :: map
    type(node), pointer :: arg

    type(ListNode), pointer :: temp_nl(:)
    integer :: i

    if (.not.associated(map%nodes)) then
      allocate(map%nodes(1))
      map%nodes(1)%this => arg
      map%length = 1
    else
      temp_nl => map%nodes
      allocate(map%nodes(size(temp_nl)+1))
      do i = 1, size(temp_nl)
        map%nodes(i)%this => temp_nl(i)%this
      enddo
      deallocate(temp_nl)
      map%nodes(size(map%nodes))%this => arg
      map%length = size(map%nodes)
    endif
    if (getNodeType(arg)==ATTRIBUTE_NODE) arg%elExtras%ownerElement => map%ownerElement

  end subroutine append_nnm


  subroutine setReadOnlyMap(map, r)
    type(namedNodeMap), pointer :: map
    logical, intent(in) :: r

    map%readonly = r
  end subroutine setReadOnlyMap

  subroutine destroyNamedNodeMap(map)
    type(namedNodeMap), pointer :: map

    if (associated(map%nodes)) deallocate(map%nodes)
    deallocate(map)
 end subroutine destroyNamedNodeMap

')`'dnl

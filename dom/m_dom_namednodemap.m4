TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs
  use m_dom_error, only: INUSE_ATTRIBUTE_ERR

')`'dnl
dnl
TOHW_m_dom_publics(`

  public :: getNamedItem
  public :: getNamedItem_Value
  public :: getNamedItem_Value_length
  public :: setNamedItem
  public :: removeNamedItem
  public :: item
!  public :: getLength
  public :: getNamedItemNS
  public :: getNamedItemNS_Value
  public :: getNamedItemNS_Value_length
  public :: setNamedItemNS
  public :: removeNamedItemNS

  public :: append
  public :: setReadOnly
  public :: destroyNamedNodeMap

  interface append
    module procedure append_nnm
  end interface

  interface item
    module procedure item_nnm
  end interface

  interface getLength
    module procedure getLength_nnm
  end interface

')`'dnl
dnl
TOHW_m_dom_contents(`

  function getNamedItem(map, name) result(np)
    type(NamedNodeMap), intent(in) :: map
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    integer :: i

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
        n = size(map%nodes(i)%this%nodeValue)
        exit
      endif
    enddo
    n = 0

  end function getNamedItem_Value_length


  pure function getNamedItem_Value(map, name) result(c)
    type(NamedNodeMap), intent(in) :: map
    character(len=*), intent(in) :: name
    character(len=getNamedItem_Value_length(map, name)) :: c

    integer :: i

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%nodeName)==name) then
        c = str_vs(map%nodes(i)%this%nodeValue)
        return
      endif
    enddo
    c = ""

  end function getNamedItem_Value


  TOHW_function(setNamedItem, (map, arg), np)
    type(NamedNodeMap), intent(inout) :: map
    type(Node), pointer :: arg
    type(Node), pointer :: np

    integer :: i

    if (map%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.associated(map%ownerElement%ownerDocument, arg%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    elseif (associated(arg%ownerElement)) then
      TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)    
    endif

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%nodeName)==str_vs(arg%nodeName)) then
        np => map%nodes(i)%this
        map%nodes(i)%this => arg
        return
      endif
    enddo

    !   If not found, insert it at the end of the linked list
    np => null()
    call append(map, arg)

    if (.not.map%ownerElement%ownerDocument%xds%building) then
      ! We need to worry about importing this node
      if (map%ownerElement%inDocument) then
        if (.not.arg%inDocument) &
          call putNodesInDocument(map%ownerElement%ownerDocument, arg)
      else
        if (arg%inDocument) &
          call removeNodesFromDocument(map%ownerElement%ownerDocument, arg)
        endif
    endif

  end function setNamedItem


  TOHW_function(removeNamedItem, (map, name), np)
    type(NamedNodeMap), intent(inout) :: map
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i2

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
          map%nodes(i2)%this => temp_nl(i2)%this
        enddo
        map%length = size(map%nodes)
        deallocate(temp_nl)
        if (.not.map%ownerElement%ownerDocument%xds%building) &
          call removeNodesFromDocument(map%ownerElement%ownerDocument, np)
        !otherwise we are only going to destroy these nodes anyway,
        ! and finish
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function removeNamedItem


  function item_nnm(map, index) result(np)
    type(NamedNodeMap), intent(in) :: map
    integer, intent(in) :: index
    type(Node), pointer :: np
    
    integer :: n

    if (index<0 .or. index>map%length-1) then
      np => null()
    else
      np => map%nodes(index+1)%this
    endif

   end function item_nnm

  function getLength_nnm(map) result(n)
    type(namedNodeMap), intent(in) :: map
    integer :: n

    n = map%length
    
  end function getLength_nnm


  function getNamedItemNS(map, namespaceURI, localName) result(np)
    type(NamedNodeMap), intent(in) :: map
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(Node), pointer :: np

    integer :: i

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%namespaceURI)==namespaceURI &
        .and. str_vs(map%nodes(i)%this%localName)==localName) then
        np => map%nodes(i)%this
        return
      endif
    enddo
    
    np => null()

  end function getNamedItemNS


  pure function getNamedItemNS_Value_length(map, namespaceURI, localName) result(n)
    type(NamedNodeMap), intent(in) :: map
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    integer :: n

    integer :: i

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%namespaceURI)==namespaceURI &
        .and. str_vs(map%nodes(i)%this%localName)==localName) then
        n = size(map%nodes(i)%this%nodeValue)
        exit
      endif
    enddo
    n = 0

  end function getNamedItemNS_Value_length


  pure function getNamedItemNS_Value(map, namespaceURI, localName) result(c)
    type(NamedNodeMap), intent(in) :: map
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    character(len=getNamedItemNS_Value_length(map, namespaceURI, localName)) :: c

    integer :: i

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%namespaceURI)==namespaceURI &
        .and. str_vs(map%nodes(i)%this%localName)==localName) then
        c = str_vs(map%nodes(i)%this%nodeValue)
        return
      endif
    enddo
    !FIXME error here

  end function getNamedItemNS_Value


  TOHW_function(setNamedItemNS, (map, arg), np)
    type(NamedNodeMap), intent(inout) :: map
    type(Node), pointer :: arg
    type(Node), pointer :: np

    integer :: i

    if (map%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.associated(map%ownerElement%ownerDocument, arg%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    elseif (associated(arg%ownerElement)) then
      TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)    
    endif

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%namespaceURI)==str_vs(arg%namespaceURI) &
        .and. str_vs(map%nodes(i)%this%localName)==str_vs(arg%localName)) then
        np => map%nodes(i)%this
        map%nodes(i)%this => arg
        return
      endif
    enddo
    !   If not found, insert it at the end of the linked list
    np => null()
    call append(map, arg)

  end function setNamedItemNS


  TOHW_function(removeNamedItemNS, (map, namespaceURI, localName), np)
    type(NamedNodeMap), intent(inout) :: map
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i2

    if (map%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    do i = 1, map%length
      if (str_vs(map%nodes(i)%this%namespaceURI)==namespaceURI &
        .and. str_vs(map%nodes(i)%this%localName)==localName) then
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
        call removeNodesFromDocument(map%ownerElement%ownerDocument, np)
        ! and finish
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function removeNamedItemNS


  subroutine append_nnm(map, arg)
    type(namedNodeMap), intent(inout) :: map
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

  end subroutine append_nnm


  subroutine setReadOnly(map, r)
    type(namedNodeMap), intent(inout) :: map
    logical, intent(in) :: r

    map%readonly = r
  end subroutine setReadOnly

  subroutine destroyNamedNodeMap(map)
    type(namedNodeMap), pointer :: map

    if (associated(map%nodes)) deallocate(map%nodes)
    deallocate(map)
 end subroutine destroyNamedNodeMap

')`'dnl

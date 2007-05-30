module m_dom_namednodemap

  use m_common_array_str, only: str_vs
  use m_dom_types, only: Node, NamedNode, NamedNodeMap, ATTRIBUTE_NODE
  
  implicit none
  private

  ! FIXME always create maps separately, don't inline all the ifs.
  ! Raise appropriate errors

  public :: getNamedItem
  public :: setNamedItem
  public :: removeNamedItem
  public :: item
  public :: getLength
  public :: getNamedItemNS
  public :: setNamedItemNS
  public :: removeNamedItemNS

  public :: append

  interface append
    module procedure append_nnm
  end interface

  interface item
    module procedure item_nnm
  end interface

  interface getLength
    module procedure getLength_nnm
  end interface

contains

  function getNamedItem(map, name) result(np)
    type(NamedNodeMap), intent(in) :: map
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(NamedNode), pointer :: nnp

    np => null()

    nnp => map%head
    do while (associated(nnp))
      if (str_vs(nnp%this%nodeName)==name) then
        np => nnp%this
        exit
      endif
      nnp => nnp%next
    enddo

    ! FIXME exceptions

  end function getNamedItem

  function setNamedItem(map, arg) result(np)
    type(NamedNodeMap), intent(inout) :: map
    type(Node), pointer :: arg
    type(Node), pointer :: np

    type(NamedNode), pointer :: nnp

    np => null()
    if (.not.associated(map%head)) then
      call append(map, arg)
    else
      nnp => map%head
      do while (associated(nnp)) 
        if (str_vs(nnp%this%nodeName) == str_vs(arg%nodeName)) then
          np => nnp%this
          nnp%this => arg
          return
        endif
        nnp => nnp%next
      enddo
      !   If not found, insert it at the end of the linked list
      call append(map, arg)
    endif

  end function setNamedItem

  function removeNamedItem(map, name) result(np)
    type(NamedNodeMap), intent(inout) :: map
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(NamedNode), pointer :: nnp, previous

    np => null()

    previous => null()
    nnp => map%head
    do while (associated(nnp)) 
      if (str_vs(nnp%this%nodeName)==name) then
        if (nnp%this%nodeType==ATTRIBUTE_NODE) then
          ! FIXME if this is attribute node removed, replace with default one
          continue
        endif
        np => nnp%this
        if (associated(nnp, map%head)) then
          map%head => nnp%next
        elseif (associated(nnp, map%tail)) then
          previous%next => null()
        else
          previous%next => nnp%next
        endif
        map%length = map%length + 1
        return
      endif
      previous => nnp
      nnp => nnp%next
    enddo
    ! We haven't found the named node
    ! FIXME raise an error

  end function removeNamedItem

  function item_nnm(map, index) result(np)
    type(NamedNodeMap), intent(in) :: map
    integer, intent(in) :: index
    type(Node), pointer :: np
    
    type(NamedNode), pointer :: nnp
    integer :: n

    np => null()

    ! FIXME exceptions

    nnp => map%head
    n = -1
    do 
      if (.not. associated(nnp))  exit
      n = n + 1
      if (n == index) then
        np => nnp%this
        exit
      endif
      nnp => nnp%next
    enddo
     
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

    type(NamedNode), pointer :: nnp

    np => null()

    nnp => map%head
    do while (associated(nnp))
      if (str_vs(nnp%this%namespaceURI)==namespaceURI .and. &
        str_vs(nnp%this%namespaceURI)==localName) then
        np => nnp%this
        exit
      endif
      nnp => nnp%next
    enddo

    ! FIXME exceptions

  end function getNamedItemNS

  function setNamedItemNS(map, arg) result(np)
    type(NamedNodeMap), intent(inout) :: map
    type(Node), pointer :: arg
    type(Node), pointer :: np

    type(NamedNode), pointer :: nnp

    np => null()
    if (.not.associated(map%head)) then
      call append(map, arg)
    else
      nnp => map%head
      do while (associated(nnp)) 
        if (str_vs(nnp%this%namespaceURI) == str_vs(arg%namespaceURI) .and. &
          str_vs(nnp%this%localName) == str_vs(arg%localName)) then
          np => nnp%this
          nnp%this => arg
          return
        endif
        nnp => nnp%next
      enddo
      !   If not found, insert it at the end of the linked list
      call append(map, arg)
    endif

  end function setNamedItemNS

  function removeNamedItemNS(map, namespaceURI, localName) result(np)
    type(NamedNodeMap), intent(inout) :: map
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(Node), pointer :: np

    type(NamedNode), pointer :: nnp, previous

    np => null()

    previous => null()
    nnp => Map%head
    do while (associated(nnp))
      if (str_vs(nnp%this%namespaceURI) == namespaceURI .and. &
        str_vs(nnp%this%localName) == localName) then
        if (nnp%this%nodeType==ATTRIBUTE_NODE) then
          ! FIXME if this is attribute node removed, replace with default one
          continue
        endif
        np => nnp%this
        if (associated(nnp, map%head)) then
          map%head => nnp%next
        elseif (associated(nnp, map%tail)) then
          previous%next => null()
        else
          previous%next => nnp%next
        endif
        map%length = map%length + 1
        return
      endif
      previous => nnp
      nnp => nnp%next
    enddo
    ! We haven't found the named node
    ! FIXME raise an error

  end function removeNamedItemNS


  subroutine append_nnm(map, arg)
    type(namedNodeMap), intent(inout) :: map
    type(node), pointer :: arg

    if (.not.associated(map%head)) then
       allocate(map%head)
       map%head%this => arg
       map%tail => map%head
       map%length = 1
    else
       allocate(map%tail%next)
       map%tail%next%this => arg
       map%tail => map%tail%next
       map%length = map%length + 1
    endif

  end subroutine append_nnm

end module m_dom_namednodemap


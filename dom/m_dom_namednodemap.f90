module m_dom_namednodemap

  use m_common_array_str, only: str_vs
  use m_dom_types, only: Node, NamedNode, NamedNodeMap
  
  implicit none
  private

  ! FIXME always create maps separately, don't inline all the ifs.
  ! Raise appropriate errors

  public :: getNamedItem
  public :: setNamedItem
  public :: removeNamedItem

  public :: item
  public :: getLength
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

  function item_nnm(map, index) result(np)
    integer, intent(in)             :: index
    type(NamedNodeMap), pointer    :: map
    type(Node), pointer :: np
    
    type(NamedNode), pointer :: nnp
    integer :: n

    np => null()
    if (.not. associated(map)) return

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
    type(namedNodeMap), pointer :: map
    integer :: n

    if (associated(map)) then
      n = map%length
    else
      n = 0
    endif
    
  end function getLength_nnm

  subroutine append_nnm(map, arg)
    type(namednodeMap), pointer :: map
    type(node), pointer :: arg

    if (.not.associated(map)) then
       allocate(map)
       map%length = 1
       allocate(map%head)
       map%head%this => arg
       map%tail => map%head
    else
       allocate(map%tail%next)
       map%tail%next%this => arg
       map%tail => map%tail%next
       map%length = map%length + 1
    endif

  end subroutine append_nnm

  function getNamedItem(map, name) result(np)
    type(NamedNodeMap), pointer :: map
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(NamedNode), pointer :: nnp

    np => null()
    if (.not. associated(map)) return 

    nnp => map%head
    do while (associated(nnp))
      if (str_vs(nnp%this%nodeName)==name) then
        np => nnp%this
        exit
      endif
      nnp => nnp%next
    enddo

  end function getNamedItem

  function setNamedItem(map, arg) result(np)
    type(NamedNodeMap), pointer :: map
    type(Node), pointer :: arg
    type(Node), pointer :: np

    type(NamedNode), pointer :: nnp

    np => null()
    if (.not.associated(map)) then
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
    type(NamedNodeMap), pointer :: map
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(NamedNode), pointer :: nnp, previous

    ! FIXME if this is attribute node removed, replace with default one

    np => null()
    if (.not. associated(map)) return  

    previous => null()
    nnp => Map%head
    do while (associated(nnp)) 
      if (str_vs(nnp%this%nodeName)==name) then
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

  !FIXME:
!!$    // Introduced in DOM Level 2:
!!$  Node               getNamedItemNS(in DOMString namespaceURI, 
!!$                                    in DOMString localName);
!!$  // Introduced in DOM Level 2:
!!$  Node               setNamedItemNS(in Node arg)
!!$                                        raises(DOMException);
!!$  // Introduced in DOM Level 2:
!!$  Node               removeNamedItemNS(in DOMString namespaceURI, 
!!$                                       in DOMString localName)
!!$                                        raises(DOMException);

end module m_dom_namednodemap


module m_dom_nodelist

use m_dom_types, only: fnode, fnodeList, flistNode

implicit none

private

public :: item
public :: getLength
public :: append

interface append
   module procedure append_nl
end interface

interface item
   module procedure item_nl
end interface

interface getLength
   module procedure getLength_nl
end interface

CONTAINS

  !-----------------------------------------------------------
  ! METHODS FOR NODELISTS
  !-----------------------------------------------------------
  function item_nl(nodeList, i)
    
    integer, intent(in)             :: i
    type(fnodeList), pointer        :: nodeList
    type(fnode), pointer            :: item_nl
    
    type(flistNode), pointer :: lp
    integer :: n

    item_nl => null()            ! In case there is no such item
    if (.not. associated(nodeList)) RETURN

    lp => nodeList%head
    n = -1
    do 
       if (.not. associated(lp))  exit
       n = n + 1
       if (n == i) then
          item_nl => lp%node
          exit
       endif
       lp => lp%next
    enddo

  end function item_nl

  !----------------------------------------------------------- 

  function getLength_nl(nodeList)
    
    type(fnodeList), pointer :: nodeList
    integer                  :: getLength_nl

    if (.not. associated(nodeList)) then
       getLength_nl = 0
    else
       getLength_nl = nodeList % length
    endif

  end function getLength_nl

  subroutine append_nl(nodeList,node)
    type(fnodeList), pointer :: nodeList
    type(fnode), pointer :: node

    if (.not. associated(nodeList)) then
       allocate(nodeList)
       nodelist%length = 1
       allocate(nodelist%head)
       nodelist%head%node => node
       nodelist%tail => nodelist%head
    else
       allocate(nodelist%tail%next)
       nodelist%tail%next%node => node
       nodelist%tail => nodelist%tail%next
       nodelist%length = nodelist%length + 1
    endif

  end subroutine append_nl

end module m_dom_nodelist


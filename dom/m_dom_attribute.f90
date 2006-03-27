module m_dom_attribute

use m_dom_types, only : fnode, ATTRIBUTE_NODE
use m_dom_node, only : setNodeValue
use m_strings, only: string, assignment(=)!, len_trim

implicit none

private
  !-------------------------------------------------------  
  ! METHODS FOR ATTRIBUTE NODES
  !------------------------------------------------------- 

  public :: getName
  public :: getValue
  public :: setValue

CONTAINS

  function getName(attribute)

    type(fnode), intent(in) :: attribute
    type(string)            :: getName

    if (attribute % nodeType == ATTRIBUTE_NODE) then
       getName = attribute%nodeName
    else
       getName = ''
    endif

  end function getName

  !-----------------------------------------------------------

  function getValue(attribute)

    type(fnode), intent(in) :: attribute
    type(string)            :: getValue

    if (attribute % nodeType == ATTRIBUTE_NODE) then
       getValue = attribute%nodeValue
    else
       getValue = ''
    endif

  end function getValue

  !-----------------------------------------------------------

  subroutine setValue(attribute, value)

    character(len=*), intent(in) :: value
    type(fnode), pointer  :: attribute

    if (attribute % nodeType == ATTRIBUTE_NODE) then
       call setNodeValue(attribute,value)
    endif

  end subroutine setValue

  !-----------------------------------------------------------


!!! NB Is this a good idea?
!!! NB pure functions have no side effects

!  pure function attr_name_len(attribute)
!    type(fnode), intent(in) :: attribute
!    integer :: attr_name_len
!    if (attribute % nodeType == ATTRIBUTE_NODE) then
!       attr_name_len = len_trim(attribute % nodeName)
!    else
!       attr_name_len = 0
!    end if
!  end function attr_name_len
!  
!  pure function attr_val_len(attribute)   
!    type(fnode), intent(in) :: attribute
!    integer :: attr_val_len
!    if (attribute % nodeType == ATTRIBUTE_NODE) then
!       attr_val_len = len_trim(attribute % nodeValue)
!    else
!       attr_val_len = 0
!    end if
!  end function attr_val_len


end module m_dom_attribute

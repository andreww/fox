module m_dom_attribute

  use m_common_array_str, only: str_vs
  use m_common_error, only: FoX_error
  use m_dom_types, only : fnode, ATTRIBUTE_NODE
  use m_dom_node, only : setNodeValue
  
  implicit none
  private

  public :: getName
  public :: getValue
!  public :: setValue
!  public :: setnsURI
!  public :: setlocalName
  
CONTAINS
  
  function getName(attribute)
    type(fnode), intent(in) :: attribute
    character(size(attribute%nodeName))  :: getName
    
    if (attribute % nodeType == ATTRIBUTE_NODE) then
      getName = str_vs(attribute%nodeName)
    else
      getName = ''
    endif
    
  end function getName


  function getValue(attribute)
    type(fnode), intent(in) :: attribute
    character(size(attribute%nodeValue))  :: getValue

    if (attribute % nodeType == ATTRIBUTE_NODE) then
       getValue = str_vs(attribute%nodeValue)
    else
       getValue = ''
    endif

  end function getValue


!!$  subroutine setValue(attribute, value)
!!$
!!$    character(len=*), intent(in) :: value
!!$    type(fnode), pointer  :: attribute
!!$
!!$    if (attribute % nodeType == ATTRIBUTE_NODE) then
!!$       call setNodeValue(attribute,value)
!!$    endif
!!$
!!$  end subroutine setValue
!!$
!!$  subroutine setnsURI(attribute, nsURI)
!!$
!!$    character(len=*), intent(in) :: nsuRI
!!$    type(fnode), pointer  :: attribute
!!$
!!$    if (attribute % nodeType == ATTRIBUTE_NODE) then
!!$      deallocate(attribute%nsURI)
!!$      attribute%nsURI => vs_str_alloc(nsURI)
!!$    endif
!!$
!!$  end subroutine setnsURI

!!$  subroutine setlocalName(attribute, localName)
!!$
!!$    character(len=*), intent(in) :: localName
!!$    type(fnode), pointer  :: attribute
!!$
!!$    if (attribute % nodeType == ATTRIBUTE_NODE) then
!!$       call setNodelocalName(attribute,localName)
!!$    endif
!!$
!!$  end subroutine setlocalName
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

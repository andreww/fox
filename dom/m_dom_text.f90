module m_dom_text

  use m_dom_types, only: fnode, TEXT_NODE
  !use m_dom_namednodemap
  !use m_dom_nodelist
  !use m_dom_attribute
  !use m_dom_document
  !use m_dom_debug
  !use m_dom_node
  use m_strings, only: string, len, assignment(=)
  implicit none

  private
  
  public :: getLength
  public :: getData
!  public :: substringData
!  public :: appendData
!  public :: insertData
!  public :: deleteData
!  public :: replaceData
!  public :: splitText

  interface getLength
     module procedure getLengthText
  end interface

CONTAINS


  !-------------------------------------------------------   
  ! METHODS FOR TEXT NODES
  !-------------------------------------------------------   
  function getLengthText(node)
    type(fnode), intent(in) :: node
    integer :: getLengthText
    if (node % nodeType == TEXT_NODE) then
       getLengthText = len(node % nodeValue)
    else
       getLengthText = 0
    endif
  end function getLengthText

  !-------------------------------------------------------   
  function getData(node)
    type(fnode), intent(in) :: node
    type(string) :: getData
    if (node % nodeType == TEXT_NODE) then
       getData = node % nodeValue
    else
       getData = ''
    endif
  end function getData


  !-------------------------------------------------------
!  function subStringData(node, offset, count) 
!    type(fnode), intent(in) :: node
!    integer,     intent(in) :: offset
!    integer,     intent(in) :: count
!    type(string) :: subStringData
!    
!    if (node % nodeType == TEXT_NODE) then
!       subStringData = extract(node%nodeValue, offset, count+offset)
!    else
!       subStringData = ''
!    endif
!  end function subStringData


  !-------------------------------------------------------
!  subroutine appendData(node, data)
!    type(fnode),      intent(in) :: node
!    character(len=*), intent(in) :: data
!    
!    character :: tmp
!
!    if (node % nodeType == TEXT_NODE) then
!       tmp = insert(node%nodeValue, len(node%nodeValue), data)
!    endif 
!  end subroutine appendData
  

  !-------------------------------------------------------
!  subroutine insertData(node, offset, data)
!    type(fnode),      intent(in) :: node
!    integer,          intent(in) :: offset
!    character(len=*), intent(in) :: data
!
!    character :: tmp
!
!    if (node % nodeType == TEXT_NODE) then
!       tmp = insert(node % nodeValue, offset, data)
!    endif
!
!  end subroutine insertData


  !-------------------------------------------------------
!  subroutine deleteData(node, offset, count)
!    type(fnode), intent(in) :: node
!    integer,     intent(in) :: offset
!    integer,     intent(in) :: count
!
!    character :: tmp
!    
!    if (node % nodeType == TEXT_NODE) then
!       tmp = remove(node%nodeValue, offset, offset+count)
!    endif
!  end subroutine deleteData


  !-------------------------------------------------------
!  subroutine replaceData(node, offset, count, data)
!    type(fnode), intent(in) :: node
!    integer,     intent(in) :: offset
!    integer,     intent(in) :: count
!    character(len=*), intent(in) :: data
!    
!    integer :: finish
!    character :: tmp
!    
!    finish = count+offset
!    
!    if (node % nodeType == TEXT_NODE) then
!       if (finish .gt. len(node%nodeValue)) then
!          tmp = remove(node%nodeValue, offset, finish)
!          tmp = insert(node%nodeValue, finish, data)
!       else
!          tmp = replace(node%nodeValue, offset, finish, data)
!       endif
!    endif
!    
!  end subroutine replaceData
  
  !-------------------------------------------------------

!  subroutine splitText(node, offset)
!    type(fnode), pointer :: node
!    integer,     intent(in) :: offset
!
!    type(fnode), pointer :: parent
!    type(fnode), pointer :: newNode
!
!    character(len=(len(node%nodeValue)-offset)) :: tmp
!
!    if (node % nodeType == TEXT_NODE) then
!       tmp = remove(node%nodeValue, offset, len(node%nodeValue)) 
!       newNode => createTextNode(node%ownerDocument, tmp)
!       parent  => node%parentNode
!       newNode => insertBefore(parent, newNode, node)
!    end if
!    
!  end subroutine splitText
                                     
end module m_dom_text

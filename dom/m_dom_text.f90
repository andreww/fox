module m_dom_text

  use m_common_array_str, only: str_vs

  use m_dom_types, only: Node, TEXT_NODE
  !use m_dom_namednodemap
  !use m_dom_nodelist
  !use m_dom_attribute
  !use m_dom_document
  !use m_dom_debug
  !use m_dom_node

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

contains

  function getLengthText(arg)
    type(Node), intent(in) :: arg
    integer :: getLengthText
    if (arg%nodeType == TEXT_NODE) then
       getLengthText = size(arg%nodeValue)
    else
       getLengthText = 0 ! or error
    endif
  end function getLengthText

  function getData(arg)
    type(Node), intent(in) :: arg
    character(len=size(arg%nodeValue)) :: getData
    if (arg%nodeType == TEXT_NODE) then
       getData = str_vs(arg%nodeValue)
    else
       getData = '' ! or error
    endif
  end function getData


  !-------------------------------------------------------
!  function subStringData(node, offset, count) 
!    type(Node), intent(in) :: node
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
!    type(Node),      intent(in) :: node
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
!    type(Node),      intent(in) :: node
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
!    type(Node), intent(in) :: node
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
!    type(Node), intent(in) :: node
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
!    type(Node), pointer :: node
!    integer,     intent(in) :: offset
!
!    type(Node), pointer :: parent
!    type(Node), pointer :: newNode
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

TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_dom_error, only: INDEX_SIZE_ERR

')`'dnl
dnl
TOHW_m_dom_publics(`

  public :: getLength
!  public :: getData
!  public :: setData
  public :: substringData
  public :: appendData
  public :: insertData
  public :: deleteData
  public :: replaceData

  interface getLength
    module procedure getLength_characterdata
  end interface

')`'dnl
dnl
TOHW_m_dom_contents(`

  pure function isCharData(nodeType) result(p)
    integer, intent(in) :: nodeType
    logical :: p

    p = (nodeType == TEXT_NODE .or. &
      nodeType == COMMENT_NODE .or. &
      nodeType == CDATA_SECTION_NODE)
  end function isCharData


  function getLength_characterdata(arg) result(n)
    type(Node), intent(in) :: arg
    integer :: n
    if (isCharData(arg%nodeType)) then
       n = size(arg%nodeValue)
    else
       n = 0 ! FIXME error
    endif
  end function getLength_characterdata


  TOHW_function(subStringData, (arg, offset, count), c)
    type(Node), intent(in) :: arg
    integer, intent(in) :: offset
    integer, intent(in) :: count
    character(len=count) :: c

    ! FIXME error if offset/count are out of range
    
    if (offset<0 .or. count<0) then
      TOHW_m_dom_throw_error(INDEX_SIZE_ERR)
    endif

    if (isCharData(arg%nodeType)) then
      c = str_vs(arg%nodeValue(offset:offset+count-1))
    else
      continue
      ! FIXME error
    endif
  end function subStringData


  TOHW_subroutine(appendData, (arg, data))
    type(Node), intent(inout) :: arg
    character(len=*), intent(in) :: data
    
    character, pointer :: tmp(:)

    if (isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif
  !FIXME what if data is wrong for node type    

    tmp => arg%nodeValue
    arg%nodeValue => vs_str_alloc(str_vs(tmp)//data)
    deallocate(tmp)

  end subroutine appendData
  

  TOHW_subroutine(insertData, (arg, offset, data))
    type(Node), intent(inout) :: arg
    integer, intent(in) :: offset
    character(len=*), intent(in) :: data

    character, pointer :: tmp(:)

    if (isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (offset<0) then
      TOHW_m_dom_throw_error(INDEX_SIZE_ERR)
    endif
  !FIXME what if data is wrong for node type    

    tmp => arg%nodeValue
    arg%nodeValue => vs_str_alloc(str_vs(tmp(:offset))//data//str_vs(tmp(offset+1:)))
    deallocate(tmp)

  end subroutine insertData


  TOHW_subroutine(deleteData, (arg, offset, count))
    type(Node), intent(inout) :: arg
    integer, intent(in) :: offset
    integer, intent(in) :: count

    character, pointer :: tmp(:)

    if (isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (offset<0 .or. count<0) then
      TOHW_m_dom_throw_error(INDEX_SIZE_ERR)
    endif
   ! FIXME offset/count check
    
    tmp => arg%nodeValue
    arg%nodeValue => vs_str_alloc(str_vs(tmp(:offset))//str_vs(tmp(offset+count:)))
    deallocate(tmp)

  end subroutine deleteData


  TOHW_subroutine(replaceData, (arg, offset, count, data))
    type(Node), intent(inout) :: arg
    integer, intent(in) :: offset
    integer, intent(in) :: count
    character(len=*), intent(in) :: data
    
    character, pointer :: tmp(:)

    ! FIXME offset >0 check
    
    if (isCharData(arg%nodeType)) then
      tmp => arg%nodeValue
      if (offset+count <= size(arg%nodeValue)) then
        arg%nodeValue => vs_str_alloc(str_vs(tmp(:offset))//data//str_vs(tmp(offset+count:)))
      else
        arg%nodeValue => vs_str_alloc(str_vs(tmp(:offset))//data)
      endif
      deallocate(tmp)
    else
      continue
      ! FIXME error
    endif
    
  end subroutine replaceData
 
')`'dnl

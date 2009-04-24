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


  TOHW_function(getLength_characterdata, (arg), n)
    type(Node), pointer :: arg
    integer :: n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    
    if (.not.isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    n = len(arg%nodeValue)
    
  end function getLength_characterdata


  TOHW_function(subStringData, (arg, offset, count), c)
    type(Node), pointer :: arg
    integer, intent(in) :: offset
    integer, intent(in) :: count
    character(len=count) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (.not.isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (offset<0.or.offset>len(arg%nodeValue).or.count<0) then
      TOHW_m_dom_throw_error(INDEX_SIZE_ERR)
    endif

    if (offset+count>len(arg%nodeValue)) then
      c = as_chars(arg%nodeValue,offset+1,len(arg%nodeValue))
    else
      c = as_chars(arg%nodeValue,offset+1,offset+count)
    endif

  end function subStringData


  TOHW_subroutine(appendData, (arg, data))
    type(Node), pointer :: arg
    character(len=*), intent(in) :: data
    
    character, pointer :: tmp(:)

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (.not.isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not.checkChars(data, getXmlVersionEnum(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif
    
    call add_chars(arg%nodeValue, data)

    ! We have to do these checks *after* appending data in case offending string
    ! spans old & new data
    if (arg%nodeType==COMMENT_NODE .and. index(as_chars(arg%nodeValue),"--")>0) then
      TOHW_m_dom_throw_error(FoX_INVALID_COMMENT)
    elseif (arg%nodeType==CDATA_SECTION_NODE .and. index(as_chars(arg%nodeValue), "]]>")>0) then
      TOHW_m_dom_throw_error(FoX_INVALID_CDATA_SECTION)
    endif

    ! And propagate length upwards ...
    if (getNodeType(arg)/=COMMENT_NODE) &
      call updateTextContentLength(arg, len(data))

  end subroutine appendData
  

  TOHW_subroutine(insertData, (arg, offset, data))
    type(Node), pointer :: arg
    integer, intent(in) :: offset
    character(len=*), intent(in) :: data

    type(vs), pointer :: tmp

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (.not.isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (offset<0.or.offset>len(arg%nodeValue)) then
      TOHW_m_dom_throw_error(INDEX_SIZE_ERR)
    endif

    if (.not.checkChars(data, getXmlVersionEnum(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif

    tmp => arg%nodeValue
    arg%nodeValue => new_vs(init_chars=as_chars(tmp,1,offset)//data//as_chars(tmp,offset+1,len(arg%nodeValue)))
    call destroy_vs(tmp)

    ! We have to do these checks *after* appending data in case offending string
    ! spans old & new data
    if (arg%nodeType==COMMENT_NODE .and. index(as_chars(arg%nodeValue),"--")>0) then
      TOHW_m_dom_throw_error(FoX_INVALID_COMMENT)
    elseif (arg%nodeType==CDATA_SECTION_NODE .and. index(as_chars(arg%nodeValue), "]]>")>0) then
      TOHW_m_dom_throw_error(FoX_INVALID_CDATA_SECTION)
    endif

    ! And propagate length upwards ...
    if (getNodeType(arg)/=COMMENT_NODE) &
      call updateTextContentLength(arg, len(data))

  end subroutine insertData


  TOHW_subroutine(deleteData, (arg, offset, count))
    type(Node), pointer :: arg
    integer, intent(in) :: offset
    integer, intent(in) :: count

    type(vs), pointer :: tmp
    integer :: n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (.not.isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (offset<0.or.offset>len(arg%nodeValue).or.count<0) then
      TOHW_m_dom_throw_error(INDEX_SIZE_ERR)
    endif

    if (offset+count>len(arg%nodeValue)) then
      n = len(arg%nodeValue)-offset
    else
      n = count
    endif
    
    tmp => arg%nodeValue
    arg%nodeValue => new_vs(init_chars=as_chars(tmp,1,offset)//as_chars(tmp,offset+count+1,len(arg%nodeValue)))
    call destroy_vs(tmp)

    ! And propagate length upwards ...
    if (getNodeType(arg)/=COMMENT_NODE) &
      call updateTextContentLength(arg, -n)

  end subroutine deleteData


  TOHW_subroutine(replaceData, (arg, offset, count, data))
    type(Node), pointer :: arg
    integer, intent(in) :: offset
    integer, intent(in) :: count
    character(len=*), intent(in) :: data
    
    type(vs), pointer :: tmp
    integer :: n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (.not.isCharData(arg%nodeType)) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (offset<0.or.offset>len(arg%nodeValue).or.count<0) then
      TOHW_m_dom_throw_error(INDEX_SIZE_ERR)
    endif

    if (.not.checkChars(data, getXmlVersionEnum(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif

    if (offset+count>len(arg%nodeValue)) then
      n = len(data)-(len(arg%nodeValue)-offset)
    else
      n = len(data)-count
    endif

    if (offset+count <= len(arg%nodeValue)) then
      tmp => arg%nodeValue
      arg%nodeValue => new_vs(init_chars=as_chars(tmp,1,offset)//data//as_chars(tmp,offset+count+1,len(tmp)))
      call destroy_vs(tmp)
    else
      call add_chars(arg%nodeValue, data)
    endif

    ! We have to do these checks *after* appending data in case offending string
    ! spans old & new data
    if (arg%nodeType==COMMENT_NODE .and. index(as_chars(arg%nodeValue),"--")>0) then
      TOHW_m_dom_throw_error(FoX_INVALID_COMMENT)
    elseif (arg%nodeType==CDATA_SECTION_NODE .and. index(as_chars(arg%nodeValue), "]]>")>0) then
      TOHW_m_dom_throw_error(FoX_INVALID_CDATA_SECTION)
    endif

    ! And propagate length upwards ...
    if (getNodeType(arg)/=COMMENT_NODE) &
      call updateTextContentLength(arg, n)

  end subroutine replaceData
 
')`'dnl

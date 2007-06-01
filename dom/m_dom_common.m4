TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc

')`'dnl
dnl
TOHW_m_dom_publics(`

! Assorted functions with identical signatures despite belonging to different types.

  public :: getData
  public :: setData
  public :: getName
  public :: getPublicId
  public :: getSystemId

')`'dnl
dnl
TOHW_m_dom_contents(`

  function getData(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%nodeValue)) :: c
    if (arg%nodeType/=TEXT_NODE .and. &
      arg%nodeType/=COMMENT_NODE .and. &
      arg%nodeType/=CDATA_SECTION_NODE .and. &
      arg%nodeType/=PROCESSING_INSTRUCTION_NODE) then
       c = str_vs(arg%nodeValue)
    else
       c = "" ! or error
    endif
  end function getData


  subroutine setData(arg, data)
    type(Node), intent(inout) :: arg
    character(len=*) :: data
    if (arg%nodeType/=TEXT_NODE .and. &
      arg%nodeType/=COMMENT_NODE .and. &
      arg%nodeType/=CDATA_SECTION_NODE .and. &
      arg%nodeType/=PROCESSING_INSTRUCTION_NODE) then
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(data)
    else
      ! or error
      continue
    endif
  end subroutine setData

  
  function getName(arg) result(c)
    type(Node), intent(in) :: arg
    character(size(arg%nodeName)) :: c
    
    if (arg%nodeType/=ATTRIBUTE_NODE .and. &
      arg%nodeType/=DOCUMENT_TYPE_NODE) then
      c = "" ! FIXME error
    endif
    c = str_vs(arg%nodeName)
    
  end function getName


  function getPublicId(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%publicId)) :: c

    if (arg%nodeType/=DOCUMENT_TYPE_NODE .and. &
      arg%nodeType/=NOTATION_NODE .and. &
      arg%nodeType/=ENTITY_NODE) then
      ! FIXME error
      continue
    endif
    c = str_vs(arg%publicId)

  end function getPublicId


  function getSystemId(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%systemId)) :: c

    if (arg%nodeType/=DOCUMENT_TYPE_NODE .and. &
      arg%nodeType/=NOTATION_NODE .and. &
      arg%nodeType/=ENTITY_NODE) then
      ! FIXME error
      continue
    endif
    c = str_vs(arg%systemId)

  end function getSystemId

')`'dnl

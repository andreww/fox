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

  pure function getData_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    if (p) then 
      n = size(arg%nodeValue)
    else
      n = 0
    endif
  end function getData_len
  
  TOHW_function(getData, (arg), c)
    type(Node), pointer :: arg
    character(len=getData_len(arg, associated(arg))) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType==TEXT_NODE .or. &
      arg%nodeType==COMMENT_NODE .or. &
      arg%nodeType==CDATA_SECTION_NODE .or. &
      arg%nodeType==PROCESSING_INSTRUCTION_NODE) then
       c = str_vs(arg%nodeValue)
    else
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
  end function getData


  TOHW_subroutine(setData, (arg, data))
    type(Node), pointer :: arg
    character(len=*) :: data

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType==TEXT_NODE .or. &
      arg%nodeType==COMMENT_NODE .or. &
      arg%nodeType==CDATA_SECTION_NODE .or. &
      arg%nodeType==PROCESSING_INSTRUCTION_NODE) then
      if (arg%readonly) then
        TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
      endif
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(data)
    else
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
  end subroutine setData

  pure function getName_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    if (p) then 
      n = size(arg%nodeName)
    else
      n = 0
    endif
  end function getName_len
  
  TOHW_function(getName, (arg), c)
    type(Node), pointer :: arg
    character(getName_len(arg, associated(arg))) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    
    if (arg%nodeType/=ATTRIBUTE_NODE .and. &
      arg%nodeType/=DOCUMENT_TYPE_NODE) then
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    c = str_vs(arg%nodeName)
    
  end function getName

  pure function getPublicId_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    if (p) then 
      n = size(arg%publicId)
    else
      n = 0
    endif
  end function getPublicId_len

  TOHW_function(getPublicId, (arg), c)
    type(Node), pointer :: arg
    character(len=getPublicId_len(arg, associated(arg))) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_TYPE_NODE .and. &
      arg%nodeType/=NOTATION_NODE .and. &
      arg%nodeType/=ENTITY_NODE) then
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    c = str_vs(arg%publicId)

  end function getPublicId

  pure function getSystemId_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    if (p) then 
      n = size(arg%systemId)
    else
      n = 0
    endif
  end function getSystemId_len

  TOHW_function(getSystemId, (arg), c)
    type(Node), pointer :: arg
    character(len=getSystemId_len(arg, associated(arg))) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_TYPE_NODE .and. &
      arg%nodeType/=NOTATION_NODE .and. &
      arg%nodeType/=ENTITY_NODE) then
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    c = str_vs(arg%systemId)

  end function getSystemId

')`'dnl

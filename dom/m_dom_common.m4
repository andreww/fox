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

  TOHW_m_dom_get(DOMString, data, np%nodeValue, (TEXT_NODE, COMMENT_NODE, CDATA_SECTION_NODE, PROCESSING_INSTRUCTION_NODE))

  TOHW_subroutine(setData, (arg, data))
    type(Node), pointer :: arg
    character(len=*) :: data
    
    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    
!NB special case in order to check readonly correctly
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
  
  TOHW_m_dom_get(DOMString, name, np%nodeName, (DOCUMENT_TYPE_NODE, ATTRIBUTE_NODE))

  TOHW_m_dom_get(DOMString, publicId, np%dtdExtras%publicId, (DOCUMENT_TYPE_NODE, NOTATION_NODE, ENTITY_NODE))

  TOHW_m_dom_get(DOMString, systemId, np%dtdExtras%systemId, (DOCUMENT_TYPE_NODE, NOTATION_NODE, ENTITY_NODE))

')`'dnl

TOHW_m_dom_publics(`

  !public :: getName
  public :: getEntities
  public :: getNotations
!  public :: getPublicId
!  public :: getSystemId
  public :: getInternalSubset

')`'dnl
dnl
TOHW_m_dom_contents(`

!  function getName(docType) result(c) See m_dom_common

  TOHW_function(getEntities, (arg), nnp)
    type(Node), pointer :: arg
    type(NamedNodeMap), pointer :: nnp

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_TYPE_NODE) then
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    nnp => arg%dtdExtras%entities
  end function getEntities

  TOHW_function(getNotations, (arg), nnp)
    type(Node), pointer :: arg
    type(NamedNodeMap), pointer :: nnp

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_TYPE_NODE) then
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    nnp => arg%dtdExtras%notations
  end function getNotations


!  function getPublicId(docType) result(c) See m_dom_common


!  function getSystemId(docType) result(c) See m_dom_common

  TOHW_m_dom_get(DOMString, internalSubset, np%dtdExtras%internalSubset, (DOCUMENT_TYPE_NODE))

')`'dnl

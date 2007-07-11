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

    nnp => arg%ownerDocument%docExtras%entities
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

    nnp => arg%ownerDocument%docExtras%notations
  end function getNotations


!  function getPublicId(docType) result(c) See m_dom_common


!  function getSystemId(docType) result(c) See m_dom_common

    pure function getInternalSubset_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    if (p) then 
      n = size(arg%internalSubset)
    else
      n = 0
    endif
  end function getInternalSubset_len

  TOHW_function(getInternalSubset, (arg), c)
    type(Node), pointer :: arg
    character(len=getInternalSubset_len(arg, associated(arg))) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_TYPE_NODE) then
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = str_vs(arg%internalSubset)
  end function getInternalSubset


')`'dnl

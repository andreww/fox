TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs

')`'dnl
dnl
TOHW_m_dom_publics(`

  !public :: getName
  public :: getEntities
  public :: getNotations
!  public :: getPublicId
!  public :: getSystemId
  public :: getInternalSubset

!  Not part of documented API:
  public :: setDocType

')`'dnl
dnl
TOHW_m_dom_contents(`

!  function getName(docType) result(c) See m_dom_common

  function getEntities(docType) result(nnp)
    type(Node), pointer :: docType
    type(NamedNodeMap), pointer :: nnp

    if (docType%nodeType/=DOCUMENT_TYPE_NODE) then
      ! FIXME error
      continue
    endif

    nnp => docType%entities
  end function getEntities

  function getNotations(docType) result(nnp)
    type(Node), pointer :: docType
    type(NamedNodeMap), pointer :: nnp

    if (docType%nodeType/=DOCUMENT_TYPE_NODE) then
      ! FIXME error
      continue
    endif

    nnp => docType%notations
  end function getNotations


!  function getPublicId(docType) result(c) See m_dom_common


!  function getSystemId(docType) result(c) See m_dom_common


  function getInternalSubset(docType) result(c)
    type(Node), intent(in) :: docType
    character(len=size(docType%internalSubset)) :: c

    if (docType%nodeType/=DOCUMENT_TYPE_NODE) then
      ! FIXME error
      continue
    endif

    c = str_vs(docType%internalSubset)
  end function getInternalSubset


  subroutine setDocType(docType, name, publicId, systemId)
    type(Node), intent(inout) :: docType
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in), optional :: systemId

    if (docType%nodeType/=DOCUMENT_TYPE_NODE) then
      ! FIXME throw an internal error
      continue
    endif
    
    deallocate(docType%nodeName)
    docType%nodeName => vs_str_alloc(name)
    if (present(publicId)) then
      deallocate(docType%publicId)
      docType%publicId => vs_str_alloc(publicId)
    endif
    if (present(systemId)) then
      deallocate(docType%systemId)
      docType%systemId => vs_str_alloc(systemId)
    endif

  end subroutine setDocType

')`'dnl

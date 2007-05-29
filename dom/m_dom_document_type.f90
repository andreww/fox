module m_dom_document_type

  use m_common_array_str, only: str_vs

  use m_dom_types, only: Node, NamedNodeMap, DOCUMENT_TYPE_NODE

  implicit none
  private

  !public :: getName
  public :: getEntities
  public :: getNotations
!  public :: getPublicId
!  public :: getSystemId
  public :: getInternalSubset

contains

!  function getName(docType) result(c) See m_dom_common

  function getEntities(docType) result(nnp)
    type(Node), intent(in) :: docType
    type(NamedNodeMap) :: nnp

    if (docType%nodeType/=DOCUMENT_TYPE_NODE) then
      ! FIXME error
      continue
    endif

    nnp = docType%entities
  end function getEntities

  function getNotations(docType) result(nnp)
    type(Node), intent(in) :: docType
    type(NamedNodeMap) :: nnp

    if (docType%nodeType/=DOCUMENT_TYPE_NODE) then
      ! FIXME error
      continue
    endif

    nnp = docType%notations
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

end module m_dom_document_type

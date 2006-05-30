module m_dom_implementation

  use m_dom_document, only : fDocument
  use m_dom_documenttype, only : fDocumentType
  use m_dom_node, only : dom_node

  implicit none
  private

  public :: hasFeature

contains

  function hasFeature(feature, version) result(p)
    character(len=*), intent(in) :: feature
    character(len=*), intent(in) :: version
    logical :: p

    !FIXME
    p = .true.
  end function hasFeature

  function createDocumentType(qualifiedName, publicId, systemId) result(dt)
    character(len=*), intent(in) :: qualifiedName
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    type(fdocumentType), pointer :: dt
    allocate(dt%name(len(qualifiedName)))
    dt%name = transfer(qualifiedName, dt%name)
    dt%entities => createNamedNodeMap()
    dt%notations => createNamedNodeMap()
    allocate(dt%publicId(len(publicId)))
    dtpublicId = transfer(publicId, dt%publicId)
    allocate(dt%publicId(len(systemId)))
    dt%systemId = transfer(systemId, dt%systemId)
    allocate(dt%internalSubset(0) !FIXME
  end function createDocumentType

  subroutine destroyDocumentType(dt)
    type(fDocumentType), pointer :: dt
    deallocate(dt%name)
    call destroyNamedNodeMap(dt%entities)
    call destroyNamedNodeMap(dt%notations)
    deallocate(dt%publicId)
    deallocate(dt%systemId)
    deallocate(dt%internalSubset)
    deallocate(dt)
  end subroutine destroyDocumentType

  ! function createDocument
  !   implementation is found in m_dom_document
  ! end function createDocument

end module m_dom_implementation

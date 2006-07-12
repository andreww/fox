module m_dom_implementation

  use m_dom_types, only : fDocumentNode
  use m_dom_types, only : fDocumentType
  use m_dom_types, only : fnode

  implicit none
  private

  public :: hasFeature
  public :: createDocument

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
    !dt%entities
    !dt%notations
    allocate(dt%publicId(len(publicId)))
    dt%publicId = transfer(publicId, dt%publicId)
    allocate(dt%publicId(len(systemId)))
    dt%systemId = transfer(systemId, dt%systemId)
    allocate(dt%internalSubset(0)) !FIXME
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

  
  function createDocument(namespaceURI, qualifiedName, docType) result(doc)
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: qualifiedName
    type(fDocumentType), pointer :: docType
    type(fDocumentNode), pointer :: doc
    
    !DOM implementation ...
    doc%doctype => docType
    
    !   !FIXME
  end function createDocument

  ! function createDocument
  !   implementation is found in m_dom_document
  ! end function createDocument

end module m_dom_implementation

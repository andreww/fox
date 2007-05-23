module m_dom_implementation

  use m_common_array_str, only: vs_str_alloc
  use m_dom_types, only : fDocumentNode
  use m_dom_types, only : fDocumentType
  use m_dom_types, only : fnode

  implicit none
  private

  public :: hasFeature
  public :: createDocument
  public :: createDocumentType

contains

  function hasFeature(feature, version) result(p)
    character(len=*), intent(in) :: feature
    character(len=*), intent(in) :: version
    logical :: p

    !FIXME
    p = .true.
  end function hasFeature

  function createDocumentType(qualifiedName, publicId, systemId) result(dt)
    character(len=*), intent(in), optional :: qualifiedName
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in), optional :: systemId
    type(fdocumentType), pointer :: dt
    if (present(qualifiedName)) then
      dt%name = vs_str_alloc(qualifiedName)
    else
      allocate(dt%name(0))
    endif
    !dt%entities
    !dt%notations
      allocate(dt%publicId(len(publicId)))
    if (present(publicId)) then
      dt%publicId = vs_str_alloc(publicId)
    else
      allocate(dt%publicId(0))
    endif
    if (present(systemId)) then
      dt%systemId = vs_str_alloc(systemId)
    else
      allocate(dt%publicId(0))
    endif
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

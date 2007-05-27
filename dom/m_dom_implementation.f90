module m_dom_implementation

  use m_common_array_str, only: vs_str_alloc
  use m_dom_document, only: createElementNS
 ! use m_dom_types, only: FoX_DOM
  use m_dom_types, only: Node, DOCUMENT_NODE

  implicit none
  private

  public :: hasFeature
  public :: createDocument
  public :: createDocumentType

  public :: createEmptyDocument
  public :: createEmptyDocumentType

contains

  function hasFeature(feature, version) result(p)
    character(len=*), intent(in) :: feature
    character(len=*), intent(in), optional :: version
    logical :: p
    
    if (present(version)) then
      if (version/="1.0".and.version/="2.0") then
        p = .false.
        return
      endif
    endif
    p = (feature=="Core".or.feature=="XML")
  end function hasFeature

  function createDocumentType(qualifiedName, publicId, systemId) result(dt)
    character(len=*), intent(in) :: qualifiedName
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    type(Node), pointer :: dt

    allocate(dt)
    dt%name = vs_str_alloc(qualifiedName)
    !dt%entities
    !dt%notations
    dt%publicId = vs_str_alloc(publicId)
    dt%systemId = vs_str_alloc(systemId)
    allocate(dt%internalSubset(0)) !FIXME
    dt%ownerDocument => null()
    ! FIXME fill in the rest of the fields ...

  end function createDocumentType

  function createEmptyDocumentType() result(dt)
    type(Node), pointer :: dt

    allocate(dt)
    allocate(dt%name(0))
    !dt%entities
    !dt%notations
    allocate(dt%publicId(0))
    allocate(dt%systemId(0))
    allocate(dt%internalSubset(0)) !FIXME
  end function createEmptyDocumentType

  subroutine destroyDocumentType(dt)
    type(Node), pointer :: dt
    
    deallocate(dt%name)
    !call destroyNamedNodeMap(dt%entities)
    !call destroyNamedNodeMap(dt%notations)
    deallocate(dt%publicId)
    deallocate(dt%systemId)
    deallocate(dt%internalSubset)
    deallocate(dt)
  end subroutine destroyDocumentType

  
  function createDocument(namespaceURI, qualifiedName, docType) result(doc)
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: qualifiedName
    type(Node), pointer, optional :: docType
    type(Node), pointer :: doc

    allocate(doc)
    doc%nodeType = DOCUMENT_NODE
    doc%nodeName = "#document"
    
    doc%docType => null()
    if (present(docType)) then
      doc%doctype => docType
    endif
    if (.not.associated(doc%docType)) then
      doc%docType => createDocumentType(qualifiedName, "", "")
    endif
!    doc%implementation => FoX_DOM
    doc%documentElement => createElementNS(doc, namespaceURI, qualifiedName)
    
  end function createDocument

  function createEmptyDocument() result(doc)
    type(Node), pointer :: doc
    
    allocate(doc)
    doc%nodeType = DOCUMENT_NODE
    ! FIXME do something with namespaceURI etc 
    doc%doctype => createEmptyDocumentType()
!    doc%implementation => FoX_DOM
    doc%documentElement => null()
    
  end function createEmptyDocument

end module m_dom_implementation

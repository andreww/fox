module m_dom_implementation

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_dom_document, only: createElementNS
 ! use m_dom_types, only: FoX_DOM
  use m_dom_types, only: Node, NodeList, createNode, destroyNode, DOCUMENT_NODE, DOCUMENT_TYPE_NODE
  use m_dom_nodeList, only: append, pop_nl, destroyNodeList

  implicit none
  private

  public :: hasFeature
  public :: createDocument
  public :: createDocumentType

  public :: destroyDocument

  public :: createEmptyDocument
  public :: createEmptyDocumentType

contains

  function hasFeature(feature, version) result(p)
    character(len=*), intent(in) :: feature
    character(len=*), intent(in), optional :: version
    logical :: p
    ! FIXME squashcase
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

    dt => createNode(null(), DOCUMENT_TYPE_NODE, qualifiedName, "")
    dt%ownerDocument => dt
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

    dt => createNode(null(), DOCUMENT_TYPE_NODE, "", "")
    !dt%entities
    !dt%notations
    allocate(dt%publicId(0))
    allocate(dt%systemId(0))
    allocate(dt%internalSubset(0)) !FIXME
  end function createEmptyDocumentType

  subroutine destroyDocumentType(dt)
    type(Node), pointer :: dt
    
    if (associated(dt%publicId)) deallocate(dt%publicId)
    if (associated(dt%systemId)) deallocate(dt%systemId)
    if (associated(dt%internalSubset)) deallocate(dt%internalSubset)
    call destroyNode(dt)
    !call destroyNamedNodeMap(dt%entities)
    !call destroyNamedNodeMap(dt%notations)
  end subroutine destroyDocumentType

  
  function createDocument(namespaceURI, qualifiedName, docType) result(doc)
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: qualifiedName
    type(Node), pointer, optional :: docType
    type(Node), pointer :: doc

    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => doc

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
    
    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => doc

    ! FIXME do something with namespaceURI etc 
    doc%doctype => createEmptyDocumentType()
!    doc%implementation => FoX_DOM
    doc%documentElement => null()
    
  end function createEmptyDocument

  subroutine destroyDocument(doc)
    type(Node), pointer :: doc
    
    type(NodeList) :: np_stack
    type(Node), pointer :: np, np_next
    logical :: alreadyDone

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

    np => doc%firstChild

    if (.not.associated(np)) then
      ! FIXME internal eror
      continue
    endif

    ! Use iteration, not recursion, to save stack space.
    call append(np_stack, np)
    alreadyDone = .false.
    do
      print*, 'iterating ...', associated(np), alreadyDONe, np_stack%length
      if (alreadyDone) then
        np => pop_nl(np_stack)
        if (np_stack%length==0) then
          exit
        else
          alreadyDone = .false.
        endif
      else if (associated(np%firstChild)) then
        call append(np_stack, np)
        np => np%firstChild
        cycle
      endif
      np_next => np%nextSibling
      print*, 'destroying Node', np%nodeType, str_vs(np%nodeName)
      call destroyNode(np)
      if (associated(np_next)) then
        np => np_next
        cycle
      else
        alreadyDone = .true.
      endif
    enddo
    call destroyNodeList(np_stack)

    call destroyDocumentType(doc%docType)
    call destroyNode(doc)

  end subroutine destroyDocument

end module m_dom_implementation

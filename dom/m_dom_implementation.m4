TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_charset, only: checkChars, XML1_0

  use m_dom_error, only: DOMException, throw_exception, is_in_error, &
    INVALID_CHARACTER_ERR, NAMESPACE_ERR, FoX_INVALID_PUBLIC_ID, FoX_INVALID_SYSTEM_ID
  use m_common_namecheck, only: checkName, checkPublicId, checkSystemId
  use m_common_string, only: toLower

')`'dnl
dnl
TOHW_m_dom_publics(`

  public :: hasFeature
  public :: createDocument
  public :: createDocumentType

  public :: destroyDocument

  public :: createEmptyDocument
  public :: createEmptyDocumentType

')`'dnl
dnl
TOHW_m_dom_contents(`

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
    p = (toLower(feature)=="core".or.toLower(feature)=="xml")
  end function hasFeature


  TOHW_function(createDocumentType, (qualifiedName, publicId, systemId), dt)
    character(len=*), intent(in) :: qualifiedName
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    type(Node), pointer :: dt

    if (.not.checkChars(qualifiedName, XML1_0)) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif

    if (.not.checkName(qualifiedName)) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    ! FIXME check that prefix etc is declared
    elseif (.not.checkPublicId(publicId)) then
      TOHW_m_dom_throw_error(FoX_INVALID_PUBLIC_ID)
    elseif (.not.checkSystemId(systemId)) then
      TOHW_m_dom_throw_error(FoX_INVALID_SYSTEM_ID)
    endif

    dt => createNode(null(), DOCUMENT_TYPE_NODE, qualifiedName, "")
    dt%readonly = .true.
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
    dt%readonly = .true.
    !dt%entities
    !dt%notations
    allocate(dt%publicId(0))
    allocate(dt%systemId(0))
    allocate(dt%internalSubset(0)) !FIXME
  end function createEmptyDocumentType


  function createDocument(namespaceURI, qualifiedName, docType) result(doc)
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: qualifiedName
    type(Node), pointer, optional :: docType
    type(Node), pointer :: doc

    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => doc

    if (present(docType)) then
      doc%doctype => appendChild(doc, doc%docType)
    endif
    if (.not.associated(doc%docType)) then
      doc%docType => appendChild(doc, createDocumentType(qualifiedName, "", ""))
    endif

    doc%docType%ownerElement => doc
!    doc%implementation => FoX_DOM
    doc%documentElement => appendChild(doc, createElementNS(doc, namespaceURI, qualifiedName))

    doc%xmlVersion = vs_str_alloc("1.0")
    
  end function createDocument


  function createEmptyDocument() result(doc)
    type(Node), pointer :: doc
    
    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => doc

    ! FIXME do something with namespaceURI etc 
    doc%doctype => appendChild(doc, createEmptyDocumentType())
    doc%docType%ownerElement => doc
!    doc%implementation => FoX_DOM
    doc%documentElement => null()

    doc%xmlVersion = vs_str_alloc("1.0")
    
  end function createEmptyDocument


  subroutine destroyDocument(doc)
    type(Node), pointer :: doc
    
    type(NodeList) :: np_stack
    type(Node), pointer :: np, np_next
    logical :: ascending

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

    np => doc%firstChild
    ! if not associated internal error
    ! FIXME DONT NEED STACK
    call append(np_stack, np)
    ascending = .false.
    do
      print*, "iterating ...", associated(np), ascending, np_stack%length
      if (ascending) then
        np => pop_nl(np_stack)
        if (np_stack%length==0) then
          exit
        else
          ascending = .false.
        endif
      else if (associated(np%firstChild)) then
        call append(np_stack, np)
        np => np%firstChild
        cycle
      endif
      np_next => np%nextSibling
      call destroyNode(np)
      if (associated(np_next)) then
        np => np_next
        cycle
      else
        ascending = .true.
      endif
    enddo
    call destroyNodeList(np_stack)

    print*, "destroying a node:", doc%nodeType, doc%nodeName
    call destroyNodeContents(doc)
    deallocate(doc)

  end subroutine destroyDocument

')`'dnl

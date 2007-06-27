TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_charset, only: checkChars, XML1_0

  use m_dom_error, only: DOMException, throw_exception, is_in_error, &
    INVALID_CHARACTER_ERR, NAMESPACE_ERR, FoX_INVALID_PUBLIC_ID, FoX_INVALID_SYSTEM_ID
  use m_common_namecheck, only: checkName, checkPublicId, checkSystemId
  use m_common_string, only: toLower
  use m_common_struct, only: init_xml_doc_state, destroy_xml_doc_state

')`'dnl
dnl
TOHW_m_dom_publics(`

  public :: hasFeature
  public :: createDocument
  public :: createDocumentType

  public :: destroyDocument

  public :: createEmptyDocument
  public :: createEmptyDocumentType

  public :: replace_xds

')`'dnl
dnl
TOHW_m_dom_contents(`

  function hasFeature(impl, feature, version) result(p)
    type(DOMImplementation), intent(in) :: impl
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


  TOHW_function(createDocumentType, (impl, qualifiedName, publicId, systemId), dt)
    type(DOMImplementation), intent(in) :: impl
    character(len=*), intent(in) :: qualifiedName
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    type(Node), pointer :: dt

    allocate(dt%xds)
    call init_xml_doc_state(dt%xds)

    if (.not.checkChars(qualifiedName, XML1_0)) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR, (dt%xds))
    endif

    if (.not.checkName(qualifiedName, dt%xds)) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR, (dt%xds))
    ! FIXME check that prefix etc is declared
    elseif (.not.checkPublicId(publicId)) then
      TOHW_m_dom_throw_error(FoX_INVALID_PUBLIC_ID, (dt%xds))
    elseif (.not.checkSystemId(systemId)) then
      TOHW_m_dom_throw_error(FoX_INVALID_SYSTEM_ID, (dt%xds))
    endif

    dt => createNode(null(), DOCUMENT_TYPE_NODE, qualifiedName, "")
    dt%readonly = .true.
    dt%publicId = vs_str_alloc(publicId)
    dt%systemId = vs_str_alloc(systemId)
    allocate(dt%internalSubset(0)) !FIXME
    dt%ownerDocument => null()
    ! FIXME fill in the rest of the fields ...

  end function createDocumentType


  function createEmptyDocumentType(doc) result(dt)
    type(Node), pointer :: doc
    type(Node), pointer :: dt

    dt => createNode(doc, DOCUMENT_TYPE_NODE, "", "")
    dt%readonly = .true.
    !dt%entities
    !dt%notations
    allocate(dt%publicId(0))
    allocate(dt%systemId(0))
    allocate(dt%internalSubset(0)) !FIXME

    allocate(dt%xds)
    call init_xml_doc_state(dt%xds)
  end function createEmptyDocumentType


  subroutine replace_xds(dt, xds)
    type(Node), pointer :: dt
    type(xml_doc_state), pointer :: xds

    call destroy_xml_doc_state(dt%xds)
    deallocate(dt%xds)
    dt%xds => xds
  end subroutine replace_xds


  function createDocument(impl, namespaceURI, qualifiedName, docType) result(doc)
    type(DOMImplementation), intent(in) :: impl
    character(len=*), intent(in), optional :: namespaceURI
    character(len=*), intent(in), optional :: qualifiedName
    type(Node), pointer, optional :: docType
    type(Node), pointer :: doc, dt

     !FIXMEFIXMEFIXME optional arguments and errors

    doc => createNode(null(), DOCUMENT_NODE, "#document", "")

    if (present(docType)) then
      docType%ownerDocument => doc
      doc%doctype => appendChild(doc, doc%docType)
    endif
    if (.not.associated(doc%docType)) then
      dt => createDocumentType(impl, qualifiedName, "", "")
      dt%ownerDocument => doc
      doc%docType => appendChild(doc, dt)
    endif

    doc%docType%ownerElement => doc
    doc%implementation => FoX_DOM
    doc%documentElement => appendChild(doc, createElementNS(doc, namespaceURI, qualifiedName))

    doc%xds => doc%docType%xds

  end function createDocument


  function createEmptyDocument() result(doc)
    type(Node), pointer :: doc
    
    print*,"creating empty document"
    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => null()
    print*,"created"

    ! FIXME do something with namespaceURI etc 
    doc%doctype => appendChild(doc, createEmptyDocumentType(doc))
    doc%docType%ownerElement => doc
    doc%implementation => FoX_DOM
    doc%documentElement => null()
    doc%xds => doc%docType%xds

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

    call destroyAllNodesRecursively(doc)

    print*, "destroying a node:", doc%nodeType, doc%nodeName
    call destroyNodeContents(doc)
    deallocate(doc)

  end subroutine destroyDocument

')`'dnl

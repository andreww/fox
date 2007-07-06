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
    type(xml_doc_state) :: temp_xds

    if (.not.checkChars(qualifiedName, XML1_0)) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif

    if (.not.checkName(qualifiedName, temp_xds))  then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    ! FIXME check that prefix etc is declared
    elseif (.not.checkPublicId(publicId)) then
      TOHW_m_dom_throw_error(FoX_INVALID_PUBLIC_ID)
    elseif (.not.checkSystemId(systemId)) then
      TOHW_m_dom_throw_error(FoX_INVALID_SYSTEM_ID)
    endif

    dt => createNode(null(), DOCUMENT_TYPE_NODE, qualifiedName, "")
    dt%readonly = .true.
    dt%publicId = vs_str_alloc(publicId)
    dt%systemId = vs_str_alloc(systemId)
    allocate(dt%internalSubset(0)) !FIXME
    dt%ownerDocument => null()
    dt%entities%ownerElement => dt
    dt%notations%ownerElement => dt
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

    dt%entities%ownerElement => dt
    dt%notations%ownerElement => dt

  end function createEmptyDocumentType


  subroutine replace_xds(doc, xds)
    type(Node), pointer :: doc
    type(xml_doc_state), pointer :: xds

    call destroy_xml_doc_state(doc%docExtras%xds)
    deallocate(doc%docExtras%xds)
    doc%docExtras%xds => xds

  end subroutine replace_xds


  function createDocument(impl, namespaceURI, qualifiedName, docType) result(doc)
    type(DOMImplementation), intent(in) :: impl
    character(len=*), intent(in), optional :: namespaceURI
    character(len=*), intent(in), optional :: qualifiedName
    type(Node), pointer :: docType
    type(Node), pointer :: doc, dt

     !FIXMEFIXMEFIXME optional arguments and errors
    ! FIXME change to match empytdocument below

    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => doc ! Makes life easier. DOM compliance in getter

    allocate(doc%docExtras)
    doc%docExtras%implementation => FoX_DOM
    allocate(doc%docExtras%nodelists(0))
    allocate(doc%docExtras%xds)
    call init_xml_doc_state(doc%docExtras%xds)

    if (associated(docType)) then
      dt => docType
      dt%ownerDocument => doc
      doc%docExtras%docType => appendChild(doc, dt)
    else
      dt => createDocumentType(impl, qualifiedName, "", "")
      dt%ownerDocument => doc
    endif
    
    call setDocumentElement(doc, appendChild(doc, createElementNS(doc, namespaceURI, qualifiedName)))

  end function createDocument


  function createEmptyDocument() result(doc)
    type(Node), pointer :: doc
    type(Node), pointer :: dt
    
    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => doc ! Makes life easier. DOM compliance in getter

    allocate(doc%docExtras)
    doc%docExtras%implementation => FoX_DOM
    allocate(doc%docExtras%nodelists(0))
    allocate(doc%docExtras%xds)
    call init_xml_doc_state(doc%docExtras%xds)

    dt => createEmptyDocumentType(doc)
    dt%ownerDocument => doc
    doc%docExtras%doctype => dt

    ! FIXME do something with namespaceURI etc 

  end function createEmptyDocument


  TOHW_subroutine(destroyDocument, (doc))
    type(Node), pointer :: doc
    
    type(Node), pointer :: dt
    type(NodeList) :: np_stack
    integer :: i

! Switch off all GC - since this is GC!
    call setGCstate(doc, .false.)

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    ! Remove docType from tree before destruction:
    dt =>  getDocType(doc)
    if (associated(getParentNode(dt))) &
      dt => removeChild(doc, dt)
    call destroyDocumentType(dt)

    ! Destroy all remaining nodelists

    do i = 1, size(doc%docExtras%nodelists)
     call destroy(doc%docExtras%nodelists(i)%this)
    enddo
    deallocate(doc%docExtras%nodelists)


    ! Destroy all remaining hanging nodes
    do i = 1, doc%docExtras%hangingNodes%length
      call destroy(doc%docExtras%hangingNodes%nodes(i)%this)
    enddo
    if (associated(doc%docExtras%hangingNodes%nodes)) deallocate(doc%docExtras%hangingNodes%nodes)

    call destroy_xml_doc_state(doc%docExtras%xds)
    deallocate(doc%docExtras%xds)

    deallocate(doc%docExtras)

    print*, "destroying a node:", doc%nodeType, doc%nodeName

    call destroyAllNodesRecursively(doc)
    call destroyNodeContents(doc)
    deallocate(doc)

  end subroutine destroyDocument

')`'dnl

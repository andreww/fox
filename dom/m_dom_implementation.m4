TOHW_m_dom_publics(`

  public :: hasFeature
  public :: createDocument
  public :: createDocumentType

  public :: destroyDocument

  public :: createEmptyDocument

')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_function(hasFeature, (impl, feature, version), p)
    type(DOMImplementation), pointer :: impl
    character(len=*), intent(in) :: feature
    character(len=*), intent(in) :: version
    logical :: p

    if (.not.associated(impl)) then
      TOHW_m_dom_throw_error(FoX_IMPL_IS_NULL)
    endif

    if (version=="1.0".or.version=="2.0".or.version=="") then
      p = (toLower(feature)=="core".or.toLower(feature)=="xml")
    else
      p = .false.
    endif

  end function hasFeature


  TOHW_function(createDocumentType, (impl, qualifiedName, publicId, systemId), dt)
    type(DOMImplementation), pointer :: impl
    character(len=*), intent(in) :: qualifiedName
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    type(Node), pointer :: dt
    type(xml_doc_state) :: temp_xds

    if (.not.associated(impl)) then
      TOHW_m_dom_throw_error(FoX_IMPL_IS_NULL)
    endif

    if (.not.checkName(qualifiedName, temp_xds)) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedName, temp_xds))  then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    ! FIXME check that prefix etc is declared
    elseif (.not.checkPublicId(publicId)) then
      TOHW_m_dom_throw_error(FoX_INVALID_PUBLIC_ID)
    elseif (.not.checkSystemId(systemId)) then
      TOHW_m_dom_throw_error(FoX_INVALID_SYSTEM_ID)
    endif

    dt => createNode(null(), DOCUMENT_TYPE_NODE, qualifiedName, "")
    allocate(dt%dtdExtras)
    dt%readonly = .true.
    dt%dtdExtras%publicId => vs_str_alloc(publicId)
    dt%dtdExtras%systemId => vs_str_alloc(systemId)
    allocate(dt%dtdExtras%internalSubset(0)) ! FIXME This is valid behaviour, but we should
                                   ! really be able to get the intSubset from SAX
    dt%ownerDocument => null()

  end function createDocumentType


  TOHW_function(createDocument, (impl, namespaceURI, qualifiedName, docType), doc)
    type(DOMImplementation), pointer :: impl
    character(len=*), intent(in), optional :: namespaceURI
    character(len=*), intent(in), optional :: qualifiedName
    type(Node), pointer :: docType
    type(Node), pointer :: doc, dt
    type(xml_doc_state), pointer :: xds

    if (.not.associated(impl)) then
      TOHW_m_dom_throw_error(FoX_IMPL_IS_NULL)
    elseif (associated(docType)) then 
      if (associated(getOwnerDocument(docType))) then
        TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
      endif
    endif

    allocate(xds)
    if (.not.checkQName(qualifiedName, xds)) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR, (xds))
    elseif (qualifiedName=="xmlns" .or. prefixOfQName(qualifiedName)=="xmlns") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR, (xds))
    endif

    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => doc ! Makes life easier. DOM compliance in getter

    allocate(doc%docExtras)
    doc%docExtras%implementation => FoX_DOM
    allocate(doc%docExtras%nodelists(0))
    doc%docExtras%xds => xds
    call init_xml_doc_state(doc%docExtras%xds)

    doc%docExtras%entities%ownerElement => doc
    doc%docExtras%notations%ownerElement => doc

    if (associated(docType)) then
      dt => docType
      dt%ownerDocument => doc
      doc%docExtras%docType => appendChild(doc, dt)
    endif
    
    call setDocumentElement(doc, appendChild(doc, createElementNS(doc, namespaceURI, qualifiedName)))

  end function createDocument


  function createEmptyDocument() result(doc)
    type(Node), pointer :: doc
    
    doc => createNode(null(), DOCUMENT_NODE, "#document", "")
    doc%ownerDocument => doc ! Makes life easier. DOM compliance maintained in getter

    allocate(doc%docExtras)
    doc%docExtras%implementation => FoX_DOM
    allocate(doc%docExtras%nodelists(0))
    allocate(doc%docExtras%xds)
    call init_xml_doc_state(doc%docExtras%xds)

    doc%docExtras%entities%ownerElement => doc
    doc%docExtras%notations%ownerElement => doc

    ! FIXME do something with namespaceURI etc 

  end function createEmptyDocument


  TOHW_subroutine(destroyDocument, (arg))
    type(Node), pointer :: arg
    
    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

! Switch off all GC - since this is GC!
    call setGCstate(arg, .false.)

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

! Destroy all entities & notations:

    if (associated(arg%docExtras%entities%nodes)) then
      do i = 1, size(arg%docExtras%entities%nodes)
        call destroyAllNodesRecursively(arg%docExtras%entities%nodes(i)%this)
        call destroy(arg%docExtras%entities%nodes(i)%this)
      enddo
      deallocate(arg%docExtras%entities%nodes)
    endif
    if (associated(arg%docExtras%notations%nodes)) then
      do i = 1, size(arg%docExtras%notations%nodes)
        call destroy(arg%docExtras%notations%nodes(i)%this)
      enddo
      deallocate(arg%docExtras%notations%nodes)
    endif

! Destroy all remaining nodelists

    do i = 1, size(arg%docExtras%nodelists)
     call destroy(arg%docExtras%nodelists(i)%this)
    enddo
    deallocate(arg%docExtras%nodelists)

    ! Destroy all remaining hanging nodes
    print*,"HANGING NODES", arg%docExtras%hangingNodes%length
    do i = 1, arg%docExtras%hangingNodes%length
      print*, "i", i
      call destroy(arg%docExtras%hangingNodes%nodes(i)%this)
    enddo
    if (associated(arg%docExtras%hangingNodes%nodes)) deallocate(arg%docExtras%hangingNodes%nodes)

    call destroy_xml_doc_state(arg%docExtras%xds)
    deallocate(arg%docExtras%xds)

    deallocate(arg%docExtras)
    call destroyAllNodesRecursively(arg)
    call destroyNodeContents(arg)
    deallocate(arg)

  end subroutine destroyDocument

')`'dnl

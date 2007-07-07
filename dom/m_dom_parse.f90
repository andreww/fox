module m_dom_parse

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_error, only: FoX_error
  use m_common_entities, only: entity_t
  use m_common_notations, only: notation
  use m_common_struct, only: xml_doc_state
  use FoX_common, only: dictionary_t, len
  use FoX_common, only: getQName, getValue, getLocalname, getURI
  use m_sax_parser, only: sax_parse
  use FoX_sax, only: xml_t
  use FoX_sax, only: open_xml_file, open_xml_string, close_xml_t

  use m_dom_dom, only: Node, NamedNodeMap, hasChildNodes, getFirstChild
  use m_dom_dom, only: DOCUMENT_NODE, getOwnerDocument, getDocumentElement
  use m_dom_dom, only: createProcessingInstruction, getDocType, createAttributeNS
  use m_dom_dom, only: createComment, getEntities, item, setSpecified
  use m_dom_dom, only: createElementNS, getNotations, getLastChild
  use m_dom_dom, only: createTextNode, createEntity, getAttributes
  use m_dom_dom, only: createNotation, setNamedItem, getNodeName
  use m_dom_dom, only: createEmptyDocument, getXMLVersion, createDocumentType
  use m_dom_dom, only: getParentNode, setDocumentElement, getDocType, setDocType
  use m_dom_dom, only: append, getNodeType, setReadOnly, getLength, getChildNodes
  use m_dom_dom, only: removeChild, appendChild, getNotations, setAttributeNodeNS, setvalue
  use m_dom_dom, only: setAttributeNodeNS, replace_xds, setGCstate, createCdataSection
  use m_dom_dom, only: createEntityReference, destroyAllNodesRecursively, setIllFormed
  use m_dom_debug, only: dom_debug

  implicit none
  private

  public :: parsefile

  type(Node), pointer, private, save  :: mainDoc => null()
  type(Node), pointer, private, save  :: current => null()

  logical :: cdata_sections, cdata
  logical :: entities_expand
  character, pointer :: inEntity(:) => null()

contains

  subroutine startElement_handler(URI, localname, name, attrs)
    character(len=*),   intent(in) :: URI
    character(len=*),   intent(in) :: localname
    character(len=*),   intent(in) :: name

    type(dictionary_t), intent(in) :: attrs
   
    type(Node), pointer :: el, temp
    integer              :: i

    if (associated(inEntity)) return

    print*, 'XML', getXmlVersion(mainDoc)

    if (dom_debug) &
      write(*,'(4a)') "Adding node for element: {",URI,'}', localname

    el => createElementNS(mainDoc, URI, name)

    do i = 1, len(attrs)
      if (dom_debug) print *, "Adding attribute: ", &
        getQName(attrs, i), ":",getValue(attrs, i)
      temp => createAttributeNS(mainDoc, getURI(attrs, i), getQName(attrs, i))
      call setValue(temp, getValue(attrs, i))
      call setSpecified(temp, .true.)
      temp =>  setAttributeNodeNS(el, temp)
      ! FIXME check specifiedness
    enddo

    !print*,"ELASS"
    !print*,associated(getOwnerDocument(el))

    if (getNodeType(current)==DOCUMENT_NODE) then
      call setDocumentElement(mainDoc, el)
    endif
    !print*, associated(getOwnerDocument(getDocumentElement(mainDoc)))
    current => appendChild(current,el)
    print*,getNodeType(el)
    if (associated(getOwnerDocument(el))) print*,"YES"

    cdata = .false.
    
  end subroutine startElement_handler

  subroutine endElement_handler(URI, localName, name)
    character(len=*), intent(in)     :: URI
    character(len=*), intent(in)     :: localname
    character(len=*), intent(in)     :: name

    if (associated(inEntity)) return

    if (dom_debug) &
      write(*,'(4a)') "Ending node for element: {",URI,'}', localname

    current => getparentNode(current)
  end subroutine endElement_handler

  subroutine characters_handler(chunk)
    character(len=*), intent(in) :: chunk

    type(Node), pointer :: temp
    
    if (associated(inEntity)) return

    if (cdata) then
      temp => createCdataSection(mainDoc, chunk)
    else
      temp => createTextNode(mainDoc, chunk)
    endif
    temp => appendChild(current, temp)

!    temp => getFirstChild(current)

  end subroutine characters_handler

  subroutine comment_handler(comment)
    character(len=*), intent(in) :: comment

    type(Node), pointer :: temp

    if (associated(inEntity)) return

    if (dom_debug) print *, "Got COMMENT: |", comment, "|"

    temp => createComment(mainDoc, comment)
    temp => appendChild(current, temp)

  end subroutine comment_handler

  subroutine processingInstruction_handler(target, data)
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data

    type(Node), pointer :: temp

    if (associated(inEntity)) return

    print*,'createPI'
    temp => createProcessingInstruction(mainDoc, target, data)
    temp => appendChild(current, temp)
    print*,'createPIdone'
  end subroutine processingInstruction_handler

  subroutine startDocument_handler
    print*,'allocating mainDoc'

    mainDoc => createEmptyDocument()
    current => mainDoc
    call setGCstate(mainDoc, .false.)

    print*,'mainDoc allocated'
  end subroutine startDocument_handler

  subroutine endDocument_Handler
    ! FIXME also walk across tree, and fix all entityReference nodes
    ! so they have appropriate children
    !FIXME set readonly appropriately
    call setGCstate(mainDoc, .true.)
  end subroutine endDocument_Handler

  subroutine startDTD_handler(name, publicId, systemId)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in), optional :: systemId

    type(Node), pointer :: np

    np => getDocType(mainDoc)
    call setDocType(np, name, publicId, systemId)
    np => appendChild(mainDoc, np)

  end subroutine startDTD_handler

  subroutine FoX_endDTD_handler(state)
    type(xml_doc_state), pointer :: state

    type(Node), pointer :: dt, np, nt
    type(NamedNodeMap), pointer :: entities, notations
    type(entity_t), pointer :: thisEntity
    type(notation), pointer :: thisNotation
    integer :: i

    dt => getDocType(mainDoc)
    call replace_xds(mainDoc, state)
    call setGCstate(mainDoc, .false.)

    ! FIXME also copy internalSubset from xds

  end subroutine FoX_endDTD_handler

  subroutine notationDecl_handler(name, publicId, systemId)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in) :: systemId
    
    type(Node), pointer :: np

    np => createNotation(mainDoc, name, publicId, systemId)
! FIXME what if two entities with the same name
    np => setNamedItem(getNotations(getDocType(mainDoc)), np)
  end subroutine notationDecl_handler

  subroutine startCdata_handler()
    if (cdata_sections) cdata = .true.
  end subroutine startCdata_handler
  subroutine endCdata_handler()
    if (cdata_sections) cdata = .false.
  end subroutine endCdata_handler

  subroutine internalEntityDecl_handler(name, value)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value
    
    type(Node), pointer :: oldcurrent
    type(xml_t) :: subsax

    oldcurrent => current
    current => createEntity(mainDoc, name, "", "", "")

    call open_xml_string(subsax, value)
    call sax_parse(subsax%fx, subsax%fb,                                        &
                   startElement_handler=startElement_handler,                   &
                   endElement_handler=endElement_handler,                       &
                   characters_handler=characters_handler,                       &
                   startCdata_handler=startCdata_handler,                       &
                   endCdata_handler=endCdata_handler,                           &
                   comment_handler=comment_handler,                             &
                   processingInstruction_handler=processingInstruction_handler, &
                   error_handler=entityErrorHandler,                            &
                   startInCharData = .true.)
    call close_xml_t(subsax)

! FIXME what if two entities with the same name
    current => setNamedItem(getEntities(getDocType(mainDoc)), current)

    current => oldcurrent

  end subroutine internalEntityDecl_handler

  subroutine entityErrorHandler(msg)
    character(len=*), intent(in) :: msg

    !This gets called if parsing of an internal entity failed. If so,
    !then we need to destroy all nodes which were being generated as
    !children of this entity, then mark the entity as ill-formed - but
    !otherwise carry on parsing the document, and only throw an error
    !if a reference is made to it.

    call destroyAllNodesRecursively(current)
    call setIllFormed(current, .true.)
  end subroutine entityErrorHandler

  subroutine externalEntityDecl_handler(name, publicId, systemId)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
!FIXME which are optional, check order
    type(Node), pointer :: np

    np => createEntity(mainDoc, name, publicId, systemId, "")    

  end subroutine externalEntityDecl_handler

  subroutine unparsedEntityDecl_handler(name, publicId, systemId, notationName)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    character(len=*), intent(in) :: notationName
!FIXME which are optional, check order
    type(Node), pointer :: np

    np => createEntity(mainDoc, name, publicId, systemId, notationName)    

  end subroutine unparsedEntityDecl_handler

  subroutine startEntity_handler(name)
    character(len=*), intent(in) :: name
    
    if (entities_expand) then
      if (.not.associated(inEntity)) then
        current => appendChild(current, createEntityReference(mainDoc, name))
        inEntity => vs_str_alloc(name)
      endif
    endif
  end subroutine startEntity_handler

  subroutine endEntity_handler(name)
    character(len=*), intent(in) :: name
    
    if (entities_expand) then
      if (str_vs(inEntity)==name) deallocate(inEntity)
    endif

  end subroutine endEntity_handler

  subroutine skippedEntity_handler(name)
    character(len=*), intent(in) :: name
    
    if (associated(inEntity)) return
    current => appendChild(current, createEntityReference(mainDoc, name))
  end subroutine skippedEntity_handler

  function parsefile(filename, configuration)

! FIXME should do string too.

    character(len=*), intent(in) :: filename
    character(len=*), intent(in), optional :: configuration

    type(Node), pointer :: parsefile, dt, np, nt
    type(NamedNodeMap), pointer :: entities, notations
    type(entity_t), pointer :: thisEntity
    type(notation), pointer :: thisNotation
    type(xml_t) :: fxml
    integer :: i, iostat
    
    if (present(configuration)) then
      cdata_sections = (scan("cdata-sections", configuration)>0)
      entities_expand = (scan("entities", configuration)>0)
    else
      cdata_sections = .false.
      entities_expand = .false.
    endif

    call open_xml_file(fxml, filename, iostat)
    if (iostat /= 0) then
      call FoX_error("Cannot open file")
    endif

! We use internal sax_parse rather than public interface in order
! to use internal callbacks to get extra info.


    call sax_parse(fxml%fx, fxml%fb,&
      characters_handler=characters_handler,            &
      endDocument_handler=endDocument_handler,           &
      endElement_handler=endElement_handler,            &
      !endPrefixMapping_handler,      &
      !ignorableWhitespace_handler,   &
      processingInstruction_handler=processingInstruction_handler, &
      ! setDocumentLocator
      skippedEntity_handler=skippedEntity_handler,         &
      startDocument_handler=startDocument_handler,         & 
      startElement_handler=startElement_handler,          &
      !startPrefixMapping_handler,    &
      notationDecl_handler=notationDecl_handler,          &
      unparsedEntityDecl_handler=unparsedEntityDecl_handler, &
      !error_handler,                 &
      !fatalError_handler,            &
      !warning_handler,               &
      !attributeDecl_handler,         &
      !elementDecl_handler,           &
      externalEntityDecl_handler=externalEntityDecl_handler, &
      internalEntityDecl_handler=internalEntityDecl_handler,    &
      comment_handler=comment_handler,              &
      endCdata_handler=endCdata_handler,             &
      !endDTD_handler=endDTD_handler,                &
      endEntity_handler=endEntity_handler,             &
      startCdata_handler=startCdata_handler,    &
      startDTD_handler=startDTD_handler,          &
      startEntity_handler=startEntity_handler, &
      FoX_endDTD_handler=FoX_endDTD_handler)


    call close_xml_t(fxml)

    parsefile => mainDoc
    mainDoc => null()
    
  end function parsefile

end module m_dom_parse

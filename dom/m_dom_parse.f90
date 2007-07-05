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
  use FoX_sax, only: open_xml_file, close_xml_t

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
  use m_dom_dom, only: setAttributeNodeNS, replace_xds, setGCstate
  use m_dom_debug, only: dom_debug

  implicit none
  private

  public :: parsefile

  type(Node), pointer, private, save  :: mainDoc => null()
  type(Node), pointer, private, save  :: current => null()

contains

  subroutine startElement_handler(URI, localname, name, attrs)
    character(len=*),   intent(in) :: URI
    character(len=*),   intent(in) :: localname
    character(len=*),   intent(in) :: name

    type(dictionary_t), intent(in) :: attrs
   
    type(Node), pointer :: el, temp
    integer              :: i

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
    
  end subroutine startElement_handler

  subroutine endElement_handler(URI, localName, name)
    character(len=*), intent(in)     :: URI
    character(len=*), intent(in)     :: localname
    character(len=*), intent(in)     :: name

    if (dom_debug) &
      write(*,'(4a)') "Ending node for element: {",URI,'}', localname

    current => getparentNode(current)
  end subroutine endElement_handler

  subroutine characters_handler(chunk)
    character(len=*), intent(in) :: chunk

    type(Node), pointer :: temp
    
    print *, "Got PCDATA: |", chunk, "|"

    temp => createTextNode(mainDoc, chunk)
    print*, "pcdata:", associated(temp)
    temp => appendChild(current, temp)

    print*, "pcdata:", hasChildNodes(current)
    temp => getFirstChild(current)
    print*, "pcdata:", associated(temp)

  end subroutine characters_handler

  subroutine comment_handler(comment)
    character(len=*), intent(in) :: comment

    type(Node), pointer :: temp

    if (dom_debug) print *, "Got COMMENT: |", comment, "|"

    temp => createComment(mainDoc, comment)
    temp => appendChild(current, temp)

  end subroutine comment_handler

  subroutine processingInstruction_handler(target, data)
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data

    type(Node), pointer :: temp

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
    call setGCstate(mainDoc, .true.)
  end subroutine endDocument_Handler

  subroutine startDTD_handler(name, publicId, systemId)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in), optional :: systemId

    type(Node), pointer :: np

    np => getDocType(mainDoc)
    ! If we have added any more document children by now (comments or PIs), we
    ! need to move the docType node to the end of the list of document children ...
    np => removeChild(mainDoc, np)
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

!FIXME set readonly appropriately below

    dt => getDocType(mainDoc)
    call replace_xds(mainDoc, state)
    call setGCstate(mainDoc, .false.)

    entities => getEntities(dt)
    notations => getNotations(dt)
    do i = 1, ubound(state%entityList%list, 1)
      thisEntity => state%entityList%list(i)
      np => createEntity(mainDoc, str_vs(thisEntity%code), &
                         str_vs(thisEntity%publicId), &
                         str_vs(thisEntity%systemId), &
                         str_vs(thisEntity%notation))
      if (.not.(thisEntity%external)) then
        ! Here we assume all internal entities are simply text
        ! FIXME in fact if they hold markup, we should indicate
        ! that this is so. We cannot parse them, because this
        ! might fail, but the failure only matters if someone
        ! tries to add an entity reference later on.
        ! They contain markup iff there is a '<' in them.
        ! Unless maybe there is an & - have we expanded at this point?
        nt => createTextNode(mainDoc, str_vs(thisEntity%repl))
        nt => appendChild(np, nt)
      endif
      np => setNamedItem(entities, np)
    enddo
    do i = 1, ubound(state%nlist%notations, 1)
      thisNotation => state%nlist%notations(i)
      np => createNotation(mainDoc, str_vs(thisNotation%name), &
                         str_vs(thisEntity%publicId), &
                         str_vs(thisEntity%systemId))
      np => setNamedItem(notations, np)
    enddo

  end subroutine FoX_endDTD_handler

  function parsefile(filename, verbose, sax_verbose)

    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: verbose
    logical, intent(in), optional :: sax_verbose

    type(Node), pointer :: parsefile, dt, np, nt
    type(NamedNodeMap), pointer :: entities, notations
    type(entity_t), pointer :: thisEntity
    type(notation), pointer :: thisNotation
    type(xml_t) :: fxml
    integer :: i, iostat

    if (present(verbose)) then
       dom_debug = verbose
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
      !skippedEntity_handler,         &
      startDocument_handler=startDocument_handler,         & 
      startElement_handler=startElement_handler,          &
      !startPrefixMapping_handler,    &
      !notationDecl_handler=notationDecl_handler,          &
      !unparsedEntityDecl_handler,    &
      !error_handler,                 &
      !fatalError_handler,            &
      !warning_handler,               &
      !attributeDecl_handler,         &
      !elementDecl_handler,           &
      !externalEntityDecl_handler,    &
      !internalEntityDecl_handler,    &
      comment_handler=comment_handler,              &
      !endCdata_handler,              &
      !endDTD_handler=endDTD_handler,                &
      !endEntity_handler,             &
      !startCdata_handler,            &
      startDTD_handler=startDTD_handler,          &
      !startEntity_handler
      FoX_endDTD_handler=FoX_endDTD_handler)

    call close_xml_t(fxml)

    parsefile => mainDoc
    mainDoc => null()
    
  end function parsefile

end module m_dom_parse

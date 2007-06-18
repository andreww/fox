module m_dom_parse

  use m_common_array_str, only: vs_str_alloc
  use m_common_error, only: FoX_error
  use FoX_common, only: dictionary_t, len
  use FoX_common, only: getQName, getValue, getLocalname, getURI
  use FoX_sax, only: parse, xml_t
  use FoX_sax, only: open_xml_file, close_xml_t

  use m_dom_dom, only: Node, NamedNodeMap, hasChildNodes, getFirstChild
  use m_dom_dom, only: createnode, DOCUMENT_NODE, destroynode
  use m_dom_dom, only: createProcessingInstruction
  use m_dom_dom, only: createComment
  use m_dom_dom, only: createElementNS
  use m_dom_dom, only: createTextNode
  use m_dom_dom, only: createNotation
  use m_dom_dom, only: createEmptyDocument
  use m_dom_dom, only: getParentNode, setDocumentElement, getDocumentType, setDocumentType
  use m_dom_dom, only: append, getNodeType, setReadOnly
  use m_dom_dom, only: appendchild, getNotations
  use m_dom_dom, only: setAttributeNS
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

    if (dom_debug) &
      write(*,'(4a)') "Adding node for element: {",URI,'}', localname

    el => createElementNS(mainDoc, URI, name)

    do i = 1, len(attrs)
      if (dom_debug) print *, "Adding attribute: ", &
        getQName(attrs, i), ":",getValue(attrs, i)
      call setAttributeNS(el, getURI(attrs, i), getQName(attrs, i), getValue(attrs, i))
    enddo

    if (getNodeType(current)==DOCUMENT_NODE) then
      call setDocumentElement(mainDoc, el)
    endif

    current => appendChild(current,el)
    
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
    
    if (dom_debug) print *, "Got PCDATA: |", chunk, "|"

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

    print*,'mainDoc allocated'
  end subroutine startDocument_handler

  subroutine startDTD_handler(name, publicId, systemId)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in), optional :: systemId

    type(Node), pointer :: np

    print*,'startingDTD'

    np => getDocumentType(mainDoc)
    call setDocumentType(np, name, publicId, systemId)

  end subroutine startDTD_handler

  subroutine notationDecl_handler(name, publicId, systemId)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in), optional :: systemId
    
    type(Node), pointer :: np, dt

    type(NamedNodeMap), pointer :: notations

    print*, 'adding notation'

    np => createNotation(mainDoc, name, publicId, systemId)
    dt => getDocumentType(mainDoc)
    notations => getNotations(dt)
    call setReadonly(notations, .false.)
    call append(notations, np)
    call setReadonly(notations, .false.)

  end subroutine notationDecl_handler

  function parsefile(filename, verbose, sax_verbose)

    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: verbose
    logical, intent(in), optional :: sax_verbose

    type(Node), pointer :: parsefile

    type(xml_t) :: fxml
    integer :: iostat

    if (present(verbose)) then
       dom_debug = verbose
    endif

    call open_xml_file(fxml, filename, iostat)
    if (iostat /= 0) then
      call FoX_error("Cannot open file")
    endif

    call parse(fxml,&
      characters_handler,            &
      !endDocument_handler,           &
      endElement_handler=endElement_handler,            &
      !endPrefixMapping_handler,      &
      !ignorableWhitespace_handler,   &
      processingInstruction_handler=processingInstruction_handler, &
      ! setDocumentLocator
      !skippedEntity_handler,         &
      startDocument_handler=startDocument_handler,         & 
      startElement_handler=startElement_handler,          &
      !startPrefixMapping_handler,    &
      notationDecl_handler=notationDecl_handler,          &
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
      startDTD_handler=startDTD_handler          &
      !startEntity_handler
      )
    
    call close_xml_t(fxml)
!    if (dom_debug) print *, "Number of allocated nodes: ", getNumberofAllocatedNodes()
    
    !    call createDocument(mainDoc, main)
    parsefile => mainDoc
    mainDoc => null()
    
  end function parsefile
  
end module m_dom_parse

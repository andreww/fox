module m_dom_parse

  use m_common_array_str, only: vs_str_alloc
  use m_common_error, only: FoX_error
  use FoX_common, only: dictionary_t, len
  use FoX_common, only: getQName, getValue, getLocalname, getURI
  use FoX_sax, only: parse, xml_t
  use FoX_sax, only: open_xml_file, close_xml_t

  use m_dom_types, only: fNode, fDocumentNode, getnumberofallocatednodes
  use m_dom_types, only: createnode, DOCUMENT_NODE, destroynode
  use m_dom_document, only: createProcessingInstruction
  use m_dom_document, only: createComment
  use m_dom_document, only: createElementNS
  use m_dom_document, only: createTextNode
  use m_dom_implementation, only: createEmptyDocument
  use m_dom_node, only: appendchild
  use m_dom_node, only: getparentnode
  use m_dom_element, only: setAttributeNS
  use m_dom_debug, only: dom_debug

  implicit none
  private

  public :: parsefile

  type(fDocumentNode), pointer, private, save  :: mainDoc => null()
  type(fNode), pointer, private, save  :: current => null()

contains

  subroutine startElement_handler(URI, localname, name, attrs)
    character(len=*),   intent(in) :: URI
    character(len=*),   intent(in) :: localname
    character(len=*),   intent(in) :: name

    type(dictionary_t), intent(in) :: attrs
   
    type(fnode), pointer :: el, temp
    integer              :: i

    if (dom_debug) &
      write(*,'(4a)') "Adding node for element: {",URI,'}', localname

    el => createElementNS(mainDoc, URI, name)

    do i = 1, len(attrs)
       if (dom_debug) print *, "Adding attribute: ", &
         getQName(attrs, i), ":",getValue(attrs, i)
        temp => setAttributeNS(el, getURI(attrs, i), getQName(attrs, i), getValue(attrs, i))
    enddo

    if (.not.associated(current)) then
      current => el ! This is the document element
    else
      current => appendChild(current,el)
    endif
    
  end subroutine startElement_handler

  subroutine endElement_handler(URI, localName, name)
    character(len=*), intent(in)     :: URI
    character(len=*), intent(in)     :: localname
    character(len=*), intent(in)     :: name

    if (dom_debug) &
      write(*,'(4a)') "Ending node for element: {",URI,'}', localname

    if (associated(mainDoc%documentElement, current)) then
      current => null()
    else
      current => getParentNode(current)
    endif
  end subroutine endElement_handler

  subroutine characters_handler(chunk)
    character(len=*), intent(in) :: chunk

    type(fnode), pointer :: temp
    
    if (dom_debug) print *, "Got PCDATA: |", chunk, "|"

    temp => createTextNode(mainDoc, chunk)
    temp => appendChild(current,temp)

  end subroutine characters_handler

  subroutine comment_handler(comment)
    character(len=*), intent(in) :: comment

    type(fnode), pointer :: temp

    if (dom_debug) print *, "Got COMMENT: |", comment, "|"

    temp => createComment(mainDoc, comment)
    temp => appendChild(current,temp)

  end subroutine comment_handler

  subroutine processingInstruction_handler(target, data)
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data

    type(fnode), pointer :: temp

    print*,'createPI'
    temp => createProcessingInstruction(mainDoc, target, data)
    temp => appendChild(current, temp)
    print*,'createPIdone'
  end subroutine processingInstruction_handler

  subroutine startDocument_handler
    print*,'allocating mainDoc'

    mainDoc => createEmptyDocument()
    current => mainDoc%documentElement
  end subroutine startDocument_handler

  subroutine startDTD_handler(name, publicId, systemId)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in), optional :: systemId

    deallocate(mainDoc%docType%name)
    mainDoc%docType%name => vs_str_alloc(name)
    if (present(publicId)) then
      deallocate(mainDoc%docType%publicId)
      mainDoc%docType%publicId => vs_str_alloc(publicId)
    endif
    if (present(systemId)) then
      deallocate(mainDoc%docType%systemId)
      mainDoc%docType%systemId => vs_str_alloc(systemId)
    endif

  end subroutine startDTD_handler


  function parsefile(filename, verbose, sax_verbose)

    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: verbose
    logical, intent(in), optional :: sax_verbose

    type(fDocumentNode), pointer :: parsefile

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
      !notationDecl_handler,          &
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
    if (dom_debug) print *, "Number of allocated nodes: ", getNumberofAllocatedNodes()
    
    !    call createDocument(mainDoc, main)
    parsefile => mainDoc
    mainDoc => null()
    
  end function parsefile
  
end module m_dom_parse

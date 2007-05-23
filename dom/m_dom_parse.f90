module m_dom_parse

  use FoX_common, only: dictionary_t, len
  use FoX_common, only: getQName, getValue, getLocalname, getURI
  use FoX_sax, only: parse, xml_t
  use FoX_sax, only: open_xml_file, close_xml_t

  use m_dom_types, only: fNode, fDocumentNode, getnumberofallocatednodes
  use m_dom_types, only: createnode, DOCUMENT_NODE, destroynode
  use m_dom_document, only: createcomment
  use m_dom_document, only: createcdatasection
  use m_dom_document, only: createElement, createElementNS
  use m_dom_document, only: createtextnode
  use m_dom_implementation, only: createDocument, createDocumentType
  use m_dom_node, only: appendchild
  use m_dom_node, only: getparentnode
  use m_dom_element, only: setattribute
  use m_dom_debug, only: dom_debug

  use m_strings, only : string, stringify, assignment(=), operator(==)

  implicit none
  
  private

  public :: parsefile

  type(fDocumentNode), pointer, private, save  :: mainDoc => null()
  type(fNode), pointer, private, save  :: main => null()
  type(fNode), pointer, private, save  :: current => null()
  type(fNode), pointer, private, save  :: documentElement => null()

  !type(nsDictionary), save :: nsDict


CONTAINS

  subroutine startElement_handler(URI, localname, name, attrs)
    character(len=*),   intent(in) :: URI
    character(len=*),   intent(in) :: localname
    character(len=*),   intent(in) :: name

    type(dictionary_t), intent(in) :: attrs
   
    type(fnode), pointer :: temp
    integer              :: i

    if (.not.associated(documentElement)) &
      mainDoc = createDocument(URI, name, createDocumentType())

    if (dom_debug) &
      write(*,'(4a)') "Adding node for element: {",URI,'}', localname

    temp => createElement(mainDoc, name)
    temp => createElementNS(mainDoc, name, URI, localName)
    current => appendChild(current,temp)

!
!   Add attributes
!
    do i = 1, len(attrs)
       if (dom_debug) print *, "Adding attribute: ", &
         getQName(attrs, i), ":",getValue(attrs, i)
       call setAttributeNS(current,getQName(attrs, i),getValue(attrs, i), getURI(attrs, i), getLocalName(attrs, i))
    enddo

    current % namespaceURI = URI
    if (current % namespaceURI == '') then
      ! this prefix is not bound to a URI - element localname is full name
      continue
    endif
    current % localname = localname

  end subroutine startElement_handler

!---------------------------------------------------------

  subroutine endElement_handler(URI, localName, name)
    character(len=*), intent(in)     :: URI
    character(len=*), intent(in)     :: localname
    character(len=*), intent(in)     :: name

!!AG for IBM    type(fnode), pointer :: np

    if (dom_debug) &
      write(*,'(4a)') "Ending node for element: {",URI,'}', localname
!!AG for IBM    np => getParentNode(current)
!!AG for IBM    current => np
    current => getParentNode(current)
  end subroutine endElement_handler

!---------------------------------------------------------

  subroutine characters_handler(chunk)
    character(len=*), intent(in) :: chunk

    type(fnode), pointer :: temp, dummy
    
    if (dom_debug) print *, "Got PCDATA: |", chunk, "|"

    temp => createTextNode(mainDoc, chunk)
    dummy => appendChild(current,temp)

  end subroutine characters_handler

!---------------------------------------------------------

  subroutine comment_handler(comment)
    character(len=*), intent(in) :: comment

    type(fnode), pointer :: temp, dummy

    if (dom_debug) print *, "Got COMMENT: |", comment, "|"

    temp => createComment(mainDoc, comment)
    dummy => appendChild(current,temp)

  end subroutine comment_handler

  subroutine startDocument_handler
    print*,'allocating mainDoc'
    allocate(mainDoc)
    main => createNode()
    main % nodeType = DOCUMENT_NODE
    current => main
  end subroutine startDocument_handler



!***************************************************
!   PUBLIC PROCEDURES
!***************************************************


  function parsefile(filename, verbose, sax_verbose)

    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: verbose
    logical, intent(in), optional :: sax_verbose

    type(fDocumentNode), pointer :: parsefile

    logical :: sax_debug = .false.

    type(xml_t) :: fxml
    integer :: iostat

    if (present(verbose)) then
       dom_debug = verbose
    endif

    if (present(sax_verbose)) then
       sax_debug = sax_verbose
    endif
    
    call open_xml_file(fxml, filename, iostat)
    if (iostat /= 0) then
      call FoX_error("Cannot open file")
    endif

    call parse(fxml,&
      characters_handler,            &
      !endDocument_handler,           &
      endElement_handler,            &
      !endPrefixMapping_handler,      &
      !ignorableWhitespace_handler,   &
      !processingInstruction_handler, &
      ! setDocumentLocator
      !skippedEntity_handler,         &
      startDocument_handler,         & 
      startElement_handler,          &
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
      comment_handler               &
      !endCdata_handler,              &
      !endDTD_handler,                &
      !endEntity_handler,             &
      !startCdata_handler,            &
      !startDTD_handler,              &
      !startEntity_handler
      )
    
    call close_xml_t(fxml)
    if (dom_debug) print *, "Number of allocated nodes: ", getNumberofAllocatedNodes()
    
    !    call createDocument(mainDoc, main)
    parsefile => mainDoc
    mainDoc => null()
    
  end function parsefile
  
end module m_dom_parse

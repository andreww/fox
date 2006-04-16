module m_dom_parse

  use m_dom_types, only: fNode, fDocumentNode, getnumberofallocatednodes
  use m_dom_types, only: createnode, DOCUMENT_NODE, destroynode
  use m_dom_document, only: createdocument
  use m_dom_document, only: createcomment
  use m_dom_document, only: createcdatasection
  use m_dom_document, only: createelement
  use m_dom_document, only: createtextnode
  use m_dom_node, only: appendchild
  use m_dom_node, only: getparentnode
  use m_dom_element, only: setattribute
  use m_dom_debug, only: dom_debug

  use xmlf90_sax, only: xml_parse, xml_t, dictionary_t, len
  use xmlf90_sax, only: open_xmlfile, close_xmlfile
  use xmlf90_sax, only: get_key, get_value

  use m_strings, only : string, stringify, assignment(=), operator(==)

  implicit none
  
  private

  public :: parsefile

  type(fDocumentNode), pointer, private, save  :: mainDoc => null()
  type(fNode), pointer, private, save  :: main => null()
  type(fNode), pointer, private, save  :: current => null()

  !type(nsDictionary), save :: nsDict


CONTAINS

  subroutine begin_element_handler(URI, localname, name,attrs)
    character(len=*),   intent(in) :: URI
    character(len=*),   intent(in) :: localname
    character(len=*),   intent(in) :: name

    type(dictionary_t), intent(in) :: attrs
   
    type(fnode), pointer :: temp
    character(len=400)   :: attr_name, attr_value
    integer              :: status
    integer              :: i

    if (dom_debug) write(*,'(4a)'), "Adding node for element: {",URI,'}', localname

    temp => createElement(mainDoc, name)
    current => appendChild(current,temp)

!
!   Add attributes
!
    do i = 1, len(attrs)
       if (dom_debug) print *, "Adding attribute: ", &
         get_key(attrs, i), ":",get_value(attrs, i)
       call setAttribute(current,get_key(attrs, i),get_value(attrs, i))
    enddo

    current % namespaceURI = URI
    if (current % namespaceURI == '') then
      ! this prefix is not bound to a URI - element localname is full name
    endif
    current % localname = localname

  end subroutine begin_element_handler

!---------------------------------------------------------

  subroutine end_element_handler(URI, localName, name)
    character(len=*), intent(in)     :: URI
    character(len=*), intent(in)     :: localname
    character(len=*), intent(in)     :: name

!!AG for IBM    type(fnode), pointer :: np

    if (dom_debug) write(*,'(4a)'), "Ending node for element: {",URI,'}', localname
!!AG for IBM    np => getParentNode(current)
!!AG for IBM    current => np
    current => getParentNode(current)
  end subroutine end_element_handler

!---------------------------------------------------------

  subroutine pcdata_chunk_handler(chunk)
    character(len=*), intent(in) :: chunk

    type(fnode), pointer :: temp, dummy
    
    if (dom_debug) print *, "Got PCDATA: |", chunk, "|"

    temp => createTextNode(mainDoc, chunk)
    dummy => appendChild(current,temp)

  end subroutine pcdata_chunk_handler

!---------------------------------------------------------

  subroutine comment_handler(comment)
    character(len=*), intent(in) :: comment

    type(fnode), pointer :: temp, dummy

    if (dom_debug) print *, "Got COMMENT: |", comment, "|"

    temp => createComment(mainDoc, comment)
    dummy => appendChild(current,temp)

  end subroutine comment_handler
!---------------------------------------------------------
  subroutine cdata_section_handler(chunk)
    character(len=*), intent(in) :: chunk

    type(fnode), pointer :: temp, dummy
    
    if (dom_debug) print *, "Got CDATA_SECTION: |", chunk, "|"

    temp => createCdataSection(mainDoc, chunk)
    dummy => appendChild(current,temp)

  end subroutine cdata_section_handler

!---------------------------------------------------------

  subroutine start_document_handler
    print*,'allocating mainDoc'
    allocate(mainDoc)
    main => createNode()
    main % nodeType = DOCUMENT_NODE
    current => main
  end subroutine start_document_handler



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
    
    call open_xmlfile(filename, fxml, iostat)

    if (iostat /= 0) then
       stop "Cannot open file."
    endif
    
    call xml_parse(fxml,  &
         start_document_handler=start_document_handler, &
         begin_element_handler=begin_element_handler, &
         end_element_handler=end_element_handler, &
         pcdata_chunk_handler=pcdata_chunk_handler, &
         comment_handler=comment_handler, &
         cdata_section_handler=cdata_section_handler, &
         verbose = sax_debug)    
    call close_xmlfile(fxml)

    if (dom_debug) print *, "Number of allocated nodes: ", getNumberofAllocatedNodes()

    call createDocument(mainDoc, main)
    parsefile => mainDoc
    mainDoc => null()

  end function parsefile

end module m_dom_parse

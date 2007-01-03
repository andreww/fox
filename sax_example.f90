program sax_example
  !
  ! Example driver for a stand-alone parsing of an xml document
  !
  use FoX_sax

  use m_handlers      ! Defines begin_element, end_element, pcdata_chunk, etc

  implicit none

  integer :: iostat
  type(xml_t)  :: fxml

  call open_xml_file(fxml, "test.xml", iostat)
  if (iostat /= 0) then
    write(*,*) "Cannot open file."
    stop
  endif

  call sax_parse_go(fxml,&
    characters_handler,            &
    endDocument_handler,           &
    endElement_handler,            &
    endPrefixMapping_handler,      &
    ignorableWhitespace_handler,   &
    processingInstruction_handler, &
    ! setDocumentLocator
    skippedEntity_handler,         &
    startDocument_handler,         & 
    startElement_handler,          &
    startPrefixMapping_handler,    &
    notationDecl_handler,          &
    unparsedEntityDecl_handler,    &
    error_handler,                 &
    fatalError_handler,            &
    warning_handler,               &
    attributeDecl_handler,         &
    elementDecl_handler,           &
    externalEntityDecl_handler,    &
    internalEntityDecl_handler,    &
    comment_handler,               &
    endCdata_handler,              &
    endDTD_handler,                &
    endEntity_handler,             &
    startCdata_handler,            &
    startDTD_handler,              &
    startEntity_handler)

  call close_xml_t(fxml)

end program sax_example

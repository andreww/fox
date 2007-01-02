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

  call sax_parse_go(fxml, &
    startElement_handler = startElement_handler , &
    endElement_handler = endElement_handler, &
    characters_handler = characters_handler, &
    comment_handler = comment_handler, &
    processingInstruction_handler = processingInstruction_handler, &
    startPrefixMapping_handler = startPrefixMapping_handler, &
    endPrefixMapping_handler = endPrefixMapping_handler)

  call close_xml_t(fxml)

end program sax_example

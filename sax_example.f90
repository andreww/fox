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
               startElement_handler = begin_element_handler , &
               endElement_handler = end_element_handler, &
               characters_handler = pcdata_chunk_handler, &
               comment_handler = comment_handler, &
               processingInstruction_handler = processing_instruction_handler, &
               startPrefixMapping_handler = start_prefix_handler, &
               endPrefixMapping_handler = end_prefix_handler)

  call close_xml_t(fxml)

end program sax_example

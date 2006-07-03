program sax_example
  !
  ! Example driver for a stand-alone parsing of an xml document
  !
  use FoX_sax
  use pxf

  use m_handlers      ! Defines begin_element, end_element, pcdata_chunk, etc

  integer :: iostat
  type(xml_t)  :: fxml

  call open_xmlfile("test.xml",fxml,iostat)
  if (iostat /= 0) then
     write(*,*) "Cannot open file."
     call pxfabort()
  endif

  call xml_parse(fxml, &
               begin_element_handler = begin_element_handler , &
               end_element_handler = end_element_handler, &
               pcdata_chunk_handler = pcdata_chunk_handler, &
               comment_handler = comment_handler, &
               xml_declaration_handler = xml_declaration_handler, &
               start_prefix_handler = start_prefix_handler, &
               end_prefix_handler = end_prefix_handler, &
               verbose = .true.)

  call close_xmlfile(fxml)

end program sax_example

program test_sax_reader

  use m_sax_operate

  use m_handlers

  type(xml_t) :: xt
  integer :: i

  call open_xml_file(xt, "testin.xml", i)

  call sax_parse_go(xt, &
       start_document_handler=start_document_handler, &
       end_document_handler=end_document_handler, &
       begin_element_handler=begin_element_handler, &
       end_element_handler=end_element_handler, &
       start_prefix_handler=start_prefix_handler, &
       end_prefix_handler=end_prefix_handler, &
       characters_handler=characters_handler)

  call close_xml_t(xt)

end program test_sax_reader

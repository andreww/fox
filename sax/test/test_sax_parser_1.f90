program test_sax_reader

  use m_sax_parser
  use m_sax_reader
  use m_sax_types

  type(file_buffer_t) :: fb
  type(sax_parser_t) :: fx

  integer :: iostat

  call open_file(fb, file="test_sax_fsm_1.in", iostat=iostat)

  call sax_parser_init(fx)

  call sax_parse(fx, fb, iostat)

  print*, fx%xml_version
  print*, fx%encoding
  print*, fx%standalone

  call sax_parser_destroy(fx)
  call close_file(fb) 

end program test_sax_reader

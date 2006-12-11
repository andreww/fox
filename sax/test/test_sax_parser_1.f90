program test_sax_reader

  use m_sax_fsm
  use m_sax_reader

  type(file_buffer_t) :: fb
  type(fsm_t) :: fx

  integer :: iostat

  call open_file("test_sax_fsm_1.in", fb, iostat)

  call init_fsm(fx)

  call evolve_fsm(fx, fb, iostat)

  print*, fx%xml_version
  print*, fx%encoding
  print*, fx%standalone

  call destroy_fsm(fx)
  call close_file(fb) 

end program test_sax_reader

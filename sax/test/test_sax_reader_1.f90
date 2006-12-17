program test_sax_reader

  use m_sax_reader

  type(file_buffer_t) :: fb

  integer :: iostat

  call open_file(fb, file="test_sax_reader_1.in",  iostat=iostat)

  write(*,'(2a)') 'char:', read_char(fb, iostat)
  write(*,'(a,i0)') 'iost:', iostat

  call close_file(fb) 

end program test_sax_reader

program test_sax_reader

  use m_sax_reader

  character(15) :: buf
  type(file_buffer_t) :: fb

  integer :: iostat

  call open_file("test_sax_reader_10.in", fb, iostat)

  buf = get_characters(fb, 15, iostat) 

  write(*,'(3a)') 'char:', buf, ':'
  write(*,'(a,i0)') 'iost:', iostat

  call put_characters(fb, "putback")

  buf = get_characters(fb, 15, iostat) 

  write(*,'(3a)') 'char:', buf, ':'
  write(*,'(a,i0)') 'iost:', iostat

  call close_file(fb) 

end program test_sax_reader

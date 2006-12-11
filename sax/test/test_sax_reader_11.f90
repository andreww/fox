program test_sax_reader

  use m_sax_reader

  character(21) :: buf
  character :: c
  type(file_buffer_t) :: fb

  integer :: iostat

  call open_file("test_sax_reader_11a.in", fb, iostat)

  buf = get_characters(fb, 21, iostat) 

  c = get_next_character_discarding_whitespace(fb, iostat) 

  write(*,'(3a)') 'char:', c, ':'
  write(*,'(a,i0)') 'iost:', iostat

  call close_file(fb) 
  call open_file("test_sax_reader_11b.in", fb, iostat) 

  buf = get_characters(fb, 21, iostat) 

  c = get_next_character_discarding_whitespace(fb, iostat) 

  write(*,'(3a)') 'char:', c, ':'
  write(*,'(a,i0)') 'iost:', iostat

  call put_characters(fb, "     ")

  c = get_next_character_discarding_whitespace(fb, iostat) 

  write(*,'(3a)') 'char:', c, ':'
  write(*,'(a,i0)') 'iost:', iostat

  call put_characters(fb, "     1")

  c = get_next_character_discarding_whitespace(fb, iostat) 

  write(*,'(3a)') 'char:', c, ':'
  write(*,'(a,i0)') 'iost:', iostat

  call close_file(fb) 

end program test_sax_reader

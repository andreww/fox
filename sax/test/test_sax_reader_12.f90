program test_sax_reader

  use m_sax_reader

  character(40) :: buf
  character :: c
  type(file_buffer_t) :: fb

  integer :: iostat

  ! test 1: move 40 chars in, and then pick up the name

  call open_file("test_sax_reader_12a.in", fb, iostat)

  buf = get_characters(fb, 40, iostat) 

  call get_characters_until_not_namechar(fb, iostat) 

  write(*,'(3a)') 'char:', retrieve_namebuffer(fb), ':'
  write(*,'(a,i0)') 'iost:', iostat

  call close_file(fb) 

  ! test 2: move 40 chars in, and then pick up the name, which is now 2004 chars long.

  call open_file("test_sax_reader_12b.in", fb, iostat) 

  buf = get_characters(fb, 40, iostat) 

  call get_characters_until_not_namechar(fb, iostat) 

  print*, len_namebuffer(fb)

  write(*,'(3a)') 'char:', retrieve_namebuffer(fb), ':'
  write(*,'(a,i0)') 'iost:', iostat

  ! test 3: put some chars on the pushback stack, and read them.

  call put_characters(fb, "abcde ")

  call get_characters_until_not_namechar(fb, iostat) 

  write(*,'(3a)') 'char:', retrieve_namebuffer(fb), ':'
  write(*,'(a,i0)') 'iost:', iostat

  buf = get_characters(fb, 1, iostat) ! clear out trailing space.

  call rewind_file(fb)

  buf = get_characters(fb, 100, iostat) !read till we're in the middle of the long name

  ! test 3: put some chars on the pushback stack, and read them, but now we need to look at the buffer as well.

  call put_characters(fb, "abcde")

  call get_characters_until_not_namechar(fb, iostat) 

  write(*,'(3a)') 'char:', retrieve_namebuffer(fb), ':'
  write(*,'(a,i0)') 'iost:', iostat

  call close_file(fb) 

end program test_sax_reader

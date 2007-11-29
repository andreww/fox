program short_test

  use m_common_parse_input, only : stringtodata

  character(len=10) :: array(5), matrix(2,3)
  integer :: i, num, iostat

  print*, "reading string array without separators"
  call stringtodata("a b c d e", array)
  do i = 1, 5
    print*, array(i)
  enddo
  print*, "ok"

  print*, "reading short string array without separators"
  call stringtodata("a b c d", array, num=num, iostat=iostat)
  if (iostat/=-1) & 
    print*, "Failed to generate error"
  if (num/=4) &
    print*, "Failed to count correctly"
  do i = 1, 5
    print*, array(i)
  enddo

  print*, "reading long string array without separators"
  call stringtodata("a b c d e f", array, num=num, iostat=iostat)
  if (iostat/=1) & 
    print*, "Failed to generate error"
  if (num/=5) &
    print*, "Failed to count correctly"
  do i = 1, 5
    print*, array(i)
  enddo

  print*, "reading string array with separators"
  call stringtodata("a, b, c, d, e", array, separator=",")
  do i = 1, 5
    print*, array(i)
  enddo
  print*, "ok"

  print*, "reading short string array with separators"
  call stringtodata("a, b, c, d", array, separator=",", num=num, iostat=iostat)
  if (iostat/=-1) & 
    print*, "Failed to generate error"
  if (num/=4) &
    print*, "Failed to count correctly"
  do i = 1, 5
    print*, array(i)
  enddo

  print*, "reading long string array with separators"
  call stringtodata("a, b, c, d, e, f", array, separator=",", num=num, iostat=iostat)
  if (iostat/=1) & 
    print*, "Failed to generate error"
  if (num/=5) &
    print*, "Failed to count correctly"
  do i = 1, 5
    print*, array(i)
  enddo

  print*, "reading string matrix without separators"
  call stringtodata("a b c d e f", matrix)
  do i = 1, 2
    do j = 1, 3
      print*, matrix(i, j)
    enddo
  enddo
  print*, "ok"

  print*, "reading short string matrix without separators"
  call stringtodata("a b c d", matrix, num=num, iostat=iostat)
  if (iostat/=-1) & 
    print*, "Failed to generate error"
  if (num/=4) &
    print*, "Failed to count correctly"
  do i = 1, 2
    do j = 1, 3
      print*, matrix(i, j)
    enddo
  enddo
  print*, "ok"

  print*, "reading long string matrix without separators"
  call stringtodata("a b c d e f g", matrix, num=num, iostat=iostat)
  if (iostat/=1) & 
    print*, "Failed to generate error"
  if (num/=6) &
    print*, "Failed to count correctly"
  do i = 1, 2
    do j = 1, 3
      print*, matrix(i, j)
    enddo
  enddo
  print*, "ok"

  print*, "reading string matrix with separators"
  call stringtodata("a, b, c, d, e, f", matrix, separator=",")
  do i = 1, 2
    do j = 1, 3
      print*, matrix(i, j)
    enddo
  enddo
  print*, "ok"

  print*, "reading short string matrix with separators"
  call stringtodata("a, b, c, d", matrix, separator=",", num=num, iostat=iostat)
  if (iostat/=-1) & 
    print*, "Failed to generate error"
  if (num/=4) &
    print*, "Failed to count correctly"
  do i = 1, 2
    do j = 1, 3
      print*, matrix(i, j)
    enddo
  enddo
  print*, "ok"

  print*, "reading long string matrix with separators"
  call stringtodata("a, b, c, d, e, f, ", matrix, separator=",", num=num, iostat=iostat)
  if (iostat/=1) & 
    print*, "Failed to generate error"
  if (num/=6) &
    print*, "Failed to count correctly"
  do i = 1, 2
    do j = 1, 3
      print*, matrix(i, j)
    enddo
  enddo
  print*, "ok"

end program short_test

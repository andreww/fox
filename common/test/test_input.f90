program short_test

  use m_common_parse_input, only : stringtodata

  
  print*,"test.1.1.1"
  call stringdataarray("a b c d e", (/"a", "b", "c", "d", "e"/), 5, 0)
  print*,"test.1.1.2"
  call stringdataarray("a b c d", (/"a", "b", "c", "d", " "/), 4, -1)
  print*,"test.1.1.3"
  call stringdataarray("a b c d e f", (/"a", "b", "c", "d", "e"/), 5, 1)

  print*,"test.1.2.1"
  call stringdataarray("a, b, c, d, e", (/"a ", " b", " c", " d", " e"/), &
    5, 0, ",")
  print*,"test.1.2.2"
  call stringdataarray("a, b, c, d", (/"a ", " b", " c", " d", "  "/), &
    4, -1, ",")
  print*,"test.1.2.3"
  call stringdataarray("a, b, c, d, e, f", (/"a ", " b", " c", " d", " e"/), &
    5, 1, ",")
  print*,"test.1.2.4"
  call stringdataarray("a, b, c, d, e, ", (/"a ", " b", " c", " d", " e"/), &
    5, 1, ",")
  print*,"test.1.2.5"
  call stringdataarray("a, b, c, d, e,", (/"a ", " b", " c", " d", " e"/), &
    5, 0, ",")

  print*,"test.1.3.1"
  call stringdatamatrix("a b c d e f", &
    reshape((/"a", "b", "c", "d", "e", "f"/), (/2,3/)), &
    6, 0)
  print*,"test.1.3.2"
  call stringdatamatrix("a b c d", &
    reshape((/"a", "b", "c", "d", " ", " "/), (/2,3/)), &
    4, -1)
  print*,"test.1.3.3"
  call stringdatamatrix("a b c d e f g", &
    reshape((/"a", "b", "c", "d", "e", "f"/), (/2,3/)), &
    6, 1)

  print*,"test.1.4.1"
  call stringdatamatrix("a, b, c, d, e, f", &
    reshape((/"a ", " b", " c", " d", " e", " f"/), (/2,3/)), &
    6, 0, ",")
  print*,"test.1.4.2"
  call stringdatamatrix("a, b, c, d", &
    reshape((/"a ", " b", " c", " d", "  ", "  "/), (/2,3/)), &
    4, -1, ",")
  print*,"test.1.4.3"
  call stringdatamatrix("a, b, c, d, e, f, g", &
    reshape((/"a ", " b", " c", " d", " e", " f"/), (/2,3/)), &
    6, 1, ",")
  print*,"test.1.4.4"
  call stringdatamatrix("a, b, c, d, e, f, ", &
    reshape((/"a ", " b", " c", " d", " e", " f"/), (/2,3/)), &
    6, 1, ",")
  print*,"test.1.4.5"
  call stringdatamatrix("a, b, c, d, e, f,", &
    reshape((/"a ", " b", " c", " d", " e", " f"/), (/2,3/)), &
    6, 0, ",")

  print*, "test.2.1.1"
  call logicaldataarray("true false false false true", &
    (/.true., .false., .false., .false., .true./), 5, 0)
  print*, "test.2.1.2"
  call logicaldataarray("true false false false", &
    (/.true., .false., .false., .false., .false./), 4, -1)
  print*, "test.2.1.3"
  call logicaldataarray("true false false false true true", &
    (/.true., .false., .false., .false., .true./), 5, 1)

  print*, "test.2.2.1"
  call logicaldatamatrix("true false false false true true", &
    reshape((/.true., .false., .false., .false., .true., .true./), (/2,3/)), &
    6, 0)
  print*, "test.2.2.2"
  call logicaldatamatrix("true false false false", &
    reshape((/.true., .false., .false., .false., .false., .false./), (/2,3/)), &
    4, -1)
  print*, "test.2.2.3"
  call logicaldatamatrix("true false false false true true false", &
    reshape((/.true., .false., .false., .false., .true., .true./), (/2,3/)), &
    6, 1)

  print*, "test.3.1.1"
  call integerdataarray("1 2 3 4 5", &
    (/1, 2, 3, 4, 5/), 5, 0)
  print*, "test.3.1.2"
  call integerdataarray("1 2 3 4", &
    (/1, 2, 3, 4, 0/), 4, -1)
  print*, "test.3.1.3"
  call integerdataarray("1 2 3 4 5 6", &
    (/1, 2, 3, 4, 5/), 5, 1)

! I actually cannot be bothered thinking up test cases
! for real numbers. So shoot me.

  contains

    subroutine stringdataarray(string, array, num, iostat, sep)
      character(len=*), intent(in) :: string
      character(len=*), dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat
      character, intent(in), optional :: sep

      character(len=len(array)) :: temp(size(array))
      integer :: n, i

      call stringtodata(string, temp, separator=sep, num=n, iostat=i)

      if (any(temp/=array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine stringdataarray

    subroutine stringdatamatrix(string, array, num, iostat, sep)
      character(len=*), intent(in) :: string
      character(len=*), dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat
      character, intent(in), optional :: sep

      character(len=len(array)) :: temp(size(array,1),size(array,2))
      integer :: n, i

      call stringtodata(string, temp, separator=sep, num=n, iostat=i)

      if (any(temp/=array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine stringdatamatrix

    subroutine logicaldataarray(string, array, num, iostat)
      character(len=*), intent(in) :: string
      logical, dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat

      logical :: temp(size(array))
      integer :: n, i

      call stringtodata(string, temp, n, i)

      if (any(temp.neqv.array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine logicaldataarray

    subroutine logicaldatamatrix(string, array, num, iostat)
      character(len=*), intent(in) :: string
      logical, dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat

      logical :: temp(size(array,1),size(array,2))
      integer :: n, i

      call stringtodata(string, temp, n, i)

      if (any(temp.neqv.array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine logicaldatamatrix

    subroutine integerdataarray(string, array, num, iostat)
      character(len=*), intent(in) :: string
      integer, dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat

      integer :: temp(size(array))
      integer :: n, i

      call stringtodata(string, temp, n, i)

      if (any(temp/=array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine integerdataarray

    subroutine integerdatamatrix(string, array, num, iostat)
      character(len=*), intent(in) :: string
      integer, dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat

      integer :: temp(size(array,1),size(array,2))
      integer :: n, i

      call stringtodata(string, temp, n, i)

      if (any(temp/=array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine integerdatamatrix

    subroutine realspdataarray(string, array, num, iostat)
      character(len=*), intent(in) :: string
      real, dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat

      real :: temp(size(array))
      integer :: n, i

      call stringtodata(string, temp, n, i)

      if (any(temp/=array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine realspdataarray

    subroutine realspdatamatrix(string, array, num, iostat)
      character(len=*), intent(in) :: string
      real, dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat

      real :: temp(size(array,1),size(array,2))
      integer :: n, i

      call stringtodata(string, temp, n, i)

      if (any(temp/=array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine realspdatamatrix

end program short_test

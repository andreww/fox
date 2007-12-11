program short_test

  use m_common_parse_input, only : rts
  use m_common_realtypes, only: sp, dp

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

  print*,"test.1.2.6"
  call stringdataarray("a, b, c, d, e", (/"a ", " b", " c", " d", " e"/), &
    5, 0, csv=.true.)
  print*,"test.1.2.7"
  call stringdataarray("a, b, c, d", (/"a ", " b", " c", " d", "  "/), &
    4, -1, csv=.true.)
  print*,"test.1.2.8"
  call stringdataarray("a, b, c, d, e, f", (/"a ", " b", " c", " d", " e"/), &
    5, 1, csv=.true.)
  print*,"test.1.2.9" 
  call stringdataarray("a, b, c, d, e, ", (/"a ", " b", " c", " d", " e"/), &
    5, 1, csv=.true.)
  print*,"test.1.2.10"
  call stringdataarray("a, b, c, d, e,", (/"a ", " b", " c", " d", " e"/), &
    5, 1, csv=.true.)
  print*,"test.1.2.11"
  call stringdataarray("a, b, c,, e", (/"a ", " b", " c", "  ", " e"/), & 
    5, 0, csv=.true.)
  print*,"test.1.2.12"
  call stringdataarray("a, b, c, d,", (/"a ", " b", " c", " d", "  "/), &  
    5, 0, csv=.true.)
  print*,"test.1.2.13"
  call stringdataarray(", b, c, d, e", (/"  ", " b", " c", " d", " e"/), & 
    5, 0, csv=.true.)

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
  print*, "test.2.1.4"
  call logicaldataarray("true, false, false, false true", &
    (/.true., .false., .false., .false., .true./), 5, 0)
  print*, "test.2.1.5"
  call logicaldataarray("true, false, false, ,false true", &
    (/.true., .false., .false., .false., .false./), 3, 2)

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

  print*, "test.3.1.4"
  call integerdataarray("1, 2, 3, 4, 5", & 
    (/1, 2, 3, 4, 5/), 5, 0)
  print*, "test.3.1.5"
  call integerdataarray("1, 2, 3, 4,", & 
    (/1, 2, 3, 4, 0/), 4, -1)
  print*, "test.3.1.6"
  call integerdataarray("1, 2, 3, 4, 5, 6,", & 
    (/1, 2, 3, 4, 5/), 5, 1)

  print*, "test.4.1.1"
  call realspdataarray("1.0 -2.e5 +3.44e-3 4. 5.09090909", & 
    (/1.0e0, -2.e5, 3.44e-3, 4.e0, 5.09090909e0/), 5, 0)
  print*, "test.4.1.2"
  call realspdataarray("1.0 -2.e5 +3.44e-3 4.", & 
    (/1.0e0, -2.e5, 3.44e-3, 4.e0, 0.0e0/), 4, -1)
  print*, "test.4.1.3"
  call realspdataarray("1.0 -2.e5 +3.44e-3 4. 5.09090909 -0.3", & 
    (/1.0e0, -2.e5, 3.44e-3, 4.e0, 5.09090909e0/), 5, 1)

  print*, "test.4.1.4"
  call realspdataarray("1.0, -2.e5 +3.44e-3, 4. 5.09090909", & 
    (/1.0e0, -2.e5, 3.44e-3, 4.e0, 5.09090909e0/), 5, 0)
  print*, "test.4.1.5"
  call realspdataarray("1.0, -2.e5 +3.44e-3, 4.", &  
    (/1.0e0, -2.e5, 3.44e-3, 4.e0, 0.0e0/), 4, -1)
  print*, "test.4.1.6"
  call realspdataarray("1.0, -2.e5 +3.44e-3, 4. 5.09090909, -0.3", & 
    (/1.0e0, -2.e5, 3.44e-3, 4.e0, 5.09090909e0/), 5, 1)

  print*, "test.5.1.1"
  call realdpdataarray("1.0 -2.e5 +3.44e-3 4. 5.09090909", & 
    (/1.0d0, -2.d5, 3.44d-3, 4.d0, 5.09090909d0/), 5, 0)
  print*, "test.5.1.2"
  call realdpdataarray("1.0 -2.e5 +3.44e-3 4.", &  
    (/1.0d0, -2.d5, 3.44d-3, 4.d0, 0.0d0/), 4, -1)
  print*, "test.5.1.3"
  call realdpdataarray("1.0 -2.e5 +3.44e-3 4. 5.09090909 -0.3", & 
    (/1.0d0, -2.d5, 3.44d-3, 4.d0, 5.09090909d0/), 5, 1)

  print*, "test.5.1.4"
  call realdpdataarray("1.0, -2.e5 +3.44e-3, 4. 5.09090909", & 
    (/1.0d0, -2.d5, 3.44d-3, 4.d0, 5.09090909d0/), 5, 0)
  print*, "test.5.1.5"
  call realdpdataarray("1.0, -2.e5 +3.44e-3, 4.", &  
    (/1.0d0, -2.d5, 3.44d-3, 4.d0, 0.0d0/), 4, -1)
  print*, "test.5.1.6"
  call realdpdataarray("1.0, -2.e5 +3.44e-3, 4. 5.09090909, -0.3", & 
    (/1.0d0, -2.d5, 3.44d-3, 4.d0, 5.09090909d0/), 5, 1)

  print*, "test.6.1.1"
  call cmplxspdataarray("(1.0)+i(-2.e5) (+3.44e-3)+i(4.) (5.09090909)+i(1.0) (-2e5)+i(+3.44e-3) (4.)+i(5.09090909)", & 
    (/(1.0e0,-2.e5), (3.44e-3,4.e0), (5.09090909e0,1.0e0), (-2e5,3.44e-3), (4.,5.09090909)/), 5, 0)
  print*, "test.6.1.2"
  call cmplxspdataarray("(1.0)+i(-2.e5) (+3.44e-3)+i(4.) (5.09090909)+i(1.0) (-2e5)+i(+3.44e-3)", & 
    (/(1.0e0,-2.e5), (3.44e-3,4.e0), (5.09090909e0,1.0e0), (-2e5,3.44e-3), (0.,0.)/), 4, -1)
  print*, "test.6.1.3"
  call cmplxspdataarray("(1.0)+i(-2.e5) (+3.44e-3)+i(4.) (5.09090909)+i(1.0) (-2e5)+i(+3.44e-3) (4.)+i(5.09090909) (1)+i(1)", & 
    (/(1.0e0,-2.e5), (3.44e-3,4.e0), (5.09090909e0,1.0e0), (-2e5,3.44e-3), (4.,5.09090909)/), 5, 1)

  print*, "test.6.1.4"
  call cmplxspdataarray("1.0,-2.e5 +3.44e-3,4. 5.09090909,1.0 -2e5,+3.44e-3 4.,5.09090909", & 
    (/(1.0e0,-2.e5), (3.44e-3,4.e0), (5.09090909e0,1.0e0), (-2e5,3.44e-3), (4.,5.09090909)/), 5, 0)
  print*, "test.6.1.5"
  call cmplxspdataarray("1.0,-2.e5 +3.44e-3,4. 5.09090909,1.0 -2e5,+3.44e-3", & 
    (/(1.0e0,-2.e5), (3.44e-3,4.e0), (5.09090909e0,1.0e0), (-2e5,3.44e-3), (0.,0.)/), 4, -1)
  print*, "test.6.1.6"
  call cmplxspdataarray("1.0,-2.e5 +3.44e-3,4. 5.09090909,1.0 -2e5,+3.44e-3 4.,5.09090909 1,1", & 
    (/(1.0e0,-2.e5), (3.44e-3,4.e0), (5.09090909e0,1.0e0), (-2e5,3.44e-3), (4.,5.09090909)/), 5, 1)

contains

    subroutine stringdataarray(string, array, num, iostat, sep, csv)
      character(len=*), intent(in) :: string
      character(len=*), dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat
      character, intent(in), optional :: sep
      logical, intent(in), optional :: csv

      character(len=len(array)) :: temp(size(array))
      integer :: n, i

      call rts(string, temp, separator=sep, csv=csv, num=n, iostat=i)

      if (any(temp/=array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"

    end subroutine stringdataarray

    subroutine stringdatamatrix(string, array, num, iostat, sep, csv)
      character(len=*), intent(in) :: string
      character(len=*), dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat
      character, intent(in), optional :: sep
      logical, intent(in), optional :: csv

      character(len=len(array)) :: temp(size(array,1),size(array,2))
      integer :: n, i

      call rts(string, temp, separator=sep, csv=csv, num=n, iostat=i)

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

      call rts(string, temp, n, i)

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

      call rts(string, temp, n, i)

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

      call rts(string, temp, n, i)

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

      call rts(string, temp, n, i)

      if (any(temp/=array)) &
        print*, "Different array"
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine integerdatamatrix

    subroutine realspdataarray(string, array, num, iostat)
      character(len=*), intent(in) :: string
      real(sp), dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat

      real(sp) :: temp(size(array))
      integer :: n, i, j

      call rts(string, temp, n, i)

      do j = 1, size(array)
        if (array(j)==0.0) then
          if (temp(j)>1e-5) then
            print*, "Different array"
            exit
          endif
        elseif (abs(temp(j)-array(j))/temp(j)>1e-5) then
          print*, "Different array"
          exit
        endif
      enddo 
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine realspdataarray

    subroutine realspdatamatrix(string, array, num, iostat)
      character(len=*), intent(in) :: string
      real(sp), dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat

      real(sp) :: temp(size(array,1),size(array,2))
      integer :: n, i, j, k

      call rts(string, temp, n, i)

      loop: do j = 1, size(array,1)
        do k = 1, size(array,2)
          if (array(j,k)==0.0) then
            if (temp(j,k)>1e-5) then
              exit loop
            endif
           elseif (abs(temp(j,k)-array(j,k))/temp(j,k)>1e-5) then
            print*, "Different array"
            exit loop
          endif
        enddo
      enddo loop
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine realspdatamatrix

    subroutine realdpdataarray(string, array, num, iostat)
      character(len=*), intent(in) :: string
      real(dp), dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat

      real(dp) :: temp(size(array))
      integer :: n, i, j

      call rts(string, temp, n, i)

      do j = 1, size(array)
        if (array(j)==0.0) then
          if (temp(j)>1e-5) then
            print*, "Different array"
            exit
          endif
        elseif (abs(temp(j)-array(j))/temp(j)>1e-5) then
          print*, "Different array"
          exit
        endif
      enddo 
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine realdpdataarray

    subroutine realdpdatamatrix(string, array, num, iostat)
      character(len=*), intent(in) :: string
      real(dp), dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat

      real(dp) :: temp(size(array,1),size(array,2))
      integer :: n, i, j, k

      call rts(string, temp, n, i)

      loop: do j = 1, size(array,1)
        do k = 1, size(array,2)
          if (array(j,k)==0.0) then
            if (temp(j,k)>1e-5) then
              print*, "Different array"
              exit loop
            endif
           elseif (abs(temp(j,k)-array(j,k))/temp(j,k)>1e-5) then
            print*, "Different array"
            exit loop
          endif
        enddo
      enddo loop
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine realdpdatamatrix

    subroutine cmplxspdataarray(string, array, num, iostat)
      character(len=*), intent(in) :: string
      complex(sp), dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat

      complex(sp) :: temp(size(array))
      real(sp) :: a(2), t(2)
      integer :: n, i, j, m

      call rts(string, temp, n, i)

      do j = 1, size(array)
        a = transfer(array(j), a)
        t = transfer(array(j), t)
        do m = 1, 2
          if (a(m)==0.0) then
            if (t(m)>1e-5) then
              print*, "Different array"
              exit
            endif
          elseif (abs(t(m)-a(m))/t(m)>1e-5) then
            print*, "Different array"
            exit
          endif
        enddo
      enddo 
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine cmplxspdataarray

    subroutine cmplxspdatamatrix(string, array, num, iostat)
      character(len=*), intent(in) :: string
      complex(sp), dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat

      complex(sp) :: temp(size(array,1),size(array,2))
      real(sp) :: a(2), t(2)
      integer :: n, i, j, k, m

      call rts(string, temp, n, i)

      loop: do j = 1, size(array,1)
        do k = 1, size(array,2)
          a = transfer(array(j,k), a)
          t = transfer(array(j,k), t)
          do m = 1, 2
            if (a(m)==0.0) then
              if (t(m)>1e-5) then
                print*, "Different array"
                exit loop
              endif
            elseif (abs(t(m)-a(m))/t(m)>1e-5) then
              print*, "Different array"
              exit loop
            endif
          enddo
        enddo
      enddo loop
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine cmplxspdatamatrix

    subroutine cmplxdpdataarray(string, array, num, iostat)
      character(len=*), intent(in) :: string
      complex(dp), dimension(:), intent(in) :: array
      integer, intent(in) :: num, iostat

      complex(dp) :: temp(size(array))
      real(dp) :: a(2), t(2)
      integer :: n, i, j, m

      call rts(string, temp, n, i)

      do j = 1, size(array)
        a = transfer(array(j), a)
        t = transfer(array(j), t)
        do m = 1, 2
          if (a(m)==0.0) then
            if (t(m)>1e-5) then
              print*, "Different array"
              exit
            endif
          elseif (abs(t(m)-a(m))/t(m)>1e-5) then
            print*, "Different array"
            exit
          endif
        enddo
      enddo 
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine cmplxdpdataarray

    subroutine cmplxdpdatamatrix(string, array, num, iostat)
      character(len=*), intent(in) :: string
      complex(dp), dimension(:, :), intent(in) :: array
      integer, intent(in) :: num, iostat

      complex(dp) :: temp(size(array,1),size(array,2))
      real(dp) :: a(2), t(2)
      integer :: n, i, j, k, m

      call rts(string, temp, n, i)

      loop: do j = 1, size(array,1)
        do k = 1, size(array,2)
          a = transfer(array(j,k), a)
          t = transfer(array(j,k), t)
          do m = 1, 2
            if (a(m)==0.0) then
              if (t(m)>1e-5) then
                print*, "Different array"
                exit loop
              endif
            elseif (abs(t(m)-a(m))/t(m)>1e-5) then
              print*, "Different array"
              exit loop
            endif
          enddo
        enddo
      enddo loop
      if (i/=iostat) &
        print*, "Wrong iostat"
      if (n/=num) &
        print*, "Wrong num"
    end subroutine cmplxdpdatamatrix

end program short_test

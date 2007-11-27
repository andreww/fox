module readarrays

  use m_common_charset, only : XML_WHITESPACE
  use m_common_realtypes, only: sp, dp

  implicit none
  private

  interface stringtodata
    module procedure arraytostring
    module procedure arraytological
    module procedure arraytointeger
    module procedure arraytorealsp
    module procedure arraytorealdp
    module procedure arraytocomplexsp
    module procedure arraytocomplexdp
    module procedure matrixtostring
    module procedure matrixtological
    module procedure matrixtointeger
    module procedure matrixtorealsp
    module procedure matrixtorealdp
    module procedure matrixtocomplexsp
    module procedure matrixtocomplexdp
  end interface

  public stringtodata

contains

  subroutine arraytostring(s, array, separator, num, iostat)
    character(len=*) :: array(:)
    character, intent(in), optional :: separator
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    n = size(array)
    err = 0
    array = ""
    ij = 0
    do i = 1, n
      if (present(separator)) then
        k = index(s(s_i:), separator)
      else
        k = verify(s(s_i:), XML_WHITESPACE)
        if (k==0) exit
        s_i = s_i + k - 1
        k = scan(s(s_i:), XML_WHITESPACE)
      endif
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      array(i) = s(s_i:k)
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do

    if (present(num)) num = ij
    if (i<=size(array)) then
      err = -1
    else
      if (present(separator)) then
        if (index(s(s_i:), separator)/=0) &
          err = 1
      else
        if (verify(s(s_i:), XML_WHITESPACE)/=0) &
          err = 1
      endif
    endif

    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in arraytostring"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in arraytostring"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in arraytostring"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine arraytostring

  subroutine matrixtostring(s, array, separator, num, iostat)
    character(len=*) :: array(:,:)
    character, intent(in), optional :: separator
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    m = size(array, 1)
    n = size(array, 2)
    err = 0
    array = ""
    ij = 0
    do i = 1, n
    do j = 1, m
      if (present(separator)) then
        k = index(s(s_i:), separator)
      else
        k = verify(s(s_i:), XML_WHITESPACE)
        if (k==0) exit
        s_i = s_i + k - 1
        k = scan(s(s_i:), XML_WHITESPACE)
      endif
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      array(m, n) = s(s_i:k)
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit
    end do
    end do

    if (ij<=size(array)) then
      if (present(num)) num = ij
      err = -1
    else
      if (present(num)) num = n
      if (present(separator)) then
        if (index(s(s_i:), separator)/=0) &
          err = 1
      else
        if (verify(s(s_i:), XML_WHITESPACE)/=0) &
          err = 1
      endif
    endif

    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in matrixtostring"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in matrixtostring"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in matrixtostring"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine matrixtostring

  subroutine arraytological(s, array, num, iostat)
    logical, intent(out) :: array(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    n = size(array)
    err = 0
    array = .false.
    ij  = 0
    do i = 1, n
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      if (s(s_i:k)=="true".or.s(s_i:k)=="1") then
        array(i) = .true.
      elseif (s(s_i:k)=="false".or.s(s_i:k)=="0") then
        array(i) = .false.
      else
        err = 2
        exit
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in arraytological"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in arraytological"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in arraytological"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine arraytological

  subroutine matrixtological(s, array, num, iostat)
    logical, intent(out) :: array(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    m = size(array, 1)
    n = size(array, 2)
    err = 0
    array = .false.
    ij = 0
    do i = 1, n
    do j = 1, m
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      if (s(s_i:k)=="true".or.s(s_i:k)=="1") then
        array(m, n) = .true.
      elseif (s(s_i:k)=="false".or.s(s_i:k)=="0") then
        array(m, n) = .false.
      else
        err = 2
        exit
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do
    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in matrixtological"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in matrixtological"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in matrixtological"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine matrixtological

  subroutine arraytointeger(s, array, num, iostat)
    integer, intent(out) :: array(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c

    s_i = 1
    n = size(array)
    err = 0
    array = 0
    ij  = 0
    do i = 1, n
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) array(i)
      if (ios/=0) then
        err = 2
        exit
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in arraytointeger"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in arraytointeger"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in arraytointeger"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine arraytointeger

  subroutine matrixtointeger(s, array, num, iostat)
    integer, intent(out) :: array(:, :)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    m = size(array, 1)
    n = size(array, 2)
    err = 0
    array = 0
    ij = 0
    do i = 1, n
    do j = 1, m
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) array(m, n)
      if (ios/=0) then
        err = 2
        exit
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do
    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in matrixtointeger"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in matrixtointeger"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in matrixtointeger"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine matrixtointeger

  subroutine arraytorealsp(s, array, num, iostat)
    real(sp), intent(out) :: array(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    n = size(array)
    err = 0
    array = 0
    ij  = 0
    do i = 1, n
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) array(i)
      if (ios/=0) then
        err = 2
        exit
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in arraytorealsp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in arraytorealsp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in arraytorealsp"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine arraytorealsp

  subroutine matrixtorealsp(s, array, num, iostat)
    real(sp), intent(out) :: array(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    m = size(array, 1)
    n = size(array, 2)
    err = 0
    array = 0
    ij = 0
    do i = 1, n
    do j = 1, m
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) array(m, n)
      if (ios/=0) then
        err = 2
        exit
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do
    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in matrixtorealsp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in matrixtorealsp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in matrixtorealsp"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine matrixtorealsp

  subroutine arraytorealdp(s, array, num, iostat)
    real(dp), intent(out) :: array(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    n = size(array)
    err = 0
    array = 0
    ij  = 0
    do i = 1, n
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) array(i)
      if (ios/=0) then
        err = 2
        exit
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in arraytorealdp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in arraytorealdp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in arraytorealdp"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine arraytorealdp

  subroutine matrixtorealdp(s, array, num, iostat)
    real(dp), intent(out) :: array(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    m = size(array, 1)
    n = size(array, 2)
    err = 0
    array = cmplx(0,0)
    ij = 0
    do i = 1, n
    do j = 1, m
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) array(m, n)
      if (ios/=0) then
        err = 2
        exit
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<size(array).and.s_i>len(s)) exit

    end do
    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in matrixtorealdp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in matrixtorealdp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in matrixtorealdp"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine matrixtorealdp

  subroutine arraytocomplexsp(s, array, num, iostat)
    complex(sp), intent(out) :: array(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    n = size(array)
    err = 0
    array = cmplx(0,0)
    ij  = 0
    do i = 1, n
      k = index(s(s_i:), "(")
      if (k==0) exit
      s_i = s_i + k
      k = index(s(s_i:), ")+i(")
      if (k==0) then
        err = 2
        exit
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) r
      if (ios/=0) then
        err = 2
        exit
      endif
      s_i = k + 5
      k = index(s(s_i:), ")")
      if (k==0) then
        err = 2
        exit
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) c
      if (ios/=0) then
        err = 2
        exit
      endif
      array(i) = cmplx(r, c)
      ij = ij + 1
      s_i = k + 2
      if (i<n.and.s_i>len(s)) exit

    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in arraytocomplexsp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in arraytocomplexsp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in arraytocomplexsp"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine arraytocomplexsp

  subroutine matrixtocomplexsp(s, array, num, iostat)
    complex(sp), intent(out) :: array(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    m = size(array, 1)
    n = size(array, 2)
    err = 0
    array = cmplx(0,0)
    ij = 0
    do i = 1, n
    do j = 1, m
      k = index(s(s_i:), "(")
      if (k==0) exit
      s_i = s_i + k
      k = index(s(s_i:), ")+i(")
      if (k==0) then
        err = 2
        exit
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) r
      if (ios/=0) then
        err = 2
        exit
      endif
      s_i = k + 5
      k = index(s(s_i:), ")")
      if (k==0) then
        err = 2
        exit
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) c
      if (ios/=0) then
        err = 2
        exit
      endif
      array(m, n) = cmplx(r, c)
      ij = ij + 1
      s_i = k + 2
      if (i<n.and.s_i>len(s)) exit

    end do
    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in matrixtocomplexsp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in matrixtocomplexsp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in matrixtocomplexsp"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine matrixtocomplexsp

  subroutine arraytocomplexdp(s, array, num, iostat)
    complex(dp), intent(out) :: array(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    n = size(array)
    err = 0
    array = cmplx(0)
    ij  = 0
    do i = 1, n
      k = index(s(s_i:), "(")
      if (k==0) exit
      s_i = s_i + k
      k = index(s(s_i:), ")+i(")
      if (k==0) then
        err = 2
        exit
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) r
      if (ios/=0) then
        err = 2
        exit
      endif
      s_i = k + 5
      k = index(s(s_i:), ")")
      if (k==0) then
        err = 2
        exit
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) c
      if (ios/=0) then
        err = 2
        exit
      endif
      array(i) = cmplx(r, c)
      ij = ij + 1
      s_i = k + 2
      if (i<n.and.s_i>len(s)) exit

    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in arraytocomplexdp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in arraytocomplexdp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in arraytocomplexdp"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine arraytocomplexdp

  subroutine matrixtocomplexdp(s, array, num, iostat)
    complex(dp), intent(out) :: array(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, m, n, s_i, err, ios
    real :: r, c


    s_i = 1
    m = size(array, 1)
    n = size(array, 2)
    err = 0
    array = cmplx(0,0)
    ij = 0
    do i = 1, n
    do j = 1, m
      k = index(s(s_i:), "(")
      if (k==0) exit
      s_i = s_i + k
      k = index(s(s_i:), ")+i(")
      if (k==0) then
        err = 2
        exit
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) r
      if (ios/=0) then
        err = 2
        exit
      endif
      s_i = k + 5
      k = index(s(s_i:), ")")
      if (k==0) then
        err = 2
        exit
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) c
      if (ios/=0) then
        err = 2
        exit
      endif
      array(m, n) = cmplx(r, c)
      ij = ij + 1
      s_i = k + 2
      if (i<n.and.s_i>len(s)) exit

    end do
    end do

    num = ij
    if (ij<size(array)) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in matrixtocomplexdp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in matrixtocomplexdp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in matrixtocomplexdp"
        write(0, *) "Malformed input"
        stop
      end select
    end if


  end subroutine matrixtocomplexdp
  
end module readarrays


module m_common_parse_input

  use m_common_charset, only : XML_WHITESPACE
  use m_common_realtypes, only: sp, dp

  implicit none
  private

  interface rts
    module procedure scalartostring
    module procedure scalartological
    module procedure scalartointeger
    module procedure scalartorealsp
    module procedure scalartorealdp
    module procedure scalartocomplexsp
    module procedure scalartocomplexdp
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

  public :: rts

contains

  subroutine scalartostring(s, data, separator, num, iostat)
    character(len=*), intent(in) :: s
    character(len=*), intent(out) :: data
    character, intent(in), optional :: separator
    integer, intent(out), optional :: num, iostat
    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = ""
    ij = 0
    length = 1
    loop: do i = 1, 1
      if (present(separator)) then
        k = index(s(s_i:), separator)
      else
        k = verify(s(s_i:), XML_WHITESPACE)
        if (k==0) exit loop
        s_i = s_i + k - 1
        k = scan(s(s_i:), XML_WHITESPACE)
      endif
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      data = s(s_i:k)
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in scalartostring"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in scalartostring"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in scalartostring"
        write(0, *) "Malformed input"
        stop
      end select
    end if

  end subroutine scalartostring

  subroutine scalartological(s, data, num, iostat)
    character(len=*), intent(in) :: s
    logical, intent(out) :: data
    integer, intent(out), optional :: num, iostat
    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = .false.
    ij = 0
    length = 1
    loop: do i = 1, 1
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k
      if (s(s_i:s_i)==",") then
        k = verify(s(s_i:), XML_WHITESPACE)
        s_i = s_i + k 
      endif
      s_i = s_i - 1
      k = scan(s(s_i:), XML_WHITESPACE//",")
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      print*,"!", s(s_i:k), "!"
      if (s(s_i:k)=="true".or.s(s_i:k)=="1") then
        data = .true.
      elseif (s(s_i:k)=="false".or.s(s_i:k)=="0") then
        data = .false.
      else
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in scalartological"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in scalartological"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in scalartological"
        write(0, *) "Malformed input"
        stop
      end select
    end if

  end subroutine scalartological

  subroutine scalartointeger(s, data, num, iostat)
    character(len=*), intent(in) :: s
    integer, intent(out) :: data
    integer, intent(out), optional :: num, iostat

    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0
    ij = 0
    length = 1
    loop: do i = 1, 1
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in scalartointeger"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in scalartointeger"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in scalartointeger"
        write(0, *) "Malformed input"
        stop
      end select
    end if

  end subroutine scalartointeger

  subroutine scalartorealsp(s, data, num, iostat)
    character(len=*), intent(in) :: s
    real(sp), intent(out) :: data
    integer, intent(out), optional :: num, iostat

    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0.0_sp
    ij = 0
    length = 1
    loop: do i = 1, 1
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in scalartorealsp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in scalartorealsp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in scalartorealsp"
        write(0, *) "Malformed input"
        stop
      end select
    end if

  end subroutine scalartorealsp

  subroutine scalartorealdp(s, data, num, iostat)
    character(len=*), intent(in) :: s
    real(dp), intent(out) :: data
    integer, intent(out), optional :: num, iostat

    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0.0_dp
    ij = 0
    length = 1
    loop: do i = 1, 1
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in scalartorealdp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in scalartorealdp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in scalartorealdp"
        write(0, *) "Malformed input"
        stop
      end select
    end if

  end subroutine scalartorealdp

  subroutine scalartocomplexsp(s, data, num, iostat)
    character(len=*), intent(in) :: s
    complex(sp), intent(out) :: data
    integer, intent(out), optional :: num, iostat

    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0.0_sp
    ij = 0
    length = 1
    loop: do i = 1, 1
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in scalartocomplexsp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in scalartocomplexsp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in scalartocomplexsp"
        write(0, *) "Malformed input"
        stop
      end select
    end if

  end subroutine scalartocomplexsp

  subroutine scalartocomplexdp(s, data, num, iostat)
    character(len=*), intent(in) :: s
    complex(dp), intent(out) :: data
    integer, intent(out), optional :: num, iostat

    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0.0_dp
    ij = 0
    length = 1
    loop: do i = 1, 1
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
      if (err==0) err = -1
    else
      if (verify(s(s_i:), XML_WHITESPACE)/=0) err = 1
    endif


    if (present(iostat)) then
      iostat = err
    else
      select case (err)
      case(-1)
        write(0, *) "Error in scalartocomplexdp"
        write(0, *) "Too few elements found"
        stop
      case(1)
        write(0, *) "Error in scalartocomplexdp"
        write(0, *) "Too many elements found"
        stop
      case(2)
        write(0, *) "Error in scalartocomplexdp"
        write(0, *) "Malformed input"
        stop
      end select
    end if

  end subroutine scalartocomplexdp



  subroutine arraytostring(s, data, separator, num, iostat)
    character(len=*) :: data(:)
    character, intent(in), optional :: separator
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = ""
    ij = 0
    length = size(data)
    loop: do i = 1, size(data)
      if (present(separator)) then
        k = index(s(s_i:), separator)
      else
        k = verify(s(s_i:), XML_WHITESPACE)
        if (k==0) exit loop
        s_i = s_i + k - 1
        k = scan(s(s_i:), XML_WHITESPACE)
      endif
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      data(i) = s(s_i:k)
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<size(data)) then
      err = -1
    else
      if (present(separator)) then
        if (len(s)-s_i>=0) &
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

  subroutine matrixtostring(s, data, separator, num, iostat)
    character(len=*) :: data(:,:)
    character, intent(in), optional :: separator
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = ""
    ij = 0
    length = size(data)
    loop: do j = 1, size(data, 2)
    do i = 1, size(data, 1)
      if (present(separator)) then
        k = index(s(s_i:), separator)
      else
        k = verify(s(s_i:), XML_WHITESPACE)
        if (k==0) exit loop
        s_i = s_i + k - 1
        k = scan(s(s_i:), XML_WHITESPACE)
      endif
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      data(i, j) = s(s_i:k)
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop
    end do
    end do loop

    if (present(num)) num = ij
    if (ij<size(data)) then
      err = -1
    else
      if (present(separator)) then
        if (len(s)-s_i>=0) &
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

  subroutine arraytological(s, data, num, iostat)
    logical, intent(out) :: data(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = .false.
    ij  = 0
    length = size(data)
    loop: do i = 1, size(data)
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k
      if (s(s_i:s_i)==",") then
        k = verify(s(s_i:), XML_WHITESPACE)
        s_i = s_i + k 
      endif
      s_i = s_i - 1
      k = scan(s(s_i:), XML_WHITESPACE//",")
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      print*,"!", s(s_i:k), "!"
      if (s(s_i:k)=="true".or.s(s_i:k)=="1") then
        data(i) = .true.
      elseif (s(s_i:k)=="false".or.s(s_i:k)=="0") then
        data(i) = .false.
      else
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine matrixtological(s, data, num, iostat)
    logical, intent(out) :: data(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = .false.
    ij = 0
    length = size(data)
    loop: do j = 1, size(data, 2)
    do i = 1, size(data, 1)
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k
      if (s(s_i:s_i)==",") then
        k = verify(s(s_i:), XML_WHITESPACE)
        s_i = s_i + k 
      endif
      s_i = s_i - 1
      k = scan(s(s_i:), XML_WHITESPACE//",")
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      print*,"!", s(s_i:k), "!"
      if (s(s_i:k)=="true".or.s(s_i:k)=="1") then
        data(i, j) = .true.
      elseif (s(s_i:k)=="false".or.s(s_i:k)=="0") then
        data(i, j) = .false.
      else
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do
    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine arraytointeger(s, data, num, iostat)
    integer, intent(out) :: data(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c

    s_i = 1
    err = 0
    data = 0
    ij  = 0
    length = size(data)
    loop: do i = 1, size(data)
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data(i)
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine matrixtointeger(s, data, num, iostat)
    integer, intent(out) :: data(:, :)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0
    ij = 0
    length = size(data)
    loop: do j = 1, size(data, 2)
    do i = 1, size(data, 1)
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data(i, j)
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do
    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine arraytorealsp(s, data, num, iostat)
    real(sp), intent(out) :: data(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0
    ij  = 0
    length = size(data)
    loop: do i = 1, size(data)
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data(i)
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine matrixtorealsp(s, data, num, iostat)
    real(sp), intent(out) :: data(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0
    ij = 0
    length = size(data)
    loop: do j = 1, size(data, 2)
    do i = 1, size(data, 1)
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data(i, j)
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do
    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine arraytorealdp(s, data, num, iostat)
    real(dp), intent(out) :: data(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = 0
    ij  = 0
    length = size(data)
    loop: do i = 1, size(data)
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data(i)
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine matrixtorealdp(s, data, num, iostat)
    real(dp), intent(out) :: data(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = cmplx(0,0)
    ij = 0
    length = size(data)
    loop: do j = 1, size(data, 2)
    do i = 1, size(data, 1)
      k = verify(s(s_i:), XML_WHITESPACE)
      if (k==0) exit loop
      s_i = s_i + k - 1
      k = scan(s(s_i:), XML_WHITESPACE)
      if (k==0) then
        k = len(s)
      else
        k = s_i + k - 2
      endif
      read(s(s_i:k), *, iostat=ios) data(i, j)
      if (ios/=0) then
        err = 2
        exit loop
      endif
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do
    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine arraytocomplexsp(s, data, num, iostat)
    complex(sp), intent(out) :: data(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = cmplx(0,0)
    ij  = 0
    length = size(data)
    loop: do i = 1, size(data)
      k = index(s(s_i:), "(")
      if (k==0) exit loop
      s_i = s_i + k
      k = index(s(s_i:), ")+i(")
      if (k==0) then
        err = 2
        exit loop
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) r
      if (ios/=0) then
        err = 2
        exit loop
      endif
      s_i = k + 5
      k = index(s(s_i:), ")")
      if (k==0) then
        err = 2
        exit loop
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) c
      if (ios/=0) then
        err = 2
        exit loop
      endif
      data(i) = cmplx(r, c)
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine matrixtocomplexsp(s, data, num, iostat)
    complex(sp), intent(out) :: data(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = cmplx(0,0)
    ij = 0
    length = size(data)
    loop: do j = 1, size(data, 2)
    do i = 1, size(data, 1)
      k = index(s(s_i:), "(")
      if (k==0) exit loop
      s_i = s_i + k
      k = index(s(s_i:), ")+i(")
      if (k==0) then
        err = 2
        exit loop
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) r
      if (ios/=0) then
        err = 2
        exit loop
      endif
      s_i = k + 5
      k = index(s(s_i:), ")")
      if (k==0) then
        err = 2
        exit loop
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) c
      if (ios/=0) then
        err = 2
        exit loop
      endif
      data(i, j) = cmplx(r, c)
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do
    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine arraytocomplexdp(s, data, num, iostat)
    complex(dp), intent(out) :: data(:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = cmplx(0)
    ij  = 0
    length = size(data)
    loop: do i = 1, size(data)
      k = index(s(s_i:), "(")
      if (k==0) exit loop
      s_i = s_i + k
      k = index(s(s_i:), ")+i(")
      if (k==0) then
        err = 2
        exit loop
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) r
      if (ios/=0) then
        err = 2
        exit loop
      endif
      s_i = k + 5
      k = index(s(s_i:), ")")
      if (k==0) then
        err = 2
        exit loop
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) c
      if (ios/=0) then
        err = 2
        exit loop
      endif
      data(i) = cmplx(r, c)
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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

  subroutine matrixtocomplexdp(s, data, num, iostat)
    complex(dp), intent(out) :: data(:,:)
    character(len=*), intent(in) :: s
    integer, intent(out), optional :: num
    integer, intent(out), optional :: iostat


    integer :: i, j, ij, k, s_i, err, ios, length
    real :: r, c


    s_i = 1
    err = 0
    data = cmplx(0,0)
    ij = 0
    length = size(data)
    loop: do j = 1, size(data, 2)
    do i = 1, size(data, 1)
      k = index(s(s_i:), "(")
      if (k==0) exit loop
      s_i = s_i + k
      k = index(s(s_i:), ")+i(")
      if (k==0) then
        err = 2
        exit loop
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) r
      if (ios/=0) then
        err = 2
        exit loop
      endif
      s_i = k + 5
      k = index(s(s_i:), ")")
      if (k==0) then
        err = 2
        exit loop
      else
        k = s_i + k - 2
      endif
      ! FIXME should write our own here so it is not recursive I/O
      ! Also not 100% sure that Fortran standard fp read is same as CMLComp
      read(s(s_i:k), *, iostat=ios) c
      if (ios/=0) then
        err = 2
        exit loop
      endif
      data(i, j) = cmplx(r, c)
      ij = ij + 1
      s_i = k + 2
      if (ij<length.and.s_i>len(s)) exit loop

    end do
    end do loop

    if (present(num)) num = ij
    if (ij<length) then
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
  
end module m_common_parse_input


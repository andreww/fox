module m_common_format

  use pxf, only: pxfabort, pxfflush

  implicit none
  private


  integer, parameter :: sp = selected_real_kind(6,30)
  integer, parameter :: dp = selected_real_kind(14,100)
  integer, parameter :: sig_sp = digits(sp)/2
  integer, parameter :: sig_dp = digits(dp)/2 ! Approximate precision worth outputting of each type.

  character(len=*), parameter :: digit = "0123456789"
  character(len=*), parameter :: hexdigit = "0123456789abcdefABCDEF"

  interface str
    module procedure str_integer, str_integer_array, &
                     str_logical, str_logical_array, &
                     str_real_dp, str_real_dp_fmt!, &
                     !str_real_sp, str_real_sp_fmt!, &
  end interface str

  public :: str

  public :: str_to_int_10
  public :: str_to_int_16

contains

  pure function str_to_int_10(str) result(n)
    character(len=*), intent(in) :: str
    integer :: n

    integer :: max_power, i, j

    if (verify(str, digit) > 0) call pxfabort()

    max_power = len(str) - 1


    n = 0
    do i = 0, max_power
      j = max_power - i + 1
      n = n + (index(digit, str(j:j)) - 1) * 10**i
    enddo

  end function str_to_int_10


  pure function str_to_int_16(str) result(n)
    character(len=*), intent(in) :: str
    integer :: n
    
    character(len=len(str)) :: str_l
    integer :: max_power, i, j

    if (verify(str, hexdigit) == 0) then
       str_l = to_lower(str)
    else
      call pxfabort()
    endif

    max_power = len(str) - 1

    n = 0
    do i = 0, max_power
      j = max_power - i + 1
      n = n + (index(hexdigit, str_l(j:j)) - 1) * 10**i
    enddo

  contains
    pure function to_lower(s) result(s2)
      character(len=*), intent(in) :: s
      character(len=len(s)) :: s2
      character(len=12), parameter :: hex = "ABCDEFabcdef"
      integer :: j, k
      do j = 1, len(s)
        if (verify(s(j:j), digit) == 0) then
          s2(j:j) = s(j:j)
        else
          k = index(s(j:j), hex)+6
          s2(j:j) = hex(k:k)
        endif
      enddo
    end function to_lower
         
  end function str_to_int_16


  pure function str_integer(i) result(s)
    integer, intent(in) :: i
    character(len=int(log10(real(max(abs(i),1)))) + 1 + dim(-i,0)/max(abs(i),1)) :: s

    integer :: ii, j, k, n


    if (i < 0) then
      s(1:1) = '-'
      n = 2
    else
      n = 1
    endif
    ii = abs(i)
    do k = len(s) - n, 0, -1
      j = ii/(10**k)
      ii = ii - j*(10**k)
      s(n:n) = digit(j+1:j+1)
      n = n + 1
    enddo

  end function str_integer


  pure function str_integer_array_len(ia) result(n)
    integer, dimension(:), intent(in) :: ia
    integer :: n
    
    integer :: j

    n = size(ia) - 1

    do j = 1, size(ia)
      n = n + len(str(ia(j)))
    enddo

  end function str_integer_array_len


  pure function str_integer_array(ia, delimiter) result(s)
    integer, dimension(:), intent(in) :: ia
    character(len=1), optional, intent(in) :: delimiter
    character(len=str_integer_array_len(ia)) :: s

    integer :: j, k, n
    character(len=1) :: d
    if (present(delimiter)) then
      d = delimiter
    else
      d = ' '
    endif

    n = 1
    do k = 1, size(ia) - 1
      j = len(str(ia(j)))
      s(n:n+j-1) = str(ia(j))
      n = n + j + 1
      s(n-1:n-1) = d
    enddo

  end function str_integer_array
    
  
  pure function str_logical(l) result(s)
    logical, intent(in)   :: l
    character(len=merge(4,5,l)) :: s
    
    if (l) then
      s='true'
    else
      s='false'
    endif
  end function str_logical

  
  pure function str_logical_array(la, delimiter) result(s)
    logical, dimension(:), intent(in)   :: la
    character(len=1), optional, intent(in) :: delimiter
    character(len=5*size(la) - 1 + count(.not.la)) :: s
    
    integer :: j, k, n
    character(len=1) :: d
    if (present(delimiter)) then
      d = delimiter
    else
      d = ' '
    endif

    n = 1
    do k = 1, size(la) - 1
      if (la(k)) then
        s(n:n+4-1) = 'true'
        n = n + 5
      else
        s(n:n+4-1) = 'false'
        n = n + 6
      endif
      s(n-1:n-1) = d
    enddo

  end function str_logical_array

  
  ! In order to convert real numbers to strings, we need to
  ! perform an internal write - but how long will the 
  ! resultant string be? We don't know & there is no way
  ! to discover for an arbitrary format. Therefore, 
  ! (if we have the capability; f95 or better)
  ! we assume it will be less than 100 characters, write
  ! it to a string of that length, then remove leading &
  ! trailing whitespace. (this means that if the specified
  ! format includes whitespace, this will be lost.)
  !
  ! If we are working with an F90-only compiler, then
  ! we cannot do this trick - the output string will
  ! always be 100 chars in length, though we will remove
  ! leading whitespace. 


  ! The standard Fortran format functions do not give us
  ! enough control, so we write our own real number formatting
  ! routines here. For each real type, we optionally take a
  ! format like so:
  ! "r<integer>" which will produce output without an exponent,
  ! and <integer> digits after the decimal point.
  ! or
  ! "s<integer>": which implies scientific notation, with an 
  ! exponent, with <integer> significant figures.
  ! If the integer is absent, then the precision will be
  ! half of the number of significant figures available
  ! for that real type.
  ! The absence of a format implies scientific notation, with
  ! the default precision.

  ! These routines are fairly imperfect - they are inaccurate for
  ! the lower-end bits of the number, since they work by simple
  ! multiplications by 10.
  ! Also they will probably be orders of magnitude slower than library IO.
  ! Ideally they'd be rewritten to convert from teh native format by
  ! bit-twidding. Not sure how to do that portably though.

  ! The format specification could be done more nicely - but unfortunately
  ! not in F95 due to *stupid* restrictions on specification expressions.

  ! And I wouldn't have to invent my own format specification if Fortran
  ! had a proper IO library anyway.

!FIXME we should round the last digit, but that's an arse to do correctly.

  function real_dp_str(x, sig) result(s)
    real(dp), intent(in) :: x
    integer, intent(in) :: sig
    character(len=sig) :: s
    ! make a string of numbers sig long of x.
    integer :: e, i, j, k, n
    real(dp) :: x_

    if (x == 0.0_dp) then
      e = 1
    else
      e = floor(log10(abs(x)))
    endif
    x_ = abs(x) / (10.0_dp**e)
    n = 1
    do k = sig - 2, 0, -1
      j = int(x_)
      s(n:n) = digit(j+1:j+1)
      n = n + 1
      x_ = (x_ - j) * 10.0_dp
    enddo
    j = nint(x_)
    if (j == 10) then
      ! Now round ...
      s(n:n) = '9'
      i = verify(s, '9', .true.)
      !if (i==1) then
      !argh need another number if this is from dec
      j = index(digit, s(i:i))
      s(i:i) = digit(j+1:j+1)
      s(i+1:) = repeat('0', sig - i + 1)
    else
      s(n:n) = digit(j+1:j+1)
    endif

  end function real_dp_str

  function str_real_dp_fmt(x, fmt) result(s)
    real(dp), intent(in) :: x
    character(len=*), intent(in) :: fmt
    character(len=str_real_dp_fmt_len(x, fmt)) :: s

    integer :: sig, dec
    integer :: e, i, j, k, n
    real(dp) :: x_
    character(len=len(s)) :: num !this wll always be enough memory.

    s= ''

    if (x == 0.0_dp) then
      e = 1
    else
      e = floor(log10(abs(x)))
    endif

    if (x < 0.0_dp) then
      s(1:1) = "-"
      n = 2
    else
      n = 1
    endif

    if (len(fmt) == 0) then

      sig = sig_dp

      num = real_dp_str(abs(x), sig)
      if (sig == 1) then
        s(n:n) = num
      else
        s(n:n+1) = num(1:1)//'.'
        s(n+2:n+sig) = num(2:)
      endif
      n = n + sig

      s(n:n) = 'e'
      s(n+1:) = str(e)

    elseif (fmt(1:1) == 'r') then

      if (len(fmt) > 1) then
        dec = str_to_int_10(fmt(2:))
      else
        dec = sig_dp - e
      endif

      x_ = abs(x) / (10.0_dp**e)

      if (abs(x) > 1.0_dp) then
        j = int(x_)
        s(n:n) = digit(j+1:j+1)
        x_ = (x_ - j) * 10.0_dp
        n = n + 1
        do k = e - 1, 0, -1
          j = int(x_)
          x_ = (x_ - j) * 10.0_dp
          s(n:n) = digit(j+1:j+1)
          n = n + 1
        enddo
        if (dec > 1) then
          s(n:n) = '.'
          n = n + 1
          do k = dec - 1, 0, -1
            j = int(x_)
            x_ = (x_ - j) * 10.0_dp
            s(n:n) = digit(j+1:j+1)
            n = n + 1
          enddo
        endif
      else
        s(n:n) = '0'
        if (dec > 0) then
          s(n+1:n+1) = '.'
          n = n + 2
          if (dec < -e-1) then
            s(n:) = repeat('0', dec)
          else
            s(n:n-e-2) = repeat('0', -e-1)
            n = n - e - 1
            if (n <= len(s)) then
              j = int(x_)
              s(n:n) = digit(j+1:j+1)
              n = n + 1
              do k = dec + e - 1, 0, -1
                x_ = (x_ - j) * 10.0_dp
                j = int(x_)
                s(n:n) = digit(j+1:j+1)
                n = n + 1
              enddo
            endif
          endif
        endif
      endif

    elseif (fmt(1:1) == 's') then

      if (len(fmt) > 1) then
        sig = str_to_int_10(fmt(2:))
      else
        sig = sig_dp
      endif
      sig = max(sig, 1)
      sig = min(sig, digits(dp))

      num = real_dp_str(abs(x), sig)
      if (sig == 1) then
        s(n:n) = num
        n = n + 1
      else
        s(n:n+1) = num(1:1)//'.'
        s(n+2:n+sig) = num(2:)
        n = n + sig + 1
      endif

      s(n:n) = 'e'
      s(n+1:) = str(e)

    endif

  end function str_real_dp_fmt


  pure function str_real_dp_fmt_len(x, fmt) result(n)
    real(dp), intent(in) :: x
    character(len=*), intent(in) :: fmt
    integer :: n

    integer :: td, dec, sig
    integer :: e
      public :: str_real_dp_fmt_len


    if (x == 0.0_dp) then
      e = 1
    else
      e = floor(log10(abs(x)))
    endif
      
    if (x < 0.0_dp) then
      n = 1
    else
      n = 0
    endif
      
    if (len(fmt) == 0) then
      sig = sig_dp

      n = n + sig + 2 + len(str(e)) 
      ! for the decimal point and the e

    elseif (fmt(1:1) == 'r') then

     if (len(fmt) > 1) then
        dec = str_to_int_10(fmt(2:))
      else
        dec = sig_dp - e
      endif

      n = n + 1
      if (abs(x) > 1.0_dp) then
        ! we want all digits before the decimal point.
        n = n + e - 1 !unless we round
      endif
      if (dec > 1) then
        n = n + dec + 1
      endif
      
    elseif (fmt(1:1) == 's') then
      if (len(fmt) > 1) then
        sig = str_to_int_10(fmt(2:))
      else
        sig = sig_dp
      endif
      sig = max(sig, 1)
      sig = min(sig, digits(dp))

      if (sig > 1) n = n + 1 
      ! for the decimal point
      
      n = n + sig + 1 + len(str(e))
    else
      call pxfabort()
    endif

  end function str_real_dp_fmt_len


  function str_real_dp(x) result(s)
    real(dp), intent(in) :: x
    character(len=str_real_dp_len(x)) :: s

    s = ""
    !s = str_real_dp_fmt(x, "")

  end function str_real_dp


  pure function str_real_dp_len(x) result(n)
    real(dp), intent(in) :: x
    integer :: n

    n = 0
    !n = str_real_dp_fmt_len(x, "")

  end function str_real_dp_len

          
end module m_common_format

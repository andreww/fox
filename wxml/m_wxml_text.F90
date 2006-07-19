module m_wxml_text

  use m_common_format, only: str_to_int_10
  use m_pxf_abort_flush
  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)

  character(len=*), parameter :: digit = "0123456789"

  interface str
    module procedure str_integer, str_integer_array, &
                     str_logical, str_logical_array, &
                     str_real_dp, str_real_dp_fmt!, &
                     !str_real_dp_array_no_fmt, str_real_dp_array_with_fmt, &
                     !str_real_sp
                     !str_real_sp_array_no_fmt, str_real_sp_array_with_fmt
  end interface

  public :: str

contains 

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
  ! The absence of a format implies scientific notation, with
  ! as many significant figures as the format supports.

  ! These routines are fairly imperfect - they will almost certainly 
  ! behave badly for denormalized numbers. Also they will probably
  ! be orders of magnitude slower than library IO.

  ! The format specification could be done more nicely - but unfortunately
  ! not in F95 due to *stupid* restrictions on specification expressions.

  ! And I wouldn't have to invent my own format specification if Fortran
  ! had a proper IO library anyway.

  pure function str_real_dp_fmt(x, fmt) result(s)
    real(dp), intent(in) :: x
    character(len=*), intent(in) :: fmt
    character(len=str_real_dp_fmt_len(x, fmt)) :: s

    integer :: sig
    integer :: e, i, j, k, n, predecimal
    real(dp) :: x_

    if (len(fmt) == 0) then
      sig = digits(dp)
      if (x < 0.0_dp) then
        s(1:1) = "-"
        n = 1
      else
        n = 0
      endif
      
      if (x == 0.0_dp) then
        e = 1
      else
        e = int(log10(abs(x)))
      endif
      j = int(x/(10**e))
      x_ = x - j*(10**e)
      
      s(n:n+1) = digit(j:j)//'.'
      do k = sig - 1, 0, -1
        j = int(x_/(10**k))
        x_ =  x_ - j*(10**k)
        s(n:n) = digit(j+1:j+1)
        n = n + 1
      enddo

      s(n:n) = 'e'
      s(n+1:) = str(e)
      

      n = n + sig + 1 + 1 + len(str(e))

    endif

  end function str_real_dp_fmt


  pure function str_real_dp_fmt_len(x, fmt) result(n)
    real(dp), intent(in) :: x
    character(len=*), intent(in) :: fmt
    integer :: n

    integer :: td, dec, sig
    integer :: e
    
    if (len(fmt) < 1) then
      sig = digits(dp)
      if (x < 0.0_dp) then
        n = 1
      else
        n = 0
      endif
      
      if (x == 0.0_dp) then
        e = 1
      else
        e = int(log10(abs(x)))
      endif
      
      n = n + sig + 1 + 1 + len(str(e))

    elseif (fmt(1:1) == 'r') then
      if (abs(x) == 0.0_dp) then
        td = digits(dp)
      else
        td = digits(dp) - int(log10(abs(x))) - 1
      endif
      
      if (len(fmt) > 1) then
        dec = str_to_int_10(fmt(2:))
        dec = max(dec, 0)
        dec = min(dec, td)
      else
        dec = td
      endif

      if (x < 0.0_dp) then
        n = 1
      else
        n = 0
      endif

      if (abs(x) == 0.0_dp) then
        n = n + dec + 1
      elseif (abs(x) > 1.0_dp) then
        n = n + int(log10(abs(x))) + 1
      else
        n = n + 2 + dec
      endif
      
    elseif (fmt(1:1) == 's') then
      if (len(fmt) > 1) then
        sig = str_to_int_10(fmt(2:))
      else
        sig = digits(dp)
      endif
      sig = max(sig, 1)
      sig = min(sig, digits(dp))
      
      if (x < 0.0_dp) then
        n = 1
      else
        n = 0
      endif
      
      if (x == 0.0_dp) then
        e = 1
      else
        e = int(log10(abs(x)))
      endif
      
      n = n + sig + 1 + 1 + len(str(e))
    else
      call pxfabort()
    endif

  end function str_real_dp_fmt_len


  pure function str_real_dp(x) result(s)
    real(dp), intent(in) :: x
    character(len=str_real_dp_len(x)) :: s

    s = str_real_dp_fmt(x, "")

  end function str_real_dp


  pure function str_real_dp_len(x) result(n)
    real(dp), intent(in) :: x
    integer :: n

    n = str_real_dp_fmt_len(x, "")

  end function str_real_dp_len

end module m_wxml_text

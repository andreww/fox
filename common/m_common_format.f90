module m_common_format

  use m_common_realtypes, only: sp, dp
  use m_common_error, only: FoX_error
  use pxf, only: pxfabort, pure_pxfabort

  implicit none
  private

  integer, parameter :: sig_sp = digits(1.0_sp)/4
  integer, parameter :: sig_dp = digits(1.0_dp)/4 ! Approximate precision worth outputting of each type.

  character(len=*), parameter :: digit = "0123456789"
  character(len=*), parameter :: hexdigit = "0123456789abcdefABCDEF"

  interface str
    module procedure str_string, str_string_array, str_string_matrix, &
                     str_integer, str_integer_array, str_integer_matrix, &
                     str_logical, str_logical_array, str_logical_matrix, &
                     str_real_dp, str_real_dp_fmt, &
                     str_real_dp_array, str_real_dp_array_fmt, &
                     str_real_dp_matrix, str_real_dp_matrix_fmt, &
                     str_real_sp, str_real_sp_fmt, &
                     str_real_sp_array, str_real_sp_array_fmt, &
                     str_real_sp_matrix, str_real_sp_matrix_fmt
  end interface str

  public :: str

  public :: str_to_int_10
  public :: str_to_int_16

contains

  pure function str_to_int_10(str) result(n)
    character(len=*), intent(in) :: str
    integer :: n

    integer :: max_power, i, j

    if (verify(str, digit) > 0) n = pure_pxfabort()

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
      n = pure_pxfabort()
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


  pure function str_string(st) result(s)
    character(len=*), intent(in) :: st
    character(len=len(st)) :: s

    s = st
  end function str_string


  pure function str_string_array_len(st) result(n)
    character(len=*), dimension(:), intent(in) :: st
    integer :: n

    integer :: k

    n = size(st) - 1
    do k = 1, size(st)
      n = n + len(st(k))
    enddo

  end function str_string_array_len


  pure function str_string_array(st, delimiter) result(s)
    character(len=*), dimension(:), intent(in) :: st
    character(len=1), intent(in), optional :: delimiter
    character(len=str_string_array_len(st)) :: s
    
    integer :: k, n
    character(len=1) :: d
    
    if (present(delimiter)) then
      d = delimiter
    else
      d = ' '
    endif

    n = 1
    do k = 1, size(st) - 1
      s(n:n+len(st(k))) = st(k)//d
      n = n + len(st(k)) + 1
    enddo
    s(n:) = st(k)

  end function str_string_array


  pure function str_string_matrix(st, delimiter) result(s)
    character(len=*), dimension(:, :), intent(in) :: st
    character(len=1), intent(in), optional :: delimiter
    character(len=str_string_array_len(reshape(st,(/size(st)/)))) :: s
    
    s = str_string_array(reshape(st,(/size(st)/)), delimiter)

  end function str_string_matrix


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
      j = len(str(ia(k)))
      s(n:n+j) = str(ia(k))//d
      n = n + j + 1
    enddo
    s(n:) = str(ia(k))

  end function str_integer_array


  pure function str_integer_matrix(ia, delimiter) result(s)
    integer, dimension(:,:), intent(in) :: ia
    character(len=1), optional, intent(in) :: delimiter
    character(len=str_integer_array_len(reshape(ia,(/size(ia)/)))) :: s

    s = str_integer_array(reshape(ia, (/size(ia)/)), delimiter)

  end function str_integer_matrix

  
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
    
    integer :: k, n
    character(len=1) :: d
    if (present(delimiter)) then
      d = delimiter
    else
      d = ' '
    endif

    n = 1
    do k = 1, size(la) - 1
      if (la(k)) then
        s(n:n+3) = 'true'
        n = n + 5
      else
        s(n:n+4) = 'false'
        n = n + 6
      endif
      s(n-1:n-1) = d
    enddo
    if (la(k)) then
      s(n:) = 'true'
    else
      s(n:) = 'false'
    endif

  end function str_logical_array


  pure function str_logical_matrix(la, delimiter) result(s)
    logical, dimension(:,:), intent(in)   :: la
    character(len=1), optional, intent(in) :: delimiter
    character(len=5*size(la) - 1 + count(.not.la)) :: s

    s = str_logical_array(reshape(la, (/size(la)/)), delimiter)

  end function str_logical_matrix
  

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

!FIXME Signed zero is not handled correctly; don't quite understand why.
!FIXME too much duplication between sp & dp, we should m4.

  pure function real_sp_str(x, sig) result(s)
    real(sp), intent(in) :: x
    integer, intent(in) :: sig
    character(len=sig) :: s
    ! make a string of numbers sig long of x.
    integer :: e, i, j, k, n
    real(sp) :: x_

    if (sig < 1) then
      s ='' 
      return
    endif

    if (x == 0.0_sp) then
      e = 1
    else
      e = floor(log10(abs(x)))
    endif
    x_ = abs(x) / (10.0_sp**e)
    n = 1
    do k = sig - 2, 0, -1
! We can't store int(x_) here or we'll get inconsistent
! answers on the x87 fpu when optimized
! FIXME we need to fix this to get the right answer as well...
      s(n:n) = digit(int(x_)+1:int(x_)+1)
      n = n + 1
      x_ = (x_ - int(x_)) * 10.0_sp
    enddo
    j = nint(x_)
    if (j == 10) then
      ! Now round ...
      s(n:n) = '9'
      i = verify(s, '9', .true.)
      if (i == 0) then
        s(1:1) = '!'
        !overflow
        return
      endif
      j = index(digit, s(i:i))
      s(i:i) = digit(j+1:j+1)
      s(i+1:) = repeat('0', sig - i + 1)
    else
      s(n:n) = digit(j+1:j+1)
    endif

  end function real_sp_str


  pure function str_real_sp_fmt_len(x, fmt) result(n)
    real(sp), intent(in) :: x
    character(len=*), intent(in) :: fmt
    integer :: n

    integer :: dec, sig
    integer :: e

    if (x == 0.0_sp) then
      e = 1
    else
      e = floor(log10(abs(x)))
    endif
      
    if (x < 0.0_sp) then
      n = 1
    else
      n = 0
    endif
      
    if (len(fmt) == 0) then
      sig = sig_sp

      n = n + sig + 2 + len(str(e)) 
      ! for the decimal point and the e

    elseif (fmt(1:1) == 's') then
      if (len(fmt) > 1) then
        sig = str_to_int_10(fmt(2:))
      else
        sig = sig_sp
      endif
      sig = max(sig, 1)
      sig = min(sig, digits(1.0_sp))

      if (sig > 1) n = n + 1 
      ! for the decimal point
      
      n = n + sig + 1 + len(str(e))

    elseif (fmt(1:1) == 'r') then

      if (len(fmt) > 1) then
        dec = str_to_int_10(fmt(2:))
      else
        dec = sig_sp - e - 1
      endif
      dec = max(dec, 0)
      dec = min(dec, digits(1.0_sp)-e)

      if (dec > 0) n = n + 1
      if (abs(x) >= 1.0_sp) n = n + 1

      ! Need to know if there's an overflow ....
      if (e+dec+1 > 0) then
        if (index(real_sp_str(abs(x), e+dec+1), '!') == 1) &
             e = e + 1
      endif

      n = n + abs(e) + dec

    else
      n = pure_pxfabort()
    endif

  end function str_real_sp_fmt_len


  function str_real_sp_fmt(x, fmt) result(s)
    real(sp), intent(in) :: x
    character(len=*), intent(in) :: fmt
    character(len=str_real_sp_fmt_len(x, fmt)) :: s

    integer :: sig, dec
    integer :: e, n
    character(len=str_real_sp_fmt_len(x, fmt)) :: num !this wll always be enough memory.

    if (.not.checkFmt(fmt)) &
      call FoX_error("Invalid format: "//fmt)

    if (x == 0.0_sp) then
      e = 0
    else
      e = floor(log10(abs(x)))
    endif

    if (x < 0.0_sp) then
      s(1:1) = "-"
      n = 2
    else
      n = 1
    endif

    if (len(fmt) == 0) then

      sig = sig_sp

      num = real_sp_str(abs(x), sig)
      if (num(1:1) == '!') then
        e = e + 1
        num = '1'//repeat('0',len(num)-1)
      endif

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

    elseif (fmt(1:1) == 's') then

      if (len(fmt) > 1) then
        sig = str_to_int_10(fmt(2:))
      else
        sig = sig_sp
      endif
      sig = max(sig, 1)
      sig = min(sig, digits(1.0_sp))

      num = real_sp_str(abs(x), sig)
      if (num(1:1) == '!') then
        e = e + 1
        num = '1'//repeat('0',len(num)-1)
      endif

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

    elseif (fmt(1:1) == 'r') then

      if (len(fmt) > 1) then
        dec = str_to_int_10(fmt(2:))
      else
        dec = sig_sp - e - 1
      endif
      dec = max(dec, 0)
      dec = min(dec, digits(1.0_sp)-e-1)

      if (e+dec+1 > 0) then
        num = real_sp_str(abs(x), e+dec+1)
      else
        num = ''
      endif
      if (num(1:1) == '!') then
        e = e + 1
        num = '1'//repeat('0',len(num)-1)
      endif

      if (abs(x) >= 1.0_sp) then
        s(n:n+e) = num(:e+1)
        n = n + e + 1
        if (dec > 0) then
          s(n:n) = '.'
          n = n + 1
          s(n:) = num(e+2:)
        endif
      else
        s(n:n) = '0'
        if (dec > 0) then
          s(n+1:n+1) = '.'
          n = n + 2
          if (dec < -e-1) then
            s(n:) = repeat('0', dec)
          else
            s(n:n-e-2) = repeat('0', max(-e-1,0))
            n = n - min(e,-1) - 1
            if (n <= len(s)) then
              s(n:) = num
            endif
          endif
        endif
      endif

    endif

  end function str_real_sp_fmt


  pure function str_real_sp_len(x) result(n)
    real(sp), intent(in) :: x
    integer :: n

    n = str_real_sp_fmt_len(x, "")

  end function str_real_sp_len


  function str_real_sp(x) result(s)
    real(sp), intent(in) :: x
    character(len=str_real_sp_len(x)) :: s

    s = str_real_sp_fmt(x, "")

  end function str_real_sp

     
  pure function str_real_sp_array_len(xa) result(n)
    real(sp), dimension(:), intent(in) :: xa
    integer :: n

    integer :: k

    n = size(xa) - 1
    do k = 1, size(xa)
      n = n + str_real_sp_fmt_len(xa(k), "")
    enddo
    
  end function str_real_sp_array_len


  function str_real_sp_array(xa) result(s)
    real(sp), dimension(:), intent(in) :: xa
    character(len=str_real_sp_array_len(xa)) :: s
    
    integer :: j, k, n

    n = 1
    do k = 1, size(xa) - 1
      j = str_real_sp_fmt_len(xa(k), "")
      s(n:n+j) = str(xa(k), "")//" "
      n = n + j + 1
    enddo
    s(n:) = str(xa(k))

  end function str_real_sp_array

 
  pure function str_real_sp_array_fmt_len(xa, fmt) result(n)
    real(sp), dimension(:), intent(in) :: xa
    character(len=*), intent(in) :: fmt
    integer :: n

    integer :: k

    n = size(xa) - 1
    do k = 1, size(xa)
      n = n + str_real_sp_fmt_len(xa(k), fmt)
    enddo
    
  end function str_real_sp_array_fmt_len
     

  function str_real_sp_array_fmt(xa, fmt, delimiter) result(s)
    real(sp), dimension(:), intent(in) :: xa
    character(len=*), intent(in) :: fmt
    character(len=1), intent(in), optional :: delimiter
    character(len=str_real_sp_array_fmt_len(xa, fmt)) :: s
    
    integer :: j, k, n
    character(len=1) :: d

    if (present(delimiter)) then
      d = delimiter
    else
      d = " "
    endif

    n = 1
    do k = 1, size(xa) - 1
      j = str_real_sp_fmt_len(xa(k), fmt)
      s(n:n+j) = str(xa(k), fmt)//d
      n = n + j + 1
    enddo
    s(n:) = str(xa(k), fmt)

  end function str_real_sp_array_fmt


  function str_real_sp_matrix(xa) result(s)
    real(sp), dimension(:,:), intent(in) :: xa
    character(len=str_real_sp_array_len(reshape(xa,(/size(xa)/)))) :: s

    s = str_real_sp_array(reshape(xa,(/size(xa)/)))
  end function str_real_sp_matrix
    

  function str_real_sp_matrix_fmt(xa, fmt) result(s)
    real(sp), dimension(:,:), intent(in) :: xa
    character(len=*), intent(in) :: fmt
    character(len=str_real_sp_array_fmt_len(reshape(xa,(/size(xa)/)),fmt)) :: s

    s = str_real_sp_array(reshape(xa,(/size(xa)/)))
  end function str_real_sp_matrix_fmt
     

  pure function real_dp_str(x, sig) result(s)
    real(dp), intent(in) :: x
    integer, intent(in) :: sig
    character(len=sig) :: s
    ! make a string of numbers sig long of x.
    integer :: e, i, j, k, n
    real(dp) :: x_

    if (sig < 1) then
      s ='' 
      return
    endif

    if (x == 0.0_dp) then
      e = 1
    else
      e = floor(log10(abs(x)))
    endif
    x_ = abs(x) / (10.0_dp**e)
    n = 1
    do k = sig - 2, 0, -1
      s(n:n) = digit(int(x_)+1:int(x_)+1)
      n = n + 1
      x_ = (x_ - int(x_)) * 10.0_dp
    enddo
    j = nint(x_)
    if (j == 10) then
      ! Now round ...
      s(n:n) = '9'
      i = verify(s, '9', .true.)
      if (i == 0) then
        s(1:1) = '!'
        !overflow
        return
      endif
      j = index(digit, s(i:i))
      s(i:i) = digit(j+1:j+1)
      s(i+1:) = repeat('0', sig - i + 1)
    else
      s(n:n) = digit(j+1:j+1)
    endif

  end function real_dp_str


  pure function str_real_dp_fmt_len(x, fmt) result(n)
    real(dp), intent(in) :: x
    character(len=*), intent(in) :: fmt
    integer :: n

    integer :: dec, sig
    integer :: e

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

    elseif (fmt(1:1) == 's') then
      if (len(fmt) > 1) then
        sig = str_to_int_10(fmt(2:))
      else
        sig = sig_dp
      endif
      sig = max(sig, 1)
      sig = min(sig, digits(1.0_dp))

      if (sig > 1) n = n + 1 
      ! for the decimal point
      
      n = n + sig + 1 + len(str(e))

    elseif (fmt(1:1) == 'r') then

      if (len(fmt) > 1) then
        dec = str_to_int_10(fmt(2:))
      else
        dec = sig_dp - e - 1
      endif
      dec = max(dec, 0)
      dec = min(dec, digits(1.0_dp)-e)

      if (dec > 0) n = n + 1
      if (abs(x) >= 1.0_dp) n = n + 1

      ! Need to know if there's an overflow ....
      if (e+dec+1 > 0) then
        if (index(real_dp_str(abs(x), e+dec+1), '!') == 1) &
             e = e + 1
      endif

      n = n + abs(e) + dec

    else
      n = pure_pxfabort()
    endif

  end function str_real_dp_fmt_len


  function str_real_dp_fmt(x, fmt) result(s)
    real(dp), intent(in) :: x
    character(len=*), intent(in) :: fmt
    character(len=str_real_dp_fmt_len(x, fmt)) :: s

    integer :: sig, dec
    integer :: e, n
    character(len=str_real_dp_fmt_len(x, fmt)) :: num !this will always be enough memory.

    if (.not.checkFmt(fmt)) &
      call FoX_error("Invalid format: "//fmt)

    if (x == 0.0_dp) then
      e = 0
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
      if (num(1:1) == '!') then
        e = e + 1
        num = '1'//repeat('0',len(num)-1)
      endif

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

    elseif (fmt(1:1) == 's') then

      if (len(fmt) > 1) then
        sig = str_to_int_10(fmt(2:))
      else
        sig = sig_dp
      endif
      sig = max(sig, 1)
      sig = min(sig, digits(1.0_dp))

      num = real_dp_str(abs(x), sig)
      if (num(1:1) == '!') then
        e = e + 1
        num = '1'//repeat('0',len(num)-1)
      endif

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

    elseif (fmt(1:1) == 'r') then

      if (len(fmt) > 1) then
        dec = str_to_int_10(fmt(2:))
      else
        dec = sig_dp - e - 1
      endif
      dec = max(dec, 0)
      dec = min(dec, digits(1.0_dp)-e-1)

      if (e+dec+1 > 0) then
        num = real_dp_str(abs(x), e+dec+1)
      else
        num = ''
      endif
      if (num(1:1) == '!') then
        e = e + 1
        num = '1'//repeat('0',len(num)-1)
      endif

      if (abs(x) >= 1.0_dp) then
        s(n:n+e) = num(:e+1)
        n = n + e + 1
        if (dec > 0) then
          s(n:n) = '.'
          n = n + 1
          s(n:) = num(e+2:)
        endif
      else
        s(n:n) = '0'
        if (dec > 0) then
          s(n+1:n+1) = '.'
          n = n + 2
          if (dec < -e-1) then
            s(n:) = repeat('0', dec)
          else
            s(n:n-e-2) = repeat('0', max(-e-1,0))
            n = n - min(e,-1) - 1
            if (n <= len(s)) then
              s(n:) = num
            endif
          endif
        endif
      endif

    endif

  end function str_real_dp_fmt


  pure function str_real_dp_len(x) result(n)
    real(dp), intent(in) :: x
    integer :: n

    n = str_real_dp_fmt_len(x, "")

  end function str_real_dp_len


  function str_real_dp(x) result(s)
    real(dp), intent(in) :: x
    character(len=str_real_dp_len(x)) :: s

    s = str_real_dp_fmt(x, "")

  end function str_real_dp


  function str_real_dp_array(xa) result(s)
    real(dp), dimension(:), intent(in) :: xa
    character(len=str_real_dp_array_len(xa)) :: s
    
    integer :: j, k, n

    n = 1
    do k = 1, size(xa) - 1
      j = str_real_dp_fmt_len(xa(k), "")
      s(n:n+j) = str(xa(k), "")//" "
      n = n + j + 1
    enddo
    s(n:) = str(xa(k))

  end function str_real_dp_array

     
  pure function str_real_dp_array_len(xa) result(n)
    real(dp), dimension(:), intent(in) :: xa
    integer :: n

    integer :: k

    n = size(xa) - 1
    do k = 1, size(xa)
      n = n + str_real_dp_fmt_len(xa(k), "")
    enddo
    
  end function str_real_dp_array_len

     
  pure function str_real_dp_array_fmt_len(xa, fmt) result(n)
    real(dp), dimension(:), intent(in) :: xa
    character(len=*), intent(in) :: fmt
    integer :: n

    integer :: k

    n = size(xa) - 1
    do k = 1, size(xa)
      n = n + str_real_dp_fmt_len(xa(k), fmt)
    enddo
    
  end function str_real_dp_array_fmt_len


  function str_real_dp_array_fmt(xa, fmt, delimiter) result(s)
    real(dp), dimension(:), intent(in) :: xa
    character(len=*), intent(in) :: fmt
    character(len=1), intent(in), optional :: delimiter
    character(len=str_real_dp_array_fmt_len(xa, fmt)) :: s
    
    integer :: j, k, n
    character(len=1) :: d

    if (present(delimiter)) then
      d = delimiter
    else
      d = " "
    endif

    n = 1
    do k = 1, size(xa) - 1
      j = str_real_dp_fmt_len(xa(k), fmt)
      s(n:n+j) = str(xa(k), fmt)//d
      n = n + j + 1
    enddo
    s(n:) = str(xa(k), fmt)

  end function str_real_dp_array_fmt


  function str_real_dp_matrix(xa) result(s)
    real(dp), dimension(:,:), intent(in) :: xa
    character(len=str_real_dp_array_len(reshape(xa,(/size(xa)/)))) :: s

    s = str_real_dp_array(reshape(xa,(/size(xa)/)))
  end function str_real_dp_matrix
    

  function str_real_dp_matrix_fmt(xa, fmt) result(s)
    real(dp), dimension(:,:), intent(in) :: xa
    character(len=*), intent(in) :: fmt
    character(len=str_real_dp_array_fmt_len(reshape(xa,(/size(xa)/)),fmt)) :: s

    s = str_real_dp_array(reshape(xa,(/size(xa)/)))
  end function str_real_dp_matrix_fmt

 
  pure function checkFmt(fmt) result(good)
    character(len=*), intent(in) :: fmt
    logical :: good

    ! should be ([rs][0-9]*)?

    if (len(fmt) > 0) then
      if (fmt(1:1) == 'r' .or. fmt(1:1) == 's') then
        if (len(fmt) > 1) then
          good = (verify(fmt(2:), digit) == 0)
        else
          good = .true.
        endif
      else
        good = .false.
      endif
    else
      good = .true.
    endif
  end function checkFmt
     
end module m_common_format

module m_common_format

  use pxf, only: pxfabort

  implicit none
  private

  character(len=*), parameter :: digits = "0123456789"
  character(len=*), parameter :: hexdigits = "0123456789abcdefABCDEF"

  public :: str_to_int_10
  public :: str_to_int_16

contains

  pure function str_to_int_10(str) result(n)
    character(len=*), intent(in) :: str
    integer :: n

    integer :: max_power, i, j

    if (verify(str, digits) > 0) call pxfabort()

    max_power = len(str) - 1


    n = 0
    do i = 0, max_power
      j = max_power - i + 1
      n = n + (index(digits, str(j:j)) - 1) * 10**i
    enddo

  end function str_to_int_10


  pure function str_to_int_16(str) result(n)
    character(len=*), intent(in) :: str
    integer :: n
    
    character(len=len(str)) :: str_l
    integer :: max_power, i, j

    if (verify(str, hexdigits) == 0) then
       str_l = to_lower(str)
    else
      call pxfabort()
    endif

    max_power = len(str) - 1

    n = 0
    do i = 0, max_power
      j = max_power - i + 1
      n = n + (index(hexdigits, str_l(j:j)) - 1) * 10**i
    enddo

  contains
    pure function to_lower(s) result(s2)
      character(len=*), intent(in) :: s
      character(len=len(s)) :: s2
      character(len=12), parameter :: hex = "ABCDEFabcdef"
      integer :: j, k
      do j = 1, len(s)
        if (verify(s(j:j), digits) == 0) then
          s2(j:j) = s(j:j)
        else
          k = index(s(j:j), hex)+6
          s2(j:j) = hex(k:k)
        endif
      enddo
    end function to_lower
         
  end function str_to_int_16
          
end module m_common_format

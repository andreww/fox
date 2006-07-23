module m_wxml_escape

!Assorted functions to do with escaping strings and checking
!strings are compliant with XML naming policy. Should be in
!common

  implicit none
  private

  public :: escape_string
  public :: escape_string_len

contains

  ! This approach might want to tie in with entities. Not sure. If
  ! so, then this approach won't work.
  pure function escape_string_len(str) result(c)
    character(len=*), intent(in) :: str
    integer :: c

    integer :: i 
    c = len(str)
    do i = 1, len(str)
      select case(str(i:i))
      case ('&')
        c = c + 4
      case ('<')
        c = c + 3
      case ('"')
        c = c + 5
      case ("'")
        c = c + 5
      end select
    enddo 

  end function escape_string_len
    

  function escape_string(str) result (str2)
    character(len=*), intent(in) :: str
    character(len=escape_string_len(str)) :: str2

    integer :: c, i

    c = 1
    do i = 1, len(str)
      select case(str(i:i))
      case ('&')
        str2(c:c+4) = "&amp;"
        c = c + 5
      case ('<')
        str2(c:c+3) = "&lt;"
        c = c + 4
      case ('"')
        str2(c:c+5) = "&quot;"
        c = c + 6
      case ("'")
        str2(c:c+5) = "&apos;"
        c = c + 6
      case default
        str2(c:c) = str(i:i)
        c = c + 1
      end select
    enddo 

  end function escape_string

end module m_wxml_escape

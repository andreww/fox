module m_wxml_escape
  
  !Ensure all characters are safe to go out into XML file.

  use m_common_error, only: FoX_error, FoX_warning
  use m_common_format, only: str

  implicit none
  private

  integer, parameter :: AMP = iachar('&')
  integer, parameter :: LT = iachar('<')
  integer, parameter :: QUOT = iachar('"')
  integer, parameter :: APOS = iachar("'")

  public :: escape_string
  public :: escape_string_len

contains

  pure function escape_string_len(s) result(c)
    character(len=*), intent(in) :: s
    integer :: c

    integer :: i 
    c = len(s)
    do i = 1, len(s)
      select case(iachar(s(i:i)))
      case (AMP)
        c = c + 4
      case (LT)
        c = c + 3
      case (QUOT)
        c = c + 5
      case (APOS)
        c = c + 5
      case (1:9)
        c = c + 3
      case (10:31)
        c = c + 4
      case (127:)
        c = c + 5
        ! a char can never contain more than 8 bits = 256 characters, so
        ! we never need more than 3 chars to represent the int.
      end select
    enddo 

  end function escape_string_len
    

  function escape_string(s) result (s2)
    character(len=*), intent(in) :: s
    character(len=escape_string_len(s)) :: s2

    integer :: c, i

    ! We have to do it this way (with achar etc) in case the native
    ! platform encoding is not ASCII

    c = 1
    do i = 1, len(s)
      select case (iachar(s(i:i)))
      case (0)
        call FoX_error("Tried to output a NUL character")
      case (1:9)
        s2(c:c+3) = "&#"//str(iachar(s(i:i)))//";"
        c = c + 4
      case (10:31)
        s2(c:c+4) = "&#"//str(iachar(s(i:i)))//";"
        c = c + 5
      case (32:126)
        select case (iachar(s(i:i)))
        case (AMP)
          s2(c:c+4) = "&amp;"
          c = c + 5
        case (LT)
          s2(c:c+3) = "&lt;"
          c = c + 4
        case (QUOT)
          s2(c:c+5) = "&quot;"
          c = c + 6
        case (APOS)
          s2(c:c+5) = "&apos;"
          c = c + 6
        case default
          s2(c:c) = achar(iachar(s(i:i)))
          c = c + 1
        end select
      case (127)
        s2(c:c+5) = "&#127;"
        c = c + 6
      case default
        !TOHW we should maybe just disallow this ...
        call FoX_warning("emitting non-ASCII character. Platform-dependent result!")
        s2(c:c+6) = "&#"//str(iachar(s(i:i)))//";"
        c = c + 6
        ! a char can never contain more than 8 bits = 256 characters, so
        ! we never need more than 3 chars to represent the int.
        ! We have to encode it though, because UTF-8 x7F-x9F must be 
        ! encoded, and if they are in the native charset they'll be > 128
      end select
    enddo 

  end function escape_string

end module m_wxml_escape

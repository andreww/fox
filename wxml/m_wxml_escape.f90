module m_wxml_escape

!Assorted functions to do with escaping strings and checking
!strings are compliant with XML naming policy. Should be in
!common

  use m_common_error, only : FoX_warning
  implicit none
  private

  public :: check_Name
  public :: escape_string

  character(len=*), parameter :: LETTER = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
  character(len=*), parameter :: DIGIT  = '1234567890'
  character(len=*), parameter :: ESCAPE = '&<'//'"'//"'"

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

    c = 0
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

  function check_Name(name) result(good)
    character(len=*), intent(in) :: name
    logical :: good
    ! Validates a string against the XML requirements for a NAME
    ! Is not fully compliant; ignores UTF issues.

    integer :: n, i

    good=.true.

    n = len(name)
    if (n == 0) then
      call FoX_warning("tagName is an empty string.")
      good=.false.
    elseif (good) then
      if (scan(name(1:1), LETTER//'_'//':') == 0) then
        call FoX_warning("tagName must begin with a letter, underscore or colon: '"//name(1:1)//"'.")
        good=.false.
      endif
    elseif (good) then
      do i = 1, n
        if (scan(name(i:i), LETTER//DIGIT//'.'//'-'//'_'//':') == 0) then
          call FoX_warning("tagName contains a forbidden character: '"//name(i:i)//"'.")
          good=.false.
          exit
        endif
      enddo
    elseif (good) then
      if (scan(name(1:1), 'Xx') == 1 .and. &
          scan(name(2:2), 'Mm') == 1 .and. &
          scan(name(3:3), 'Ll') == 1) then
        call FoX_warning("tagName cannot start with the characters 'XML'.")
        good=.false.
      endif
    endif
       
  end function check_Name

end module m_wxml_escape

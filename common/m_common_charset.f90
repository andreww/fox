module m_common_charset

  ! Written to use ASCII charset only. Full UNICODE would
  ! take much more work and need a proper unicode library.

  implicit none
  private

  interface operator(.in.)
    module procedure belongs
  end interface

  character(len=1), parameter :: SPACE           = achar(32)
  character(len=1), parameter :: NEWLINE         = achar(10)
  character(len=1), parameter :: CARRIAGE_RETURN = achar(13)
  character(len=1), parameter :: TAB             = achar(9)

  character(len=*), parameter :: whitespace = SPACE//NEWLINE//CARRIAGE_RETURN//TAB

  character(len=*), parameter :: lowerCase = "abcdefghijklmnopqrstuvwxyz"
  character(len=*), parameter :: upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=*), parameter :: digits = "0123456789"
  character(len=*), parameter :: NameChars = lowerCase//upperCase//digits//".-_:"
  character(len=*), parameter :: InitialNameChars = lowerCase//upperCase//"-_:"

  character(len=*), parameter :: PubIdChars = NameChars//whitespace//"'()+,/=?;!*#@$%"
  character(len=*), parameter :: validchars = &
       whitespace//"!""#$%&'()*+,-./"//digits// &
       ":;<=>?@"//upperCase//"[\]^_`"//lowerCase//"{|}~"
  ! these are all the standard ASCII printable characters: whitespace + (33-126)
  ! which are the only characters we can guarantee to know how to handle properly.

  public :: operator(.in.)

  public :: InitialNameChars
  public :: NameChars
  public :: validchars
  public :: whitespace
  public :: uppercase

  public :: digits

contains !==========================================================

  function belongs(c,charset)  result(res)
    character(len=1), intent(in)  :: c
    character(len=*), intent(in)  :: charset
    logical                       :: res

    res = (verify(c, charset) == 0)

  end function belongs

end module m_common_charset










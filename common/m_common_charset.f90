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
  character(len=*), parameter :: hexdigits = "0123456789abcdefABCDEF"
  character(len=*), parameter :: NameChars = lowerCase//upperCase//digits//".-_:"
  character(len=*), parameter :: InitialNameChars = lowerCase//upperCase//"_:"

  character(len=*), parameter :: PubIdChars = NameChars//whitespace//"'()+,/=?;!*#@$%"
  character(len=*), parameter :: validchars = &
       whitespace//"!""#$%&'()*+,-./"//digits// &
       ":;<=>?@"//upperCase//"[\]^_`"//lowerCase//"{|}~"
  ! these are all the standard ASCII printable characters: whitespace + (33-126)
  ! which are the only characters we can guarantee to know how to handle properly.

  integer, parameter :: XML1_0 = 10
  integer, parameter :: XML1_1 = 11

  character(len=*), parameter :: XML1_0_NAMECHARS = NameChars
  character(len=*), parameter :: XML1_1_NAMECHARS = NameChars

  character(len=*), parameter :: XML1_0_INITIALNAMECHARS = InitialNameChars
  character(len=*), parameter :: XML1_1_INITIALNAMECHARS = InitialNameChars

  character(len=*), parameter :: XML_WHITESPACE = whitespace
  character(len=*), parameter :: XML_INITIALENCODINGCHARS = lowerCase//upperCase
  character(len=*), parameter :: XML_ENCODINGCHARS = lowerCase//upperCase//digits//'._-'

  public :: operator(.in.)

  public :: validchars
  public :: whitespace
  public :: uppercase

  public :: digits
  public :: hexdigits

  public :: XML1_0
  public :: XML1_1
  public :: XML1_0_NAMECHARS
  public :: XML1_1_NAMECHARS
  public :: XML1_0_INITIALNAMECHARS
  public :: XML1_1_INITIALNAMECHARS
  public :: XML_WHITESPACE
  public :: XML_INITIALENCODINGCHARS
  public :: XML_ENCODINGCHARS

  public :: isInitialNameChar
  public :: isNameChar
  public :: isXML1_0_NameChar
  public :: isXML1_1_NameChar

contains

  function belongs(c, str) result(p)
    character, intent(in) :: c
    character(len=*), intent(in) :: str
    logical :: p
    p = (verify(c, str)==0)
  end function belongs

  function isInitialNameChar(c, xml_version) result(p)
    character, intent(in) :: c
    integer, intent(in) :: xml_version
    logical :: p

    select case(xml_version)
      case (XML1_0)
        p = (verify(c, XML1_0_INITIALNAMECHARS)==0)
      case (XML1_1)
        p = (verify(c, XML1_1_INITIALNAMECHARS)==0)
      end select

    end function isInitialNameChar

  function isNameChar(c, xml_version) result(p)
    character, intent(in) :: c
    integer, intent(in) :: xml_version
    logical :: p

    select case(xml_version)
      case (XML1_0)
        p = (verify(c, XML1_0_NAMECHARS)==0)
      case (XML1_1)
        p = (verify(c, XML1_1_NAMECHARS)==0)
      end select

    end function isNameChar

    function isXML1_0_NameChar(c) result(p)
      character, intent(in) :: c
      logical :: p
    
      p = (verify(c, XML1_0_NAMECHARS)==0)
      
    end function isXML1_0_NameChar

    function isXML1_1_NameChar(c) result(p)
    character, intent(in) :: c
    logical :: p

    p = (verify(c, XML1_1_NAMECHARS)==0)

  end function isXML1_1_NameChar

end module m_common_charset










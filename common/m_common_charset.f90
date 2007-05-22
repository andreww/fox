module m_common_charset

  ! Written to use ASCII charset only. Full UNICODE would
  ! take much more work and need a proper unicode library.

  implicit none
  private

  interface operator(.in.)
    module procedure belongs
  end interface

!!$  character(len=1), parameter :: ASCII = &
!!$achar(0)//achar(1)//achar(2)//achar(3)//achar(4)//achar(5)//achar(6)//achar(7)//achar(8)//achar(9)//&
!!$achar(10)//achar(11)//achar(12)//achar(13)//achar(14)//achar(15)//achar(16)//achar(17)//achar(18)//achar(19)//&
!!$achar(20)//achar(21)//achar(22)//achar(23)//achar(24)//achar(25)//achar(26)//achar(27)//achar(28)//achar(29)//&
!!$achar(30)//achar(31)//achar(32)//achar(33)//achar(34)//achar(35)//achar(36)//achar(37)//achar(38)//achar(39)//&
!!$achar(40)//achar(41)//achar(42)//achar(43)//achar(44)//achar(45)//achar(46)//achar(47)//achar(48)//achar(49)//&
!!$achar(50)//achar(51)//achar(52)//achar(53)//achar(54)//achar(55)//achar(56)//achar(57)//achar(58)//achar(59)//&
!!$achar(60)//achar(61)//achar(62)//achar(63)//achar(64)//achar(65)//achar(66)//achar(67)//achar(68)//achar(69)//&
!!$achar(70)//achar(71)//achar(72)//achar(73)//achar(74)//achar(75)//achar(76)//achar(77)//achar(78)//achar(79)//&
!!$achar(80)//achar(81)//achar(82)//achar(83)//achar(84)//achar(85)//achar(86)//achar(87)//achar(88)//achar(89)//&
!!$achar(90)//achar(91)//achar(92)//achar(93)//achar(94)//achar(95)//achar(96)//achar(97)//achar(98)//achar(99)//&
!!$achar(100)//achar(101)//achar(102)//achar(103)//achar(104)//achar(105)//achar(106)//achar(107)//achar(108)//achar(109)//&
!!$achar(110)//achar(111)//achar(112)//achar(113)//achar(114)//achar(115)//achar(116)//achar(117)//achar(118)//achar(119)//&
!!$achar(120)//achar(121)//achar(122)//achar(123)//achar(124)//achar(125)//achar(126)//achar(127)

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

  integer, parameter :: XML1_0 = 10      ! NB 0x7F was legal in XML-1.0, but illegal in XML-1.1

  integer, parameter :: XML1_1 = 11

  character(len=*), parameter :: XML1_0_ILLEGALCHARS = achar(0)
  character(len=*), parameter :: XML1_1_ILLEGALCHARS = NameChars

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

  public :: isLegalChar
  public :: isLegalCharRef
  public :: isInitialNameChar
  public :: isNameChar
  public :: isInitialNCNameChar
  public :: isNCNameChar
  public :: isXML1_0_NameChar
  public :: isXML1_1_NameChar
  public :: checkChars

contains

  function belongs(c, str) result(p)
    character, intent(in) :: c
    character(len=*), intent(in) :: str
    logical :: p
    p = (verify(c, str)==0)
  end function belongs

  pure function isLegalChar(c, xml_version) result(p)
    character, intent(in) :: c
    integer, intent(in) :: xml_version
    logical :: p 
    ! Is this character legal as a source character in the document?
    integer :: i
    i = iachar(c)
    if (i<0.or.i>127) then
      p = .false. ! FIXME but it's processor dependent ...
    endif
    select case(xml_version)
    case (XML1_0)
      p = (i==9.or.i==10.or.i==13.or.(i>31.and.i<127))
    case (XML1_1)
      p = (i==9.or.i==10.or.i==13.or.(i>31.and.i<128))
      ! NB 0x7F was legal in XML-1.0, but illegal in XML-1.1
    end select
  end function isLegalChar

  pure function isLegalCharRef(i, xml_version) result(p)
    integer, intent(in) :: i
    integer, intent(in) :: xml_version
    logical :: p 

    ! Is ASCII character #i legal as a character reference?

    if (xml_version==XML1_0) then
      p = (i==9).or.(i==10).or.(i==13).or.(i>31.and.i<55296).or.(i>57343.and.i<65534).or.(i>65535.and.i<1114112)
    elseif (xml_version==XML1_1) then
      p = (i>0.and.i<55296).or.(i>57343.and.i<65534).or.(i>65535.and.i<1114112)
      ! XML 1.1 made all control characters legal as character references.
    end if
  end function isLegalCharRef

  pure function isInitialNameChar(c, xml_version) result(p)
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

  pure function isNameChar(c, xml_version) result(p)
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

  pure function isInitialNCNameChar(c, xml_version) result(p)
    character, intent(in) :: c
    integer, intent(in) :: xml_version
    logical :: p

    if (c==":") then
      p = .false.
    else
      select case(xml_version)
      case (XML1_0)
        p = (verify(c, XML1_0_INITIALNAMECHARS)==0)
      case (XML1_1)
        p = (verify(c, XML1_1_INITIALNAMECHARS)==0)
      end select
    endif
  end function isInitialNCNameChar

  pure function isNCNameChar(c, xml_version) result(p)
    character, intent(in) :: c
    integer, intent(in) :: xml_version
    logical :: p

    if (c==":") then
      p = .false.
    else
      select case(xml_version)
      case (XML1_0)
        p = (verify(c, XML1_0_NAMECHARS)==0)
      case (XML1_1)
        p = (verify(c, XML1_1_NAMECHARS)==0)
      end select
    endif
  end function isNCNameChar

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

  pure function checkChars(value, xv) result(p)
    character(len=*), intent(in) :: value
    integer, intent(in) :: xv
    logical :: p

    ! This checks if value only contains values
    ! legal to appear (escaped or unescaped) 
    ! according to whichever XML version is in force.
    integer :: i

    p = .true.
    do i = 1, len(value)
      if (xv == XML1_0) then
        select case(iachar(value(i:i)))
        case (0,8)
          p = .false.
        case (11,12)
          p = .false.
        end select
      else
        if (iachar(value(i:i))==0) p =.false.
      endif
    enddo
  end function checkChars

end module m_common_charset










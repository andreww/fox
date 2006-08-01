module m_common_namecheck

  use m_common_format, only: str_to_int_10, str_to_int_16

  implicit none
  private

  character(len=*), parameter :: spaces = " "//achar(9)//achar(10)//achar(13)
  character(len=*), parameter :: lowerCase = "abcdefghijklmnopqrstuvwxyz"
  character(len=*), parameter :: upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=*), parameter :: letters = lowerCase//upperCase
  character(len=*), parameter :: digits = "0123456789"
  character(len=*), parameter :: hexdigits = "0123456789abcdefABCDEF"
  character(len=*), parameter :: NameChars = lowerCase//upperCase//digits//".-_:"
  character(len=*), parameter :: NCNameChars = lowerCase//upperCase//digits//".-_"
  character(len=*), parameter :: PubIdChars = NameChars//spaces//"'()+,/=?;!*#@$%"

  public :: checkName
  public :: checkQName
  public :: checkNCName
  public :: checkEncName
  public :: checkPITarget
  public :: checkSystemId
  public :: checkPubId
  public :: checkIRI
  public :: checkCharacterEntityReference
  public :: checkEntityValue

  public :: prefixOfQName
  public :: localpartOfQName

contains

  pure function checkEncName(name) result(good)
    ![81]   	EncName	   ::=   	[A-Za-z] ([A-Za-z0-9._] | '-')*
    character(len=*), intent(in) :: name
    logical :: good

    integer :: n
    
    n = len(name)
    good = (n > 0)
    if (good) good = (scan(name(1:1), letters) /= 0)
    if (good .and. n > 1) &
         good = (verify(name(2:), NCNameChars) == 0)
  end function checkEncName


  pure function checkPITarget(name) result(good)
    character(len=*), intent(in) :: name
    logical :: good
    ! Validates a string against the XML requirements for a NAME
    ! Is not fully compliant; ignores UTF issues.

    integer :: n

    n = len(name)
    good = (n > 0)
    if (good) good = (scan(name(1:1), letters//'_'//':') /= 0) 
    if (good .and. n > 1) &
             good = (verify(name(2:), NameChars) == 0) 
    if (good .and. n > 2) then
      good = (.not.(scan(name(1:1), 'Xx') == 1 .and. &
                    scan(name(2:2), 'Mm') == 1 .and. &
                    scan(name(3:3), 'Ll') == 1))
    endif
       
  end function checkPITarget


  pure function checkName(name) result(good)
    character(len=*), intent(in) :: name
    logical :: good
    ! Validates a string against the XML requirements for a NAME
    ! Is not fully compliant; ignores UTF issues.

    integer :: n

    n = len(name)
    good = (n > 0)
    if (good) good = (scan(name(1:1), letters//'_'//':') /= 0) 
    if (good .and. n > 1) &
             good = (verify(name(2:), NameChars) == 0) 
       
  end function checkName


  pure function checkQName(name) result(good)
    character(len=*), intent(in) :: name
    logical :: good
    ! Validates a string against the XML requirements for a NAME
    ! Is not fully compliant; ignores UTF issues.

    integer :: n

    n = index(name, ':')
    if (n == 0) then
      good = checkNCName(name)
    else
      good = (checkNCName(name(:n-1)) .and. checkNCName(name(n+1:)))
    endif
  end function checkQName


  pure function checkNCName(name) result(good)
    character(len=*), intent(in) :: name
    logical :: good
    ! Validates a string against the XML requirements for an NCNAME
    ! Is not fully compliant; ignores UTF issues.

    integer :: n

    n = len(name)
    good = (n > 0)
    if (good) good = (scan(name(1:1), letters//'_') /= 0) 
    if (good .and. n > 1) &
             good = (verify(name(2:), NCNameChars) == 0) 
       
  end function checkNCName


  pure function checkPubId(PubId) result(good)
    character(len=*), intent(in) :: PubId
    logical :: good
    good =  (verify(PubId, PubIdChars) == 0) 
  end function checkPubId


  pure function checkSystemId(SystemId) result(good)
    character(len=*), intent(in) :: SystemId
    logical :: good
    if (index(SystemId, '"') > 0) then
      good = (index(SystemId, "'") > 0)
    elseif (index(SystemId, "'") > 0) then
      good = (index(SystemId, '"') > 0)
    else
      good = .true.
    end if
  end function checkSystemId
    
  function checkIRI(IRI) result(good)
    character(len=*), intent(in) :: IRI
    logical :: good
    !By [Namespaces] section 9, there is no
    ! formal definition of IRI's yet.
    good = .true.
  end function checkIRI

  pure function checkCharacterEntityReference(code) result(good)
    character(len=*), intent(in) :: code
    logical :: good

    ! This is XML-1.1 compliant (not 1.0), character range according to:
    !
    ![2] Char ::= [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
    ![66] CharRef ::= '&#' [0-9]+ ';'
    !               | '&#x' [0-9a-fA-F]+ ';' [WFC: Legal Character]o
    ! Well-formedness constraint: Legal Character
    ! Characters referred to using character references MUST match the production for Char [2].

    integer :: i

    good = .false.
    if (len(code) > 0) then
      if (code(1:1) == "#") then
        if (code(2:2) == "x") then
          if (len(code) > 2) then
            good = (verify(code(3:), hexdigits) == 0)
            if (good) then
              i = str_to_int_16(code(3:))
            endif
          endif
        else
          good = (verify(code(2:), digits) == 0)
          if (good) then
            i = str_to_int_10(code(3:))
          endif
        endif
      endif
    endif
    if (good) &
      good = ((0<i .and. i<55296) &
      .or.(57343<i .and. i<65534) &
      .or.(65535<i .and. i<4177778))

  end function checkCharacterEntityReference

  
  pure function checkEntityValue(value) result (good)
    ![9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
    !                 |  "'" ([^%&'] | PEReference | Reference)* "'"
    character(len=*), intent(in) :: value
    logical :: good
    ! this is a very imperfect check and should really be in
    ! m_common_entities
    good = (len(value) > 0)
    if (good) then
      if (index(value, '"') > 0) then
        good = (index(value, "'") == 0)
      elseif (index(value, "'") > 0) then
        good = (index(value, '"') == 0)
      endif
    endif

  end function checkEntityValue


  pure function prefixOfQName(qname) result(prefix)
    character(len=*), intent(in) :: qname
    character(len=max(index(qname, ':')-1,0)) :: prefix

    prefix = qname ! automatic truncation
  end function prefixOfQName

  
  pure function localpartOfQname(qname) result(localpart)
    character(len=*), intent(in) :: qname
    character(len=max(len(qname)-index(qname,':'),0)) ::localpart

    localpart = qname(index(qname,':')+1:)
  end function localpartOfQname

end module m_common_namecheck

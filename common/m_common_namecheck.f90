module m_common_namecheck

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
  public :: checkNCName
  public :: checkEncName
  public :: checkPITarget
  public :: checkPubId
  public :: checkCharacterEntityReference
  public :: checkEntityValue

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
    good =  (verify(PubId, PubIdChars) /= 0) 
  end function checkPubId

  !function checkURI
  !end function checkURI

  pure function checkCharacterEntityReference(code) result(good)
    character(len=*), intent(in) :: code
    logical :: good

    good = .false.
    if (len(code) > 0) then
      if (code(1:1) == "#") then
        if (code(2:2) == "x") then
          if (len(code) > 2) then
            good = (verify(code(3:), hexdigits) == 0)
          endif
        else
          good = (verify(code(2:), digits) == 0)
        endif
      endif
    endif
  end function checkCharacterEntityReference

  
  pure function checkEntityValue(value) result (good)
    ![9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
    !                 |  "'" ([^%&'] | PEReference | Reference)* "'"
    character(len=*), intent(in) :: value
    logical :: good
    ! this is a very imperfect check
    good = (len(value) > 0)
    if (good) then
      if (index(value, '"') > 0) then
        good = (index(value, "'") == 0)
      elseif (index(value, "'") > 0) then
        good = (index(value, '"') == 0)
      endif
    endif

  end function checkEntityValue

end module m_common_namecheck

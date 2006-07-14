module m_common_namecheck

  implicit none
  private

  character(len=*), parameter :: spaces = " "//achar(9)//achar(10)//achar(13)
  character(len=*), parameter :: lowerCase = "abcdefghijklmnopqrstuvwxyz"
  character(len=*), parameter :: upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=*), parameter :: letters = lowerCase//upperCase
  character(len=*), parameter :: digits = "0123456789"
  character(len=*), parameter :: NameChars = lowerCase//upperCase//digits//".-_:"
  character(len=*), parameter :: QNameChars = lowerCase//upperCase//digits//".-_"
  character(len=*), parameter :: PubIdChars = NameChars//spaces//"'()+,/=?;!*#@$%"

  public :: checkName
  public :: checkEncName
  public :: checkPITarget
  public :: checkPubId

contains

  function checkEncName(name) result(good)
    ![81]   	EncName	   ::=   	[A-Za-z] ([A-Za-z0-9._] | '-')*
    character(len=*), intent(in) :: name
    logical :: good

    integer :: n
    
    n = len(name)
    good = (n > 0)
    if (good) good = (scan(name(1:1), letters) /= 0)
    if (good .and. n > 1) &
         good = (verify(name(2:), QNameChars) == 0)
  end function checkEncName


  function checkPITarget(name) result(good)
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


  function checkName(name) result(good)
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

  !function checkQName

  !end function checkQName

  function checkPubId(PubId) result(good)
    character(len=*), intent(in) :: PubId
    logical :: good
    good =  (verify(PubId, PubIdChars) /= 0) 
  end function checkPubId

  !function checkURI

  !end function checkURI

end module m_common_namecheck

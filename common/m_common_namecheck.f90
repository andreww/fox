module m_common_namecheck

  ! These are basically a collection of what would be regular
  ! expressions in a more sensible language.
  ! The only external dependency should be knowing how these
  ! regular expressions may differ between XML-1.0 and 1.1 (which
  ! is only in the areas of
  ! 1: allowing character entity references to control characters
  ! 2: More characters allowed in Names (but this only affects
  !    unicode-aware programs, so is only skeleton here)

  use m_common_charset, only: isLegalCharRef, isNCNameChar, &
    isInitialNCNameChar, isInitialNameChar, isNameChar
  use m_common_format, only: str_to_int_10, str_to_int_16, operator(//)
  use m_common_struct, only: xml_doc_state

  implicit none
  private

  character(len=*), parameter :: lowerCase = "abcdefghijklmnopqrstuvwxyz"
  character(len=*), parameter :: upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=*), parameter :: letters = lowerCase//upperCase
  character(len=*), parameter :: digits = "0123456789"
  character(len=*), parameter :: hexdigits = "0123456789abcdefABCDEF"
  character(len=*), parameter :: NameChars = lowerCase//upperCase//digits//".-_:"

  public :: checkName
  public :: checkQName
  public :: checkNCName
  public :: checkEncName
  public :: checkPITarget
  public :: checkPublicId
  public :: checkSystemId
  public :: checkIRI
  public :: checkPEDef
  public :: checkPseudoAttValue
  public :: checkAttValue
  public :: checkCharacterEntityReference
  public :: likeCharacterEntityReference

  public :: prefixOfQName
  public :: localpartOfQName

  public :: resolveSystemId

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
         good = (verify(name(2:), letters//digits//'.-_') == 0)
  end function checkEncName


  function checkPITarget(name, xds) result(good)
    character(len=*), intent(in) :: name
    type(xml_doc_state), intent(in) :: xds
    logical :: good
    ! Validates a string against the XML requirements for a NAME
    ! Is not fully compliant; ignores UTF issues.

    good = checkName(name, xds)
    if (good .and. len(name) > 2) then
      good = (scan(name(1:1), 'Xx') == 0 .and. &
        scan(name(2:2), 'Mm') == 0 .and. &
        scan(name(3:3), 'Ll') == 0)
    endif
       
  end function checkPITarget


  pure function checkName(name, xds) result(good)
    character(len=*), intent(in) :: name
    type(xml_doc_state), intent(in) :: xds
    logical :: good
    ! Validates a string against the XML requirements for a NAME
    ! Is not fully compliant; ignores UTF issues.

    integer :: i

    good = (len(name) > 0)
    if (good) good = isInitialNameChar(name(1:1), xds%xml_version) 
    do i = 1, len(name)
      if (.not.isNameChar(name(i:i), xds%xml_version)) then
        good = .false.
        exit
      endif
    enddo
       
  end function checkName


  pure function checkQName(name, xds) result(good)
    character(len=*), intent(in) :: name
    type(xml_doc_state), intent(in) :: xds
    logical :: good
    ! Validates a string against the XML requirements for a NAME
    ! Is not fully compliant; ignores UTF issues.

    integer :: n

    n = index(name, ':')
    if (n == 0) then
      good = checkNCName(name, xds%xml_version)
    else
      good = (checkNCName(name(:n-1), xds%xml_version) .and. checkNCName(name(n+1:), xds%xml_version))
    endif
  end function checkQName


  pure function checkNCName(name, xv) result(good)
    character(len=*), intent(in) :: name
    integer, intent(in) :: xv
    logical :: good
    ! Validates a string against the XML requirements for an NCNAME
    ! Is not fully compliant; ignores UTF issues.

    integer :: i

    good = .false.
    if (len(name)>0) then
      if (.not.isInitialNCNameChar(name(1:1), xv)) return
      do i = 2, len(name)
        if (.not.isNCNameChar(name(i:i), xv)) return
      enddo
      good = .true.
    endif
       
  end function checkNCName


  function checkPublicId(value) result(good)
    character(len=*), intent(in) :: value
    logical :: good
    character(len=*), parameter :: PubIdChars = &
      " "//achar(10)//achar(13)//lowerCase//upperCase//digits//"-'()+,./:=?;!*#@$_%"

    good = (verify(value, PubIdChars)==0) 
  end function checkPublicId

  function checkSystemId(value) result(good)
    character(len=*), intent(in) :: value
    logical :: good
    ! FIXME Check resolving system id -> URL
    ! System ID should be a URL
    ! No fragment IDs allowed!
    ! Only simple check is, can't have both ' & "
    ! For the moment we let everything else through
    
    good = .not.(index(value, '"')>0 .and. index(value, "'")>0)
  end function checkSystemId

  pure function resolveSystemId_len(value) result(n)
    character(len=*), intent(in) :: value
    integer :: n

    integer :: i
    n = len(value)
    do i = 1, len(value)
      select case(iachar(value(i:i)))
      case (0:32,34,60,62,92,94,96,123:125)
        n = n + 2
      end select
    enddo
      
  end function resolveSystemId_len

  function resolveSystemId(value) result(sys)
    character(len=*), intent(in) :: value
    character(len=resolveSystemId_len(value)) :: sys

    integer :: i, i_s
    i_s = 1
    do i = 1, len(value)
      select case(iachar(value(i:i)))
      case (0:32,34,60,62,92,94,96,123:125)
        sys(i_s:i_s+3) = '%'//iachar(value(i:i))
        i_s = i_s + 3
      case default
        sys(i_s:i_s) = value(i:i)
        i_s = i_s + 1
      end select
    enddo
      
  end function resolveSystemId



  function checkIRI(IRI) result(good)
    character(len=*), intent(in) :: IRI
    logical :: good
    !By [Namespaces] section 9, there is no
    ! formal definition of IRI's yet.
    ! The result of this depends whether we are doing
    ! namespaces 1.1 or 1.0. Firstly, because 1.1
    ! allows IRIs as well as URIs, and secondly because
    ! 1.1 allows undeclaring prefixes, so an empty
    ! string is valid here (which is how FoX signals
    ! an undeclared prefix)
    good = .true.
  end function checkIRI

  function checkPEDef(value, xds) result(p)
    character(len=*), intent(in) :: value
    type(xml_doc_state), intent(in) :: xds
    logical :: p

    integer :: i1, i2

    p = .false.
    if (scan(value, '%&')==0) then
      p = .true.
    elseif (scan(value, '"')==0) then
      i1 = scan(value, '%&')
      i2 = 0
      do while (i1>0)
        i1 = i2 + i1
        i2 = index(value(i1+1:),';')
        if (i2==0) return
        i2 = i1 + i2
        if (value(i1:i1)=='&') then
          if (.not.checkName(value(i1+1:i2-1), xds) .and. &
            .not.checkCharacterEntityReference(value(i1+1:i2-1), xds%xml_version)) return
        else
          if (.not.checkName(value(i1+1:i2-1), xds)) &
            return
        endif
        i1 = scan(value(i2+1:), '%&')
      enddo
      p = .true.
    endif
  end function checkPEDef

  function checkPseudoAttValue(value, xds) result(p)
    character(len=*), intent(in) :: value
    type(xml_doc_state), intent(in) :: xds
    logical :: p

    integer :: i1, i2

    p = .false.
    if (scan(value, '"<&')==0) then
      p = .true.
    elseif (index(value, '&') > 0) then
      i1 = index(value, '&')
      i2 = 0
      do while (i1 > 0)
        i1 = i2 + i1
        i2 = index(value(i1+1:),';')
        if (i2==0) return
        i2 = i1 + i2
        if (value(i1+1:i2-1) /= 'amp' .and. &
          value(i1+1:i2-1) /= 'lt' .and. &
          value(i1+1:i2-1) /= 'gt' .and. &
          value(i1+1:i2-1) /= 'quot' .and. &
          value(i1+1:i2-1) /= 'apos' .and. &
          .not.checkCharacterEntityReference(value(i1+1:i2-1), xds%xml_version)) &
          return
        i1 = index(value(i2+1:), '&')
      enddo
      p = .true.
    endif
  end function checkPseudoAttValue

  function checkAttValue(value, xds) result(p)
    character(len=*), intent(in) :: value
    type(xml_doc_state), intent(in) :: xds
    logical :: p

    integer :: i1, i2

    p = .false.
    if (scan(value, '"<&'//"'")==0) then
      p = .true.
    elseif (index(value, '&') > 0) then
      i1 = index(value, '&')
      i2 = 0
      do while (i1 > 0)
        i1 = i2 + 1
        i2 = scan(value(i1+1:),';')
        if (i2 == 0) return
        i2 = i1 + i2
        if (.not.checkName(value(i1+1:i2-1), xds) .and. &
          .not.checkCharacterEntityReference(value(i1+1:i2-1), xds%xml_version)) &
          return
        i1 = index(value(i2+1:), '&')
      enddo
      p = .true.
    endif
  end function checkAttValue

  
  function likeCharacterEntityReference(code) result(good)
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

  end function likeCharacterEntityReference

  function checkCharacterEntityReference(code, xv) result(good)
    character(len=*), intent(in) :: code
    integer, intent(in) :: xv
    logical :: good

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
            i = str_to_int_10(code(2:))
          endif
        endif
      endif
    endif
    if (good) good = isLegalCharRef(i, xv)

  end function checkCharacterEntityReference
  

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

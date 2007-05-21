module m_common_namecheck

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
  public :: checkCharacterEntityReference
  public :: likeCharacterEntityReference
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
    ! For the moment we let everything through.
    
    good = .true.
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
      do while (i1>0)
        i2 = scan(value(i1+1:),';')
        if (i2==0) return
        if (.not.checkName(value(i1+1:i2-1), xds)) return
        i1 = scan(value(i2+1:), '%&')
      enddo
      p = .true.
    endif
  end function checkPEDef

  function likeCharacterEntityReference(code, xv) result(good)
    character(len=*), intent(in) :: code
    integer, intent(in) :: xv
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
!      good = ((0<i .and. i<55296) &
!      .or.(57343<i .and. i<65534) &
!      .or.(65535<i .and. i<4177778))

  end function checkCharacterEntityReference
  
  pure function checkEntityValue(value) result (good)
    ![9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
    !                 |  "'" ([^%&'] | PEReference | Reference)* "'"
    character(len=*), intent(in) :: value
    logical :: good
    ! this is a very imperfect check and should really be in
    ! m_common_entities, and have better error messages
    ! FIXME
    good = (len(value) > 0)
    !good = good.and.(index(value,'&') > 0)
    ! unless it is a char entity reference...
    !if (good) &
    !  good = (index(value, "%") == 0).and.(index(value,"&")==0))
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

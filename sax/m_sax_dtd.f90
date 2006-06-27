module m_sax_dtd

  !Handling the Document Type Definition
  !Only very limited: just internal entities;
  !no external entities
  !And certainly no validation.

  use m_common_array_str, only : str_vs
  use m_common_error, only: FoX_error
  use m_sax_entities, only: entity_list, add_char_entity

  implicit none
  private

  character(len=1), allocatable, dimension(:), save :: docTypeName
  character(len=1), allocatable, dimension(:), save :: PublicId
  character(len=1), allocatable, dimension(:), save :: SystemId

  character(len=*), parameter :: spaces = " "//achar(9)//achar(10)//achar(13)

  character(len=*), parameter :: lowerCase = "abcdefghijklmopqrstuvwxyz"
  character(len=*), parameter :: upperCase = "ABCDEFGHIJKLMNOPRSTUVWXYZ"
  character(len=*), parameter :: digits = "0123456789"
  character(len=*), parameter :: NameChars = lowerCase//upperCase//digits//".-_:"

  character(len=*), parameter :: PubIdChars = NameChars//spaces//"'()+,/=?;!*#@$%"

  public :: parse_dtd

contains

  subroutine parse_dtd(ents, dtd)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in):: dtd

    integer :: c, i, cp
    integer :: dtd_state

    if (dtd(1:7) /= "DOCTYPE") then
      call FoX_error("Invalid DTD")
    endif

    if (len(dtd) > 7) then 
      c = 8
    else
      call FoX_error("Could not find Doctype Name")
    endif
      
    !skip spaces until we hit the Name
    cp = verify(dtd(c:), spaces)
    if (cp == 0) then
      call FoX_error("No Doctype Name found")
    elseif (cp == 1) then
      call FoX_error("Missing space after DOCTYPE declaration")
    endif
    c = c + cp - 1

    ! get doctype NAME
    cp = scan(dtd(c:), spaces)
    if (cp == 0) then
      call FoX_error("Unterminated Doctype Name")
    endif
    allocate(docTypeName(cp-1))
    docTypeName = transfer(dtd(c:c+cp-1), docTypeName)
    if (verify(str_vs(docTypeName), nameChars) /= 0) then
      call FoX_error("Invalid Doctype Name")
    endif
    print'(2a)', 'DOCTYPE ', str_vs(docTypeName)
    c = c + cp - 1

    !skip space until next item
    cp = verify(dtd(c:), spaces)
    if (cp == 0) then
      ! Complete but empty DTD
      return
    endif
    c = c + cp - 1

    if (dtd(c:c+5) == "PUBLIC") then
      c = c + 6
      cp = verify(dtd(c:), spaces)
      if (cp == 0) then
        call FoX_error("Missing PUBLIC ID")
      elseif (cp == 1) then
        call FoX_error("Missing space after PUBLIC keyword")
      endif
      c = c + cp - 1

      if (dtd(c:c) == '"') then
        cp = index(dtd(c+1:), '"')
      else if (dtd(c:c) == "'") then
        cp = index(dtd(c+1:), "'")
      else
        call FoX_error("Unquoted PUBLIC ID")
      endif
      if (cp > 0) then
        allocate(PublicId(cp-1))
        PublicId = transfer(dtd(c+1:c+cp-1), PublicId)
        if (verify(str_vs(PublicId), PubIdChars) /= 0) &
          call FoX_error("Invalid PUBLIC ID")
        c = c + cp + 1
      else
        call FoX_error("Empty PUBLIC ID")
      endif

      cp = verify(dtd(c:), spaces)
      if (cp == 0) then
        call FoX_error("Missing SYSTEM ID")
      elseif (cp == 1) then
        call FoX_error("Missing space after PUBLIC ID")
      endif
      c = c + cp - 1
      print'(2a)', 'PUBLIC ', str_vs(PublicId)

      if (dtd(c:c) == '"') then
        cp = index(dtd(c+1:), '"')
      else if (dtd(c:c) == "'") then
        cp = index(dtd(c+1:), "'")
      else
        call FoX_error("Unquoted SYSTEM ID")
      endif
      if (cp > 0) then
        allocate(SystemId(cp-1))
        SystemId = transfer(dtd(c+1:c+cp-1), SystemId)
        c = c + cp + 1
      else
        call FoX_error("Empty SYSTEM ID")
      endif

      cp = verify(dtd(c:), spaces)
      if (cp == 0) then
        return
      elseif (cp == 1) then
        call FoX_error("Missing space after SYSTEM ID")
      endif
      c = c + cp - 1
      print'(2a)', 'SYSTEM ', str_vs(SystemId)

    elseif (dtd(c:c+5) == "SYSTEM") then

      c = c + 6
      cp = verify(dtd(c:), spaces)
      if (cp == 0) then
        call FoX_error("Missing SYSTEM ID")
      elseif (cp == 1) then
        call FoX_error("Missing space after SYSTEM keyword")
      endif
      c = c + cp - 1

      if (dtd(c:c) == '"') then
        cp = index(dtd(c+1:), '"')
      else if (dtd(c:c) == "'") then
        cp = index(dtd(c+1:), "'")
      else
        call FoX_error("Unquoted SYSTEM ID")
      endif
      if (cp > 0) then
        allocate(SystemId(cp-1))
        SystemId = transfer(dtd(c+1:c+cp-1), SystemId)
        c = c + cp + 1
      else
        call FoX_error("Empty SYSTEM ID")
      endif

      cp = verify(dtd(c:), spaces)
      if (cp == 0) then
        return
      elseif (cp == 1) then
        call FoX_error("No space after SYSTEM ID")
      endif
      c = c + cp - 1
      print'(2a)', 'SYSTEM ', str_vs(SystemId)
      print*, len(SystemId)

    endif

    if (dtd(c:c) == "[") then
      cp = index(dtd(c:), "]")
      if (cp == 0) then
        call FoX_error("Unterminated Internal Subset")
      else
        call parse_internal_subset(ents, dtd(c+1:c+cp-1))
        c = c + cp + 1
      endif
    endif

    if (c == len(dtd)) return

    if (verify(dtd(c:), spaces) > 0) &
      call FoX_error("Broken DTD")

  end subroutine parse_dtd


  subroutine parse_internal_subset(ents, subset)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in) :: subset

    integer :: c, cp

    c = 1
    do while (c < len(subset))
      cp = scan(subset(c:), spaces)
      if (cp == 0) then
        call FoX_error("No meaningful declaration in internal subset")
      endif
      c = c + cp

      if (subset(c:c+1) == "<!") then
        cp = index(subset(c+2:), '>')
        if (cp == 0) then
          call FoX_error("Unterminated declaration")
          return
        endif
        call parse_decl(ents, subset(c+2:c+cp))
        c = c + cp
      elseif (c == len(subset)) then
        return
      else
        call FoX_error("No declaration in internal subset")
      endif
    enddo

  end subroutine parse_internal_subset


  subroutine parse_decl(ents, decl)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in) :: decl

    integer :: c, cp1, cp2, cp3, cp4
    integer :: code_1, code_2, repl_1, repl_2

! FIXME we need to check what sort of entity it is first.

    if (decl(1:6) == "ENTITY") then
      ! we deal with it
      cp1 = verify(decl(7:), spaces)
      if (cp1 == 0) then
        call FoX_error("No ENTITY specified")
      elseif (cp1 == 1) then
        call FoX_error("Missing space after ENTITY keyword")
      endif
      code_1 = 6 + cp1
      cp2 = scan(decl(code_1:), spaces) - 1
      if (cp2 == -1) &
        call FoX_error("No ENTITY text specified")
      code_2 = code_1 + cp2
!FIXME we are not checking for quotes here
      cp3 = verify(decl(code_2+1:), spaces)
      if (cp3 == 0) &
        call FoX_error("No ENTITY text specified")
      repl_1 = code_2 + 1 + cp3
      cp4 = scan(decl(repl_1:), spaces) - 1
      repl_2 = repl_1 + cp4 - 1
      if (cp4 == -1) &
        repl_2 = len(decl) - 1
      call add_char_entity(ents, decl(code_1:code_2), decl(repl_1:repl_2))
    else
      continue
    endif

  end subroutine parse_decl

end module m_sax_dtd

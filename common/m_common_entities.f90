module m_common_entities

  ! Entity management

  ! It deals with: 
  !    1. The five standard entities (gt,lt,amp,apos,quot)
  !    2. Character entities  (but only within the range of the char intrinsic)
  !    3. Parameter entities
  !    4. Internal entities
  ! In addition, it has the capacity to make lists of
  !    1. External parsed entities
  !    2. External unparsed entities
  ! Though nothing is done with them currently elsewhere in FoX.

  !FIXME need to worry about removing entities from a list.

  use m_common_array_str, only: str_vs, vs_str, vs_str_alloc
  use m_common_charset, only: digits
  use m_common_error, only: error_t, ERR_WARNING, ERR_ERROR, FoX_warning, FoX_error
  use m_common_format, only: str_to_int_10, str_to_int_16
  use m_common_namecheck, only: checkName, checkSystemId, checkPubId, &
    checkCharacterEntityReference, checkEntityValue

  implicit none
  private

  type entity_t
    logical :: external
    character(len=1), dimension(:), pointer :: code => null()
    character(len=1), dimension(:), pointer :: repl => null()
    character(len=1), dimension(:), pointer :: publicId => null()
    character(len=1), dimension(:), pointer :: systemId => null()
    character(len=1), dimension(:), pointer :: notation => null()
  end type entity_t

  type entity_list
    private
    type(entity_t), dimension(:), pointer :: list => null()
  end type entity_list

  public :: is_unparsed_entity
  public :: is_external_entity

  public :: expand_entity_text
  public :: expand_entity_text_len
  public :: existing_entity

  public :: expand_char_entity

  public :: entity_filter_text_len
  public :: entity_filter_text

  public :: expand_parameter_entity
  public :: expand_parameter_entity_len

  public :: expand_entity_value_alloc

  public :: entity_filter_EV_len
  public :: entity_filter_EV

  public :: entity_list
  public :: init_entity_list
  public :: reset_entity_list
  public :: destroy_entity_list
  public :: shallow_destroy_entity_list
  public :: print_entity_list
  public :: shallow_copy_entity_list_without
  public :: add_internal_entity
  public :: add_external_entity

contains


  function shallow_copy_entity(ent1) result(ent2)
    type(entity_t), intent(in) :: ent1
    type(entity_t) :: ent2
    
    ent2%external = ent1%external
    ent2%code => ent1%code
    ent2%repl => ent1%repl
    ent2%publicId => ent1%publicId
    ent2%systemId => ent1%systemId
    ent2%notation => ent1%notation

  end function shallow_copy_entity


  subroutine destroy_entity(ent)
    type(entity_t), intent(inout) :: ent
    
    deallocate(ent%code)
    deallocate(ent%repl)
    deallocate(ent%publicId)
    deallocate(ent%systemId)
    deallocate(ent%notation)

  end subroutine destroy_entity


  subroutine init_entity_list(ents)
    type(entity_list), intent(inout) :: ents

    if (associated(ents%list)) deallocate(ents%list)
    allocate(ents%list(0))

  end subroutine init_entity_list


  subroutine reset_entity_list(ents)
    type(entity_list), intent(inout) :: ents

    call destroy_entity_list(ents)
    call init_entity_list(ents)

  end subroutine reset_entity_list


  subroutine destroy_entity_list(ents)
    type(entity_list), intent(inout) :: ents

    integer :: i, n

    n = size(ents%list)
    do i = 1, n
      call destroy_entity(ents%list(i))
    enddo
    deallocate(ents%list)
  end subroutine destroy_entity_list

  subroutine shallow_destroy_entity_list(ents)
    type(entity_list), intent(inout) :: ents

    deallocate(ents%list)
  end subroutine shallow_destroy_entity_list


  function shallow_copy_entity_list_without(ents, code) result(ents2)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in) :: code
    type(entity_list) :: ents2

    integer :: i, i2, n

    n = size(ents%list)
    ! We trust that the relevant entity is in the list ...
    allocate(ents2%list(n-1))
    i2 = 1
    do i = 1, n
      if (str_vs(ents%list(i)%code) /= code) then
        ents2%list(i2) = shallow_copy_entity(ents%list(i))
        i2 = i2 + 1
      endif
    enddo

  end function shallow_copy_entity_list_without


  subroutine print_entity_list(ents)
    type(entity_list), intent(in) :: ents

    integer :: i, n

    n = size(ents%list)
    write(*,'(a)') '>ENTITYLIST'
    do i = 1, n
      write(*,'(a)') str_vs(ents%list(i)%code)
      write(*,'(a)') str_vs(ents%list(i)%repl)
      write(*,'(a)') str_vs(ents%list(i)%publicId)
      write(*,'(a)') str_vs(ents%list(i)%systemId)
      write(*,'(a)') str_vs(ents%list(i)%notation)
    enddo
    write(*,'(a)') '<ENTITYLIST'
  end subroutine print_entity_list


  subroutine add_entity(ents, code, repl, publicId, systemId, notation, external)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in) :: code
    character(len=*), intent(in) :: repl
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    character(len=*), intent(in) :: notation
    logical, intent(in) :: external

    type(entity_list) :: ents_tmp
    integer :: i, n

    ! This should only ever be called by add_internal_entity or add_external_entity
    ! below, so we don't bother sanity-checking input. Note especially we don't 
    ! check for duplication of entities, so this will happily add another entity
    ! of the same name if you ask it to. This should't matter though, since the
    ! first defined will always be picked up first, which is what the XML spec
    ! requires.

    n = size(ents%list)
    
    allocate(ents_tmp%list(n))
    do i = 1, n
      ents_tmp%list(i) = shallow_copy_entity(ents%list(i))
    enddo
    deallocate(ents%list)
    allocate(ents%list(n+1))
    do i = 1, n
      ents%list(i) = shallow_copy_entity(ents_tmp%list(i))
    enddo
    deallocate(ents_tmp%list)
    ents%list(i)%external = external
    allocate(ents%list(i)%code(len(code)))
    ents%list(i)%code = vs_str(code)
    allocate(ents%list(i)%repl(len(repl)))
    ents%list(i)%repl = vs_str(repl)
    allocate(ents%list(i)%publicId(len(publicId)))
    ents%list(i)%publicId = vs_str(publicId)
    allocate(ents%list(i)%systemId(len(systemId)))
    ents%list(i)%systemId = vs_str(systemId)
    allocate(ents%list(i)%notation(len(notation)))
    ents%list(i)%notation = vs_str(notation)
  end subroutine add_entity


  subroutine add_internal_entity(ents, code, repl)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in) :: code
    character(len=*), intent(in) :: repl

    if (.not.checkName(code)) &
      call FoX_error("Illegal entity name: "//code)
    if (.not.checkEntityValue(repl)) &
      call FoX_error("Illegal entity value: "//repl)
    call add_entity(ents, code, repl, "", "", "", .false.)
  end subroutine add_internal_entity

  
  subroutine add_external_entity(ents, code, systemId, publicId, notation)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in) :: code
    character(len=*), intent(in) :: systemId
    character(len=*), intent(in), optional :: publicId
    character(len=*), intent(in), optional :: notation

    if (.not.checkName(code)) &
      call FoX_error("Illegal entity name. "//code)
    if (.not.checkSystemId(systemId)) &
      call FoX_error("Illegal system Id. "//systemId)
    if (present(publicId)) then
      if (.not.checkPubId(publicId)) &
        call FoX_error("Illegal publicId. "//publicId)
    endif
    if (present(notation)) then
      if (.not.checkName(notation)) &
        call FoX_error("Illegal notation. "//notation)
    endif

    if (present(publicId) .and. present(notation)) then
      call add_entity(ents, code, "", systemId, publicId, notation, .true.)
    elseif (present(publicId)) then
      call add_entity(ents, code, "", systemId, publicId, "", .true.)
    elseif (present(notation)) then
      call add_entity(ents, code, "", systemId, "", notation, .true.)
    else
      call add_entity(ents, code, "", systemId, "", "", .true.)
    endif
  end subroutine add_external_entity


  function is_unparsed_entity(ents, code) result(p)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in) :: code
    logical :: p

    integer :: i

    p = .false.

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        p = (size(ents%list(i)%notation)>0)
        exit
      endif
    enddo
  end function is_unparsed_entity

  function is_external_entity(ents, code) result(p)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in) :: code
    logical :: p

    integer :: i

    p = .false.

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        p = ents%list(i)%external
        exit
      endif
    enddo
  end function is_external_entity
 
  pure function expand_char_entity_len(code) result(n)
    character(len=*), intent(in) :: code
    integer :: n

    integer :: number

    if (code(1:1) == "#") then
      if (code(2:2) == "x") then       ! hex character reference
        if (verify(code(3:), digits) == 0) then
          number = str_to_int_16(code(3:))   
          if (32 <= number .and. number <= 126) then
            n = 1
          else
            n = len(code) + 2
          endif
        else 
           n = 0
        endif
      else                             ! decimal character reference
        if (verify(code(3:), digits) == 0) then
          number = str_to_int_10(code(2:))
          if (32 <= number .and. number <= 126) then
            n = 1
          else
            n = len(code) + 2
          endif
        else 
          n = 0
        endif
      endif
    else
      n = 0
    endif
  end function expand_char_entity_len


  function expand_char_entity(code) result(repl)
    character(len=*), intent(in) :: code
    character(len=expand_char_entity_len(code)) :: repl

    integer :: number

    select case (len(repl))
    case (0)
      call FoX_error("Invalid character entity reference")
    case (1)  
      if (code(2:2) == "x") then       ! hex character reference
        number = str_to_int_16(code(3:))   
      else                             ! decimal character reference
        number = str_to_int_10(code(2:))
      endif
      repl = achar(number)
      ! FIXME what about > 127 ...
    case default
      repl = "&"//code//";"
    end select

  end function expand_char_entity


  pure function existing_entity(ents, code) result(p)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    logical :: p

    integer :: i

    p = .false.
    
!FIXME the following test is not entirely in accordance with the valid chars check we do elsewhere...

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        p = .true.
        return
      endif
    enddo

  end function existing_entity


  pure function expand_entity_text_len(ents, code) result(n)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    integer :: n

    integer :: i

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        n = size(ents%list(i)%repl)
      endif
    enddo

  end function expand_entity_text_len


  function expand_entity_text(ents, code) result(repl)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    character(len=expand_entity_text_len(ents, code)) :: repl

    integer :: i

    ! No error checking - make sure entity exists before calling it.

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        repl = str_vs(ents%list(i)%repl)
        exit
      endif
    enddo

  end function expand_entity_text


  pure function expand_parameter_entity_len(ents, code) result(n)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    integer :: n

    integer :: i

    if (.not.existing_entity(ents, code)) then
      n = 0
      return
    endif

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        n = size(ents%list(i)%repl)
      endif
    enddo

  end function expand_parameter_entity_len


  function expand_parameter_entity(ents, code) result(repl)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    character(len=expand_entity_text_len(ents, code)) :: repl

    integer :: i

    if (len(repl) == 0) &
      call FoX_error("Non-existent PE")

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        repl = str_vs(ents%list(i)%repl)
      endif
    enddo

  end function expand_parameter_entity
    

  pure function entity_filter_text_len(ents, str) result(n)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in) :: str
    integer :: n

    integer :: i, i2, j, k
    
    n = 0

    i = 1
    i2 = 1
    do
      if (i > len(str)) exit
      if (str(i:i) == "&") then
        if (i+1 > len(str)) then
          exit
        endif
        k = index(str(i+1:),";")
        if (k == 0) then
          exit
        endif
        j = expand_entity_text_len(ents, str(i+1:i+k-1))
        i  = i + k + 1
        i2 = i2 + j
      else
        i = i + 1
        i2 = i2 + 1
      endif
    enddo
    
    n = i2 - 1

  end function entity_filter_text_len

  function entity_filter_text(ents, str) result(str2)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in) :: str
    character(len=entity_filter_text_len(ents, str)) :: str2

    integer :: i, i2, j, k, n

    n = len(str2)

    i = 1
    i2 = 1
    do
      if (i > len(str)) exit
      if (str(i:i) == "&") then
        if (i+1 > len(str)) then
          call FoX_error("Unmatched & in entity reference")
        endif
        k = index(str(i+1:),";")
        if (k == 0) then
          call FoX_error("Unmatched & in entity reference")
        endif
        ! We really need to check here if we have got an unparsed
        ! entity - if we do, then we only let it pass if this is
        ! in content, not in an attribute value.
        j = expand_entity_text_len(ents, str(i+1:i+k-1))
        if (j > 0 .and. n > 0) then
          str2(i2:i2+j-1) = expand_entity_text(ents, str(i+1:i+k-1))
        else
          call FoX_warning("Ignored unknown entity: &" // str(i+1:i+k-1) // ";")
        endif
        i  = i + k + 1
        i2 = i2 + j
      else
        if (n > 0) str2(i2:i2) = str(i:i)
        i = i + 1
        i2 = i2 + 1
      endif
    enddo

  end function entity_filter_text


  pure function entity_filter_EV_len(pents, str) result(n)
    type(entity_list), intent(in) :: pents
    character(len=*), intent(in) :: str
    integer :: n

    integer :: i, i2, j, k

    i = 1
    i2 = 1
    do
      if (i > len(str)) exit
      if (str(i:i) == "&") then
        if (i+1 > len(str)) then
          n = 0
          return
        endif
        k = index(str(i+1:),";")
        if (k == 0) then
          n = 0
          return
        endif
        ! We only want to expand this if it's a character or parameter entity ...
        ! Unparsed entities give undefined results here - we ignore them.FIXME?
        if (checkCharacterEntityReference(str(i+1:i+k-1))) then
          j = expand_char_entity_len(str(i+1:i+k-1))
          i  = i + k + 1
          i2 = i2 + j
        else
          i = i + 1
          i2 = i2 + 1
        endif
      elseif (str(i:i) == "%") then
        if (i+1 > len(str)) then
          n = 0
          return
        endif
        k = index(str(i+1:),";")
        if (k == 0) then
          n = 0
          return
        endif
        j = expand_parameter_entity_len(pents, str(i+1:i+k-1))
        if (j == 0) then
          n = 0
          return
        endif
        i  = i + k + 1
        i2 = i2 + j
      else
        i = i + 1
        i2 = i2 + 1
      endif
    enddo

    n = i2 - 1

  end function entity_filter_EV_len


  function entity_filter_EV(pents, str) result(str2)
    type(entity_list), intent(in) :: pents
    character(len=*), intent(in) :: str
    character(len=entity_filter_EV_len(pents, str)) :: str2

    integer :: i, i2, j, k, n

    n = len(str2)
    str2 = ""
    i = 1
    i2 = 1
    do
      if (i > len(str)) exit
      if (str(i:i) == "&") then
        if (i+1 > len(str)) then
          call FoX_error("Unmatched & in entity reference")
        endif
        k = index(str(i+1:),";")
        if (k == 0) then
          call FoX_error("Unmatched & in entity reference")
        endif
        ! We only want to expand this if it's a character or parameter entity ...
        ! Unparsed entities give undefined results here - we ignore them.FIXME?
        if (checkCharacterEntityReference(str(i+1:i+k-1))) then
          j = expand_char_entity_len(str(i+1:i+k-1))
          if (n > 0) str2(i2:i2+j-1) = expand_char_entity(str(i+1:i+k-1))
          i  = i + k + 1
          i2 = i2 + j
        else
          str2(i2:i2) = "&"
          i = i + 1
          i2 = i2 + 1
        endif
     elseif (str(i:i) == "%") then
        if (i+1 > len(str)) then
          call FoX_error("Unmatched % in entity reference")
        endif
        k = index(str(i+1:),";")
        if (k == 0) then
          call FoX_error("Unmatched % in entity reference")
        endif
        j = expand_parameter_entity_len(pents, str(i+1:i+k-1))
        if (j > 0 .and. n > 0) then
          str2(i2:i2+j-1) = expand_parameter_entity(pents, str(i+1:i+k-1))
        else
          call FoX_error("Unknown parameter entity")
        endif
        i  = i + k + 1
        i2 = i2 + j
      else
        if (n > 0) str2(i2:i2) = str(i:i)
        i = i + 1
        i2 = i2 + 1
      endif
      print*,str2
    enddo

  end function entity_filter_EV

  function expand_entity_error(repl, error) result(n)
    character(len=*), intent(in) :: repl
    type(error_t), optional, intent(out) :: error
    integer :: n
    
    integer :: i, i2, j

    if (index(repl,'%')/=0) then
      n = -1
      if (present(error)) &
        error%msg => vs_str_alloc("Cannot have '%' inside internal entity value in internal subset")
      return
    endif
    
    i = 1
    i2 = 1
    do
      if (i>len(repl)) exit
      if (repl(i:i)=='&') then
        j = index(repl(i+1:),';')
        if (j==0) then
          n = -1
          if (present(error)) &
            error%msg => vs_str_alloc("Unterminated entity reference")
          return
        elseif (checkName(repl(i+1:j-1))) then
          i = i + j
          i2 = i2 + j
        elseif (checkCharacterEntityReference(repl(i+1:j-1))) then
          i = i + 1
          i2 = i2 + 1
        else
          n = -1
          if (present(error)) &
            error%msg => vs_str_alloc("Invalid entity reference")
          return
        endif
      else
        i = i + 1
      endif
    enddo

    n = i2 - 1

  end function expand_entity_error

  function expand_entity_value_alloc(repl, error) result(repl_new)
    !perform expansion of 
    ! 1: character entity references
    ! 2: internal parsed entity references
    !on the value of a just-declared internal parsed entity,
    !before storing it.
    !
    ! This is only ever called from the SAX parser
    ! (might it be called from WXML?)
    ! so input & output is with character arrays, not strings.
    character, dimension(:), intent(in) :: repl
    type(error_t), intent(out) :: error
    character, dimension(:), pointer :: repl_new

    character, dimension(:), pointer :: repl_temp
    integer :: i, i2, j
    
      allocate(repl_new(0))
    if (index(str_vs(repl),'%')/=0) then
      error%msg = vs_str_alloc("Not allowed % in intenal subset general entity value")
      return
    else
      allocate(repl_temp(size(repl))) ! it will always be less than or equal
    endif

    i = 1
    i2 = 1
    do
      if (i>size(repl)) exit
      if (repl(i)=='&') then
        j = index(str_vs(repl(i+1:)),';')
        if (j==0) then
          error%msg = vs_str_alloc("Not allowed bare & in entity value")
          return
        elseif (checkName(str_vs(repl(i+1:j-1)))) then
          repl_temp(i2:i2+j-1) = repl(i:i+j-1)
          i = i + j + 1
          i2 = i2 + j + 1
        elseif (checkCharacterEntityReference(str_vs(repl(i+1:j-1)))) then
          !if it is ascii then
          repl_temp(i2:i2) = vs_str(expand_char_entity(str_vs(repl(i+1:j-1))))
          i = i + j + 1
          i2 = i2 + 1
        else
          error%msg = vs_str_alloc("Invalid entity reference")
          return
        endif
      else
        repl_temp(i2) = repl(i)
        i = i + 1
        i2 = i2 + 1
      endif
    enddo

    deallocate(repl_new)
    allocate(error%msg(0))
    allocate(repl_new(i2-1))
    repl_new = repl_temp(:i2-1)

  end function expand_entity_value_alloc

end module m_common_entities

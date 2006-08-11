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

  use m_common_array_str, only: str_vs, vs_str
  use m_common_charset, only: digits
  use m_common_error, only: FoX_warning, FoX_error
  use m_common_format, only: str_to_int_10, str_to_int_16
  use m_common_namecheck, only: checkName, checkSystemId, checkPubId, &
    checkCharacterEntityReference, checkEntityValue

  implicit none
  private

  type entity_t
    character(len=1), dimension(:), pointer :: code
    logical :: internal
    logical :: parsed
    character(len=1), dimension(:), pointer :: repl
    character(len=1), dimension(:), pointer :: publicId
    character(len=1), dimension(:), pointer :: systemId
    character(len=1), dimension(:), pointer :: notation
  end type entity_t

  type entity_list
    private
    type(entity_t), dimension(:), pointer :: list
    logical :: PE
  end type entity_list

  public :: is_unparsed_entity

  public :: expand_entity_text
  public :: expand_entity_text_len
  public :: existing_entity

  public :: entity_filter_text_len
  public :: entity_filter_text

  public :: expand_parameter_entity
  public :: expand_parameter_entity_len

  public :: entity_filter_EV_len
  public :: entity_filter_EV

  public :: entity_list
  public :: init_entity_list
  public :: reset_entity_list
  public :: destroy_entity_list
  public :: print_entity_list
  public :: copy_entity_list
  public :: add_internal_entity
  public :: add_external_entity

contains


  pure function deep_copy_entity(ent1) result(ent2)
    type(entity_t), intent(in) :: ent1
    type(entity_t) :: ent2
    
    ent2%internal = ent1%internal
    ent2%parsed = ent1%parsed
    allocate(ent2%code(size(ent1%code)))
    allocate(ent2%repl(size(ent1%repl)))
    allocate(ent2%PublicId(size(ent1%PublicId)))
    allocate(ent2%SystemId(size(ent1%SystemId)))
    allocate(ent2%notation(size(ent1%notation)))
    ent2%code = ent1%code
    ent2%repl = ent1%repl
    ent2%publicId = ent1%publicId
    ent2%systemId = ent1%systemId
    ent2%notation = ent1%notation

  end function deep_copy_entity


  function shallow_copy_entity(ent1) result(ent2)
    type(entity_t), intent(in) :: ent1
    type(entity_t) :: ent2
    
    ent2%internal = ent1%internal
    ent2%parsed = ent1%parsed
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


  subroutine init_entity_list(ents, PE)
    type(entity_list), intent(out) :: ents
    logical :: PE

    allocate(ents%list(0))

    ents%PE = PE
    if (.not.PE) then
      call add_entity(ents, "gt", ">", "", "", "", internal=.true., parsed=.true.)
      call add_entity(ents, "lt", "<", "", "", "", internal=.true., parsed=.true.)
      call add_entity(ents, "apos", "'", "", "", "", internal=.true., parsed=.true.)
      call add_entity(ents, "quot", '"', "", "", "", internal=.true., parsed=.true.)
      call add_entity(ents, "amp", "&", "", "", "", internal=.true., parsed=.true.)
    endif

  end subroutine init_entity_list


  subroutine reset_entity_list(ents)
    type(entity_list), intent(inout) :: ents

    call destroy_entity_list(ents)
    call init_entity_list(ents, ents%PE)

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


  function copy_entity_list(ents) result(ents2)
    type(entity_list), intent(in) :: ents
    type(entity_list) :: ents2

    integer :: i, n

    ents2%PE = ents%PE
    n = size(ents%list)
    allocate(ents2%list(n))
    do i = 1, n
      ents2%list(i) = deep_copy_entity(ents%list(i))
    enddo

  end function copy_entity_list


  subroutine print_entity_list(ents)
    type(entity_list), intent(inout) :: ents

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
    deallocate(ents%list)    
  end subroutine print_entity_list


  subroutine add_entity(ents, code, repl, publicId, systemId, notation, internal, parsed)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in) :: code
    character(len=*), intent(in) :: repl
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    character(len=*), intent(in) :: notation
    logical, intent(in) :: internal
    logical, intent(in) :: parsed

    type(entity_list) :: ents_tmp
    integer :: i, n

    ! This should only ever be called by add_internal_entity or add_external_entity
    ! below, so we don't bother sanity-checking input.

    n = size(ents%list)
    !Are we redefining an existing entity?
    do i = 1, n
      if (len(code) == size(ents%list(i)%code)) then
        if (code == str_vs(ents%list(i)%code)) then
          deallocate(ents%list(i)%repl)
          deallocate(ents%list(i)%publicId)
          deallocate(ents%list(i)%systemId)
          deallocate(ents%list(i)%notation)

          ents%list(i)%internal = internal
          ents%list(i)%parsed = parsed
          allocate(ents%list(i)%repl(len(repl)))
          ents%list(i)%repl = vs_str(repl)
          allocate(ents%list(i)%publicId(len(publicId)))
          ents%list(i)%publicId = vs_str(publicId)
          allocate(ents%list(i)%systemId(len(systemId)))
          ents%list(i)%systemId = vs_str(systemId)
          allocate(ents%list(i)%notation(len(notation)))
          ents%list(i)%notation = vs_str(notation)
          return
        endif
      endif
    enddo
    
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

    allocate(ents%list(i)%code(len(code)))
    ents%list(i)%code = vs_str(code)
    ents%list(i)%internal = internal
    ents%list(i)%parsed = parsed
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
      call FoX_error("Illegal entity name")
    if (.not.checkEntityValue(repl)) &
      call FoX_error("Illegal entity value")

    call add_entity(ents, code, repl, "", "", "", .true., .true.)
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
      call add_entity(ents, code, "", systemId, publicId, notation, .true., .false.)
    elseif (present(publicId)) then
      call add_entity(ents, code, "", systemId, publicId, "", .true., .true.)
    elseif (present(notation)) then
      call add_entity(ents, code, "", systemId, "", notation, .true., .false.)
    else
      call add_entity(ents, code, "", systemId, "", "", .true., .true.)
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
        p = .not.ents%list(i)%parsed
      endif
    enddo
  end function is_unparsed_entity
 
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

    if (.not.ents%PE) then
      p = checkCharacterEntityReference(code)
    endif
 
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

    if (checkCharacterEntityReference(code)) then
      n = expand_char_entity_len(code)
      return
    endif

    if (.not.existing_entity(ents, code)) then
      n = len(code) + 2
      return
    endif

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        if (ents%list(i)%internal) then
          n = size(ents%list(i)%repl)
        else
          if (.not.ents%list(i)%parsed) then
            n = 0
          else
            ! Here we would read in the additional doc
            n = len(code) + 2
            ! unless we're in an attribute value, in which case this is forbidden.
          endif
        endif
        return
      endif
    enddo

  end function expand_entity_text_len


  function expand_entity_text(ents, code) result(repl)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    character(len=expand_entity_text_len(ents, code)) :: repl

    integer :: i

    if (checkCharacterEntityReference(code)) then
      repl = expand_char_entity(code)
      return
    endif

    if (.not.existing_entity(ents, code)) then
      repl = "&"//code//";"
      return
    endif

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        if (ents%list(i)%internal) then
          repl = str_vs(ents%list(i)%repl)
        else
          repl = "&"//code//";"
        endif
        return
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

end module m_common_entities

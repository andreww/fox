module m_sax_entities
!
! Entity management
!
! It deals with: 
!    1. The five standard entities (gt,lt,amp,apos,quot)
!    2. Character entities  (but only within the range of the char intrinsic)
!
use m_common_array_str, only: str_vs, vs_str
use m_common_error, only : FoX_warning, FoX_error
use m_common_format, only : str_to_int_10, str_to_int_16

implicit none
private

type entity_t
  character(len=1), dimension(:), pointer :: code
  character(len=1), dimension(:), pointer :: repl
end type entity_t

type entity_list
  private
  type(entity_t), dimension(:), pointer :: list
  logical :: PE
end type entity_list

character(len=*), parameter :: digits = "0123456789"
character(len=*), parameter :: hexdigits = "0123456789abcdefABCDEF"

public :: code_to_str
public :: code_to_str_len
public :: code_registered

public :: entity_filter_len
public :: entity_filter

public :: entity_list
public :: init_entity_list
public :: reset_entity_list
public :: destroy_entity_list
public :: print_entity_list
public :: copy_entity_list
public :: add_char_entity

contains

  subroutine init_entity_list(ents, PE)
    type(entity_list), intent(out) :: ents
    logical :: PE

    allocate(ents%list(0))

    ents%PE = PE
    if (PE) then
      call add_char_entity(ents, "gt", ">")
      call add_char_entity(ents, "lt", "<")
      call add_char_entity(ents, "apos", "'")
      call add_char_entity(ents, "quot", '"')
      call add_char_entity(ents, "amp", "&")
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
      deallocate(ents%list(i)%code)
      deallocate(ents%list(i)%repl)
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
      allocate(ents2%list(i)%code(size(ents%list(i)%code)))
      ents2%list(i)%code = ents%list(i)%code
      allocate(ents2%list(i)%repl(size(ents%list(i)%repl)))
      ents2%list(i)%repl = ents%list(i)%repl
    enddo

  end function copy_entity_list


  subroutine print_entity_list(ents)
    type(entity_list), intent(inout) :: ents

    integer :: i, n

    n = size(ents%list)
    print*, '>ENTITYLIST'
    do i = 1, n
      print'(a)', ents%list(i)%code
      print'(a)', ents%list(i)%repl
    enddo
    print*, '<ENTITYLIST'
    deallocate(ents%list)    
  end subroutine print_entity_list

  subroutine add_char_entity(ents, code, repl)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in) :: code
    character(len=*), intent(in) :: repl

    type(entity_list) :: ents_tmp
    integer :: i, n

    print*,'new entity:'
    print*,code
    print*, repl

    n = size(ents%list)
    
    allocate(ents_tmp%list(n))
    do i = 1, n
      ents_tmp%list(i)%code => ents%list(i)%code
      ents_tmp%list(i)%repl => ents%list(i)%repl
    enddo
    deallocate(ents%list)
    allocate(ents%list(n+1))
    do i = 1, n
      ents%list(i)%code => ents_tmp%list(i)%code
      ents%list(i)%repl => ents_tmp%list(i)%repl
    enddo
    deallocate(ents_tmp%list)
    allocate(ents%list(i)%code(len(code)))
    allocate(ents%list(i)%repl(len(repl)))
    ents%list(i)%code = vs_str(code)
    ents%list(i)%repl = vs_str(repl)
  end subroutine add_char_entity

  
  pure function code_registered(ents, code) result(p)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    logical :: p

    integer :: i

    p = .false.

    if (.not.ents%PE) then
      if (len(code) > 1) then
        if (code(1:1) == "#") then
          if (code(2:2) == "x") then
            if (len(code) > 2) p = (verify(code(3:), hexdigits) == 0)
          else
            p = (verify(code(2:), digits) == 0)
          endif
        endif
      endif
    endif
 
    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        p = .true.
        return
      endif
    enddo

  end function code_registered

  pure function code_to_str_len(ents, code) result(n)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    integer :: n

    integer :: i

    if (.not.code_registered(ents, code)) then
      n = 0
      return
    endif

    if (.not.ents%PE) then
      if (code(1:1) == "#") then
        n = 1
      endif
    endif

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        n = size(ents%list(i)%repl)
        return
      endif
    enddo
  end function code_to_str_len

  function code_to_str(ents, code) result(repl)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in)  :: code
    character(len=code_to_str_len(ents, code)) :: repl

    integer :: number, i

    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
        repl = str_vs(ents%list(i)%repl)
        return
      endif
    enddo

    if (.not.ents%PE) then
      ! Replace character references  (but only within the range of the
      ! char intrinsic !!)
      if (code(1:1) == "#") then
        if (code(2:2) == "x") then       ! hex character reference
          number = str_to_int_16(code(3:))   
          repl = char(number)
        else                             ! decimal character reference
          number = str_to_int_10(code(2:))
          repl = char(number)
        endif
      endif
    endif

  end function code_to_str
    

  pure function entity_filter_len(ents, str) result(n)
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
        j = code_to_str_len(ents, str(i+1:i+k-1))
        i  = i + k + 1
        i2 = i2 + j
      else
        i = i + 1
        i2 = i2 + 1
      endif
    enddo
    
    n = i2 - 1

  end function entity_filter_len

  function entity_filter(ents, str) result(str2)
    type(entity_list), intent(in) :: ents
    character(len=*), intent(in) :: str
    character(len=entity_filter_len(ents, str)) :: str2

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
        j = code_to_str_len(ents, str(i+1:i+k-1))
        if (j > 0 .and. n > 0) then
          str2(i2:i2+j-1) = code_to_str(ents, str(i+1:i+k-1))
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

  end function entity_filter

end module m_sax_entities

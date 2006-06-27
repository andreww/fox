module m_sax_entities
!
! Entity management
!
! It deals with: 
!    1. The five standard entities (gt,lt,amp,apos,quot)
!    2. Character entities  (but only within the range of the char intrinsic)
!
use m_common_array_str, only: str_vs, vs_str
use m_common_buffer
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
end type entity_list

character(len=*), parameter :: digits = "0123456789"
character(len=*), parameter :: hexdigits = "0123456789abcdefABCDEF"

public :: code_to_str , entity_filter
public :: code_to_str_len
public :: code_registered

public :: entity_list
public :: init_entity_list
public :: reset_entity_list
public :: destroy_entity_list
public :: add_char_entity

contains

  subroutine init_entity_list(ents, standard)
    type(entity_list), intent(out) :: ents
    logical, optional :: standard

    allocate(ents%list(0))

    if (present(standard)) then
      if (.not.standard) then
        return
      endif
    endif
    call add_char_entity(ents, "gt", ">")
    call add_char_entity(ents, "lt", "<")
    call add_char_entity(ents, "apos", "'")
    call add_char_entity(ents, "quot", '"')
    call add_char_entity(ents, "amp", "&")

  end subroutine init_entity_list


  subroutine reset_entity_list(ents, standard)
    type(entity_list), intent(inout) :: ents
    logical, optional :: standard

    type(entity_list) :: ents_tmp
    integer :: i, n

    call destroy_entity_list(ents)
    call init_entity_list(ents, standard)

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


  subroutine add_char_entity(ents, code, repl)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in) :: code
    character(len=*), intent(in) :: repl

    type(entity_list) :: ents_tmp
    integer :: i, n

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

    if (len(code) > 1) then
      if (code(1:1) == "#") then
        if (code(2:2) == "x") then
          if (len(code) > 2) p = (verify(code(3:), hexdigits) == 0)
        else
          p = (verify(code(2:), digits) == 0)
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

    if (code(1:1) == "#") then
      n = 1
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

    ! Replace character references  (but only within the range of the
    ! char intrinsic !!)
    if (code(1:1) == "#") then
      if (code(2:2) == "x") then       ! hex character reference
        number = str_to_int_16(code(3:))   
        repl = char(number)
      else                             ! decimal character reference
        number = str_to_int_10(code(3:))   
        repl = char(number)
      endif
    endif

  end function code_to_str
    

  subroutine entity_filter(ents, buf1, buf2)
    type(entity_list), intent(inout) :: ents
    type(buffer_t), intent(in)    :: buf1
    type(buffer_t), intent(out)   :: buf2

    ! Replaces entity references by their value
    integer :: i, k, len1
    character(len=MAX_BUFF_SIZE)           :: s1
    character(len=1)                       :: c

    integer                                :: n

    call buffer_to_character(buf1,s1)        !! Avoid allocation of temporary
    len1 = len(buf1)

    i = 1

    call reset_buffer(buf2)

    do
      if (i > len1) exit
      c = s1(i:i)
      if (c == "&") then
        if (i+1 > len1) then
          call FoX_error("Unmatched & in entity reference")
          return
        endif
        k = index(s1(i+1:),";")
        if (k == 0) then
          call FoX_error("Unmatched & in entity reference")
          return
        endif
        n = code_to_str_len(ents, s1(i+1:i+k-1))
        if (n > 0) then
          call add_to_buffer(code_to_str(ents, s1(i+1:i+k-1)), buf2)
        else
          call FoX_warning("Ignored unknown entity: &" // s1(i+1:i+k-1) // ";")
        endif
        i  = i + k + 1
      else
        call add_to_buffer(c,buf2)
        i = i + 1
      endif
    enddo

  end subroutine entity_filter

end module m_sax_entities

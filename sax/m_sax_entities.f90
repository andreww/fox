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
implicit none
private

integer, parameter :: MAX_REPLACEMENT_SIZE = 200

type entity_t
  character(len=1), dimension(:), pointer :: code
  character(len=1), dimension(:), pointer :: repl
end type entity_t

type entity_list
  private
  type(entity_t), dimension(:), pointer :: list
end type entity_list

integer, parameter  ::  N_ENTITIES  = 5

public :: code_to_str , entity_filter

public :: entity_list
public :: init_entity_list
public :: reset_entity_list
public :: destroy_entity_list
public :: add_char_entity

contains

  subroutine init_entity_list(ents)
    type(entity_list), intent(out) :: ents

    allocate(ents%list(0))
    call add_char_entity(ents, "gt", ">")
    call add_char_entity(ents, "lt", "<")
    call add_char_entity(ents, "apos", "'")
    call add_char_entity(ents, "quot", '"')
    call add_char_entity(ents, "amp", "&")
  end subroutine init_entity_list


  subroutine reset_entity_list(ents)
    type(entity_list), intent(inout) :: ents

    type(entity_list) :: ents_tmp
    integer :: i, n

    n = size(ents%list)
    
    allocate(ents_tmp%list(n))
    do i = 1, 5
      ents_tmp%list(i)%code => ents%list(i)%code
      ents_tmp%list(i)%repl => ents%list(i)%repl
    enddo
    do i = 6, n
      deallocate(ents%list(i)%code)
      deallocate(ents%list(i)%repl)
    enddo
    deallocate(ents%list)
    allocate(ents%list(5))
    do i = 1, 5
      ents%list(i)%code => ents_tmp%list(i)%code
      ents%list(i)%repl => ents_tmp%list(i)%repl
    enddo

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
    allocate(ents%list(i)%code(len(code)))
    allocate(ents%list(i)%repl(len(repl)))
    ents%list(i)%code = vs_str(code)
    ents%list(i)%repl = vs_str(repl)
  end subroutine add_char_entity


  subroutine code_to_str(ents, code, str, status)
    type(entity_list), intent(inout) :: ents
    character(len=*), intent(in)  :: code
    character(len=*), intent(out) :: str
    integer, intent(out)          :: status         
    integer :: i

    integer   :: number, ll
    character(len=4)  :: fmtstr

    status = -1
    do i = 1, size(ents%list)
      if (code == str_vs(ents%list(i)%code)) then
         str = str_vs(ents%list(i)%repl)
         status = 0
         return
      endif
    enddo

    ! Replace character references  (but only within the range of the
    ! char intrinsic !!)
    if (code(1:1) == "#") then
      if (code(2:2) == "x") then       ! hex character reference
        ll = len_trim(code(3:))
        write(unit=fmtstr,fmt="(a2,i1,a1)") "(Z", ll,")"
        read(unit=code(3:),fmt=fmtstr) number
        str = char(number)
        status = 0
        return
      else                             ! decimal character reference
        read(unit=code(2:),fmt=*) number
        str = char(number)
        status = 0
        return
      endif
    endif

  end subroutine code_to_str

  subroutine entity_filter(ents, buf1, buf2)
    type(entity_list), intent(inout) :: ents
    type(buffer_t), intent(in)    :: buf1
    type(buffer_t), intent(out)   :: buf2

    ! Replaces entity references by their value
    integer :: i, k, len1
    character(len=MAX_BUFF_SIZE)           :: s1
    character(len=1)                       :: c
    character(len=MAX_REPLACEMENT_SIZE)    :: repl

    character(len=120)                     :: message
    integer                                :: status

    call buffer_to_character(buf1,s1)        !! Avoid allocation of temporary
    len1 = len(buf1)

    i = 1

    call reset_buffer(buf2)

    do
      if (i > len1) exit
      c = s1(i:i)
      if (c == "&") then
        if (i+1 > len1) then
          message=  " Unmatched & in entity reference"
          call FoX_error(message)
          return
        endif
        k = index(s1(i+1:),";")
        if (k == 0) then
          message=  " Unmatched & in entity reference"
          call FoX_error(message)
          return
        endif
        call code_to_str(ents, s1(i+1:i+k-1),repl,status)
        if (status /= 0) then
          message= "Ignored unknown entity: &" // s1(i+1:i+k-1) // ";"
          call FoX_warning(message)
        else
          call add_to_buffer(trim(repl),buf2)
        endif
        i  = i + k + 1
      else
        call add_to_buffer(c,buf2)
        i = i + 1
      endif
    enddo

  end subroutine entity_filter

end module m_sax_entities

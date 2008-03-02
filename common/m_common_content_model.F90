module m_common_content_model

#ifndef DUMMYLIB
  ! Allow validating the content model of an XML document

  use fox_m_fsys_array_str, only: str_vs, vs_str_alloc
  implicit none
  private

  integer, parameter :: OP_NULL = 0
  integer, parameter :: OP_EMPTY = 1
  integer, parameter :: OP_ANY = 2
  integer, parameter :: OP_MIXED = 3
  integer, parameter :: OP_NAME = 4
  integer, parameter :: OP_CHOICE = 5
  integer, parameter :: OP_SEQ = 6

  integer, parameter :: REP_NULL = 0
  integer, parameter :: REP_ONCE = 1
  integer, parameter :: REP_QUESTION_MARK = 2
  integer, parameter :: REP_ASTERISK = 3
  integer, parameter :: REP_PLUS = 4
  
  type content_particle_t
    character, pointer :: name(:) => null()
    integer :: operator = OP_NULL
    integer :: repeater = REP_NULL
    type(content_particle_t), pointer :: nextSibling => null()
    type(content_particle_t), pointer :: parent => null()
    type(content_particle_t), pointer :: firstChild => null()
    type(content_particle_t), pointer :: lastChild => null()
  end type content_particle_t

  type content_model_state
    type(content_particle_t), pointer :: cp
    logical :: match = .false.
  end type content_model_state

  public :: content_particle_t
  public :: content_model_state

  public :: newCP
  public :: checkContentModel
  public :: destroyCPtree

  public :: OP_MIXED, OP_CHOICE, OP_SEQ
  public :: REP_QUESTION_MARK, REP_ASTERISK, REP_PLUS

contains

  function newCP(empty, any, name, repeat) result(cp)
    logical, intent(in), optional :: empty
    logical, intent(in), optional :: any
    character(len=*), intent(in), optional :: name
    character, intent(in), optional :: repeat
    type(content_particle_t), pointer :: cp

    allocate(cp)
    if (present(empty)) then
      cp%operator = OP_EMPTY
    elseif (present(any)) then
      cp%operator = OP_ANY
    elseif (present(name)) then
      cp%operator = OP_NAME
      cp%name => vs_str_alloc(name)
    endif
    if (present(repeat)) then
      select case (repeat)
      case("?")
        cp%repeater = REP_QUESTION_MARK
      case("*")
        cp%repeater = REP_ASTERISK
      case("+")
        cp%repeater = REP_PLUS
      end select
    endif

  end function newCP

  function checkContentModel(cms, name) result(p)
    type(content_model_state), pointer :: cms
    character(len=*), intent(in) :: name
    logical :: p

    type(content_particle_t), pointer :: tcp

    select case(cms%cp%operator)
    case (OP_EMPTY)
      p = .false.
    case (OP_ANY)
      p = .true.
    case (OP_MIXED)
      if (name(1:1)=="#") then
        ! any text/pi/comment/entity etc allowed.
        p = .true.
      else
        p = .false.
        tcp => cms%cp%firstChild
        do while (associated(tcp))
          if (name==str_vs(tcp%name)) then
            p = .true.
            exit
          endif
          tcp => tcp%nextSibling
        enddo
      endif
    case default
      p = .true.
    end select

  end function checkContentModel

  subroutine destroyCP(cp)
    type(content_particle_t), pointer :: cp

    if (associated(cp%name)) deallocate(cp%name)
    deallocate(cp)
  end subroutine destroyCP

  subroutine destroyCPtree(cp)
    type(content_particle_t), pointer :: cp

    type(content_particle_t), pointer :: current, tcp

    current => cp
    do
      do while (associated(current%firstChild))
        current => current%firstChild
      enddo
      tcp => current
      do while (associated(current%nextSibling))
        current => current%nextSibling
        call destroyCP(tcp)
        tcp => current
      enddo
      print*,associated(current), associated(cp)
      if (associated(current, cp)) exit
      current => current%parent
      call destroyCP(tcp)
      current%firstChild => null()
    enddo
    call destroyCP(cp)

  end subroutine destroyCPtree
    
    
      

#endif

end module m_common_content_model

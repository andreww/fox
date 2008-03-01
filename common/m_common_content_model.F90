module m_common_content_model

#ifndef DUMMYLIB
  ! Allow validating the content model of an XML document

  use fox_m_fsys_array_str, only: vs_str_alloc
  implicit none
  private

  integer, parameter :: OP_NULL = 0
  integer, parameter :: OP_EMPTY = 1
  integer, parameter :: OP_ANY = 2
  integer, parameter :: OP_PCDATA = 3
  integer, parameter :: OP_NAME = 4
  integer, parameter :: OP_CHOICE = 5
  integer, parameter :: OP_SEQ = 6

  integer, parameter :: REP_NULL = 0
  integer, parameter :: REP_ONCE = 1
  integer, parameter :: REP_QUESTION_MARK = 2
  integer, parameter :: REP_ASTERISK = 3
  integer, parameter :: REP_PLUS = 4
  
  type content_particle_t
    private
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

contains

  function newCP(empty, any) result(cp)
    logical, intent(in), optional :: empty
    logical, intent(in), optional :: any
    type(content_particle_t), pointer :: cp

    allocate(cp)
    if (present(empty)) then
      cp%operator = OP_EMPTY
    elseif (present(any)) then
      cp%operator = OP_ANY
    endif
  end function newCP

  function checkContentModel(cms, name) result(p)
    type(content_model_state), pointer :: cms
    character(len=*), intent(in) :: name
    logical :: p
    p = .true.
  end function checkContentModel

#endif

end module m_common_content_model

module m_common_content_model

#ifndef DUMMYLIB
  ! Allow validating the content model of an XML document

  use fox_m_fsys_array_str, only: str_vs, vs_str_alloc, vs_vs_alloc
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
  
  type content_particle_t
    character, pointer :: name(:) => null()
    integer :: operator = OP_NULL
    integer :: repeater = REP_NULL
    type(content_particle_t), pointer :: nextSibling => null()
    type(content_particle_t), pointer :: parent => null()
    type(content_particle_t), pointer :: firstChild => null()
  end type content_particle_t

  type content_model_state
    type(content_particle_t), pointer :: cp
    logical :: checked = .false.
    logical :: matched = .false.
  end type content_model_state

  public :: content_particle_t
  public :: content_model_state

  public :: newCP
  public :: transformCPPlus
  public :: checkContentModel
  public :: nextCP
  public :: destroyCPtree

  public :: OP_NULL, OP_MIXED, OP_CHOICE, OP_SEQ
  public :: REP_QUESTION_MARK, REP_ASTERISK

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
      end select
    endif

  end function newCP

  function copyCP(cp) result(cp_out)
    type(content_particle_t), pointer :: cp
    type(content_particle_t), pointer :: cp_out

    allocate(cp_out)
    if (associated(cp%name)) cp_out%name => vs_vs_alloc(cp%name)
    cp_out%operator = cp%operator
    cp_out%repeater = cp%repeater
  end function copyCP

  function copyCPtree(cp) result(cp_out)
    type(content_particle_t), pointer :: cp
    type(content_particle_t), pointer :: cp_out

    type(content_particle_t), pointer :: tcp, tcp_out, tcpn_out, tcpp_out
    logical :: done
    tcp => cp
    cp_out => copyCP(cp)
    tcp_out => cp_out
    done = .false.
    do while (associated(tcp_out))
      do while (associated(tcp%firstChild).and..not.done)
        tcp => tcp%firstChild
        tcpn_out => copyCP(tcp)
        tcp_out%firstChild => tcpn_out
        tcpn_out%parent => tcp_out
        tcp_out => tcpn_out
      enddo
      tcpp_out => tcp%parent
      if (associated(tcp%nextSibling)) then
        done = .false.
        tcp => tcp%nextSibling
        tcpn_out => copyCP(tcp)
        tcp_out%nextSibling => tcpn_out
        tcpn_out%parent => tcpp_out
        tcp_out => tcpn_out
      else
        done = .true.
        tcp => tcp%parent
        tcp_out => tcp_out%parent
      endif
    enddo
  end function copyCPtree

  subroutine transformCPPlus(cp)
    type(content_particle_t), pointer :: cp

    type(content_particle_t), pointer :: cp_new

! Make copy of cp, and graft children on
    cp_new => copyCP(cp)
    cp_new%firstChild => cp%firstChild

! Clear cp & make it an SEQ
    if (associated(cp%name)) deallocate(cp%name)
    cp%operator = OP_SEQ

! Append our copied cp to the now-an-SEQ
    cp%firstChild => cp_new
    cp_new%parent => cp
! Copy it for a sibling, and make the sibling a *
    cp_new%nextSibling => copyCPtree(cp_new)
    cp_new%nextSibling%parent => cp
    cp_new%nextSibling%repeater = REP_ASTERISK
  end subroutine transformCPPlus

  function checkContentModel(cms, name) result(p)
    type(content_model_state), pointer :: cms
    character(len=*), intent(in) :: name
    logical :: p

    type(content_particle_t), pointer :: tcp, tcp2
    logical :: opt

    ! cms%cp will initially either point at:
    ! a) the top level cp node
    ! b) a named cp node that was matched last time
    ! c) null(), because weve exhausted the regex.

    ! for EMPTY, ANY or MIXED, cms%cp never moves.
    ! for element content, we move the pointer as we
    ! move through the regex.

    ! If the regex includes ambiguous content, we are
    ! a bit screwed. But the document is in error if so.
    ! (and we are not required to diagnose errors.)

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
      do
        if (.not.associated(cms%cp)) then
          p = .false.
          exit
        endif
        select case (cms%cp%operator)
        case (OP_NAME)
          select case(cms%cp%repeater)
          case (REP_NULL)
            p = (name==str_vs(cms%cp%name))
            if (p) call nextCP(cms)
            exit
          case (REP_QUESTION_MARK)
            p = (name==str_vs(cms%cp%name))
            call nextCP(cms)
            if (p) exit
          case (REP_ASTERISK)
            p = (name==str_vs(cms%cp%name))
            if (p) exit
            call nextCP(cms)
          end select
        case (OP_CHOICE)
          p = .true.
          exit
        case (OP_SEQ)
!!$          if (.not.checked)
!!$          select case(cms%cp%repeater)
!!$          case (REP_NULL)
!!$            if (cms%cp%checked) then
!!$              call nextCP(cms)
!!$            else
!!$              p = .false.
!!$              exit
!!$            endif
!!$            cms%checked = .true.
!!$          case (REP_QUESTION_MARK, REP_ASTERISK)
!!$            cms%checked = .true.
!!$            call nextCP(cms)
!!$          case (REP_ASTERISK)
!!$            if (cms%cp%checked) then
!!$              if (cms%cp%matched) then
!!$                call nextCP(cms)
!!$              else
!!$                p = .false.
!!$                exit
!!$              endif
!!$            endif
!!$            cms%checked = .true.
!!$          end select
          p = .true.
          exit
        end select
      end do
    end select

  end function checkContentModel

  subroutine nextCP(cms)
    type(content_model_state), pointer :: cms

    if (.not.cms%checked &
      .and.associated(cms%cp%firstChild)) then
      cms%cp => cms%cp%firstChild
    elseif (associated(cms%cp%nextSibling)) then
      cms%cp => cms%cp%nextSibling
    else
      cms%cp => cms%cp%parent
      cms%checked = .true.
    endif

  end subroutine nextCP

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
      if (associated(current, cp)) exit
      tcp => current
      if (associated(current%nextSibling)) then
        current => current%nextSibling
        call destroyCP(tcp)
      else
        current => current%parent
        call destroyCP(tcp)
        current%firstChild => null()
      endif
    enddo
    call destroyCP(cp)

  end subroutine destroyCPtree
#endif

end module m_common_content_model

module m_common_notations

  use m_common_array_str, only: vs_str, str_vs
  use m_common_error, only: FoX_error

  implicit none
  private

  type notation
    character(len=1), dimension(:), pointer :: name
    character(len=1), dimension(:), pointer :: systemID
    character(len=1), dimension(:), pointer :: publicId
  end type notation

  type notation_list
    type(notation), dimension(:), pointer :: notations
  end type notation_list

  public :: notation
  public :: notation_list
  public :: init_notation_list
  public :: destroy_notation_list
  public :: add_notation
  public :: notation_exists

contains

  subroutine init_notation_list(nlist)
! It is not clear how we should specify the 
! intent of this argument - different 
! compilers seem to have different semantics
    type(notation_list), intent(inout) :: nlist

    allocate(nlist%notations(0:0))
    allocate(nlist%notations(0)%name(0))
    allocate(nlist%notations(0)%systemId(0))
    allocate(nlist%notations(0)%publicId(0))
    
  end subroutine init_notation_list


  subroutine destroy_notation_list(nlist)
    type(notation_list), intent(inout) :: nlist

    integer :: i

    do i = 0, ubound(nlist%notations, 1)
      deallocate(nlist%notations(i)%name)
      deallocate(nlist%notations(i)%systemId)
      deallocate(nlist%notations(i)%publicId)
    enddo
    deallocate(nlist%notations)
  end subroutine destroy_notation_list
 

  subroutine add_notation(nlist, name, systemId, publicId)
    type(notation_list), intent(inout) :: nlist
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: systemId
    character(len=*), intent(in), optional :: publicId

    integer :: i
    type(notation), dimension(:), allocatable :: temp

    if (.not.present(systemId) .and. .not.present(publicId)) &
      call FoX_error("Neither System nor Public Id specified for notation: "//name)

    allocate(temp(0:ubound(nlist%notations,1)))
    do i = 0, ubound(nlist%notations, 1)
      temp(i)%name => nlist%notations(i)%name
      temp(i)%systemId => nlist%notations(i)%systemId
      temp(i)%publicId => nlist%notations(i)%publicId
    enddo

    deallocate(nlist%notations)
    allocate(nlist%notations(0:ubound(temp, 1)+1))
    do i = 0, ubound(temp, 1)
      nlist%notations(i)%name => temp(i)%name
      nlist%notations(i)%systemId => temp(i)%systemId
      nlist%notations(i)%publicId => temp(i)%publicId
    enddo

    allocate(nlist%notations(i)%name(len(name)))
    nlist%notations(i)%name = vs_str(name)
    if (present(systemId)) then
      allocate(nlist%notations(i)%systemId(len(systemId)))
      nlist%notations(i)%systemId = vs_str(systemId)
    else
      allocate(nlist%notations(i)%systemId(0))
    endif
    if (present(publicId)) then
      allocate(nlist%notations(i)%publicId(len(publicId)))
      nlist%notations(i)%publicId = vs_str(publicId)
    else
      allocate(nlist%notations(i)%publicId(0))
    endif
  end subroutine add_notation


  function notation_exists(nlist, name) result(p)
    type(notation_list), intent(in) :: nlist
    character(len=*), intent(in) :: name
    logical :: p

    integer :: i
    
    p = .false.
    do i = 1, ubound(nlist%notations, 1)
      if (str_vs(nlist%notations(i)%name) == name) then
        p = .true.
        exit
      endif
    enddo
  end function notation_exists

end module m_common_notations

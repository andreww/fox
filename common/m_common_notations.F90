module m_common_notations

#ifndef DUMMYLIB
  use fox_m_fsys_vstr, only: vs, new_vs, destroy_vs, & 
      & as_chars, operator(==)
  use m_common_error, only: FoX_error

  implicit none
  private

  type notation
    type(vs), pointer :: name
    type(vs), pointer :: systemID
    type(vs), pointer :: publicId
  end type notation

  type notation_list
    type(notation), dimension(:), pointer :: list
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

    allocate(nlist%list(0:0))
    nlist%list(0)%name => new_vs()
    nlist%list(0)%systemId => new_vs()
    nlist%list(0)%publicId => new_vs()
    
  end subroutine init_notation_list


  subroutine destroy_notation_list(nlist)
    type(notation_list), intent(inout) :: nlist

    integer :: i

    do i = 0, ubound(nlist%list, 1)
      call destroy_vs(nlist%list(i)%name)
      call destroy_vs(nlist%list(i)%systemId)
      call destroy_vs(nlist%list(i)%publicId)
    enddo
    deallocate(nlist%list)
  end subroutine destroy_notation_list
 

  subroutine add_notation(nlist, name, systemId, publicId)
    type(notation_list), intent(inout) :: nlist
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: systemId
    character(len=*), intent(in), optional :: publicId

    integer :: i
    type(notation), dimension(:), pointer :: temp
    ! pointer not allocatable to avoid bug on Lahey

    if (.not.present(systemId) .and. .not.present(publicId)) &
      call FoX_error("Neither System nor Public Id specified for notation: "//name)

    allocate(temp(0:ubound(nlist%list,1)))
    do i = 0, ubound(nlist%list, 1)
      temp(i)%name => nlist%list(i)%name
      temp(i)%systemId => nlist%list(i)%systemId
      temp(i)%publicId => nlist%list(i)%publicId
    enddo

    deallocate(nlist%list)
    allocate(nlist%list(0:ubound(temp, 1)+1))
    do i = 0, ubound(temp, 1)
      nlist%list(i)%name => temp(i)%name
      nlist%list(i)%systemId => temp(i)%systemId
      nlist%list(i)%publicId => temp(i)%publicId
    enddo
    deallocate(temp)

    nlist%list(i)%name => new_vs(init_chars=name)
    if (present(systemId)) then
      nlist%list(i)%systemId => new_vs(init_chars=systemId)
    else
      nlist%list(i)%systemId => new_vs()
    endif
    if (present(publicId)) then
      nlist%list(i)%publicId => new_vs(init_chars=publicId)
    else
      nlist%list(i)%publicId => new_vs()
    endif
  end subroutine add_notation


  function notation_exists(nlist, name) result(p)
    type(notation_list), intent(in) :: nlist
    character(len=*), intent(in) :: name
    logical :: p

    integer :: i
    
    p = .false.
    do i = 1, ubound(nlist%list, 1)
      if (nlist%list(i)%name == name) then
        p = .true.
        exit
      endif
    enddo
  end function notation_exists

#endif
end module m_common_notations

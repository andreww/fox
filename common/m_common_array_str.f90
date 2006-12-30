module m_common_array_str

  implicit none
  private

  type string_t
    character, pointer :: s(:) => null()
  end type string_t

  type string_list
    type(string_t), pointer :: list(:) => null()
  end type string_list

  public :: string_t
  public :: string_list

  public :: init_string_list
  public :: destroy_string_list
  public :: add_string

  public :: str_vs
  public :: vs_str
  public :: vs_str_alloc

contains

  subroutine init_string_list(s_list)
    type(string_list), intent(inout) :: s_list

    allocate(s_list%list(0))
  end subroutine init_string_list

  subroutine destroy_string_list(s_list)
    type(string_list), intent(inout) :: s_list

    integer :: i

    do i = 1, size(s_list%list)
      deallocate(s_list%list(i)%s)
    enddo
    deallocate(s_list%list)
  end subroutine destroy_string_list

  subroutine add_string(s_list, s)
    type(string_list), intent(inout) :: s_list
    character(len=*), intent(in) :: s

    integer :: i
    type(string_t), pointer :: temp(:)

    temp => s_list%list
    allocate(s_list%list(size(temp)+1))
    do i = 1, size(temp)
      s_list%list(i)%s => temp(i)%s
    enddo
    deallocate(temp)
    s_list%list(i)%s => vs_str_alloc(s)
  end subroutine add_string

  pure function str_vs(vs) result(s)
    character, dimension(:), intent(in) :: vs
    character(len=size(vs)) :: s

    s = transfer(vs, s)
  end function str_vs


  pure function vs_str(s) result(vs)
    character(len=*), intent(in) :: s
    character, dimension(len(s)) :: vs

    vs = transfer(s, vs)
  end function vs_str

  pure function vs_str_alloc(s) result(vs)
    character(len=*), intent(in) :: s
    character, dimension(:), pointer :: vs

    allocate(vs(len(s)))
    vs = vs_str(s)
  end function vs_str_alloc

end module m_common_array_str

module m_common_array_str

  use m_common_realtypes, only: dp

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

  interface destroy
    module procedure destroy_vs
  end interface destroy

  interface concat
    module procedure vs_s_concat
  end interface

  interface alloc
    module procedure vs_str_alloc
  end interface

  interface logical
    module procedure vs_logical
  end interface logical
  interface int
    module procedure vs_int
  end interface int
  interface real
    module procedure vs_real
  end interface

  public :: str_vs
  public :: vs_str
  public :: vs_str_alloc
  public :: vs_vs_alloc

  public :: vs_logical
  public :: vs_int
  public :: vs_real

  public :: logical
  public :: int
  public :: real

  public :: concat
  public :: alloc

  public :: destroy

  public :: devnull

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
#ifdef PGF90
!PGI crashes on this use of transfer. Knob-ends.
    integer :: i
    do i = 1, size(vs)
      s(i:i) = vs(i)
    enddo
#else
    s = transfer(vs, s)
#endif
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

  pure function vs_vs_alloc(s) result(vs)
    character, dimension(:), intent(in) :: s
    character, dimension(:), pointer :: vs

    allocate(vs(size(s)))
    vs = s
  end function vs_vs_alloc

  pure function vs_s_concat(vs, s) result(vs2)
    character, dimension(:), intent(in) :: vs
    character(len=*), intent(in) :: s
    character, dimension(:), pointer :: vs2

    allocate(vs2(size(vs)+len(s)))
    vs2(:size(vs)) = vs
    vs2(size(vs)+1:) = vs_str(s)
  end function vs_s_concat

  pure function vs_logical(vs) result(p)
    character, dimension(:), intent(in) :: vs
    logical :: p

    integer :: ios
    character(len=size(vs)) :: s
    s = str_vs(vs)
! FIXME really this should be done by bit-twiddling
! to avoid recursive IO
    read(s, *, iostat=ios) p
    if (ios/=0) p = .false.
  end function vs_logical

  pure function vs_int(vs) result(i)
    character, dimension(:), intent(in) :: vs
    integer :: i

    integer :: ios
    character(len=size(vs)) :: s
    s = str_vs(vs)
! FIXME really this should be done by bit-twiddling
! to avoid recursive IO
    read(s, *, iostat=ios) i
    if (ios/=0) i = 0
  end function vs_int

  pure function vs_real(vs) result(r)
    character, dimension(:), intent(in) :: vs
    real(dp) :: r

    integer :: ios
    character(len=size(vs)) :: s
    s = str_vs(vs)
! FIXME really this should be done by bit-twiddling
! to avoid recursive IO
    read(s, *, iostat=ios) r
    if (ios/=0) r = 0
  end function vs_real

  subroutine destroy_vs(vs)
    character, dimension(:), pointer :: vs

    deallocate(vs)
  end subroutine destroy_vs

  subroutine devnull(in)
    character(len=*), intent(in) :: in
    ! just so we can call string functions for their side-effects
  end subroutine devnull

end module m_common_array_str

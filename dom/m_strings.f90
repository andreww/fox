module m_strings

  implicit none
  private

  type string
    character, pointer :: s(:)
  end type string

  interface operator(+)
    module procedure add_vs_vs, add_vs_s, add_s_vs
  end interface operator(+)

  interface operator(==)
    module procedure eq_s_vs, eq_vs_s, eq_vs_vs
  end interface operator(==)

  interface operator(/=)
    module procedure ne_s_vs, ne_vs_s, ne_vs_vs
  end interface operator(/=)

  interface assignment(=)
    module procedure ass_vs_s, ass_s_vs
  end interface assignment(=)
  
  interface index
    module procedure index_vs_vs, index_vs_s, index_s_vs
  end interface index

  interface len
    module procedure len_vs
  end interface len

  interface stringify
    module procedure stringify_vs, stringify_s, stringify_i
  end interface stringify

  interface substr
    module procedure substr_vs, substr_s
  end interface substr

  public :: string
  public :: unstring
  public :: operator(+)
  public :: operator(==)
  public :: operator(/=)
  public :: assignment(=)
  public :: stringify
  public :: len
  public :: index
  public :: substr
  public :: concatenate


contains

  subroutine unstring(s)
    type(string) :: s
    deallocate(s%s)
  end subroutine unstring

  function add_vs_vs (s1, s2) result (s12)
    type(string), intent(in) :: s1, s2
    type(string) :: s12
    allocate(s12%s(size(s1%s)+size(s2%s)))
    s12%s(:size(s1%s)) = s1%s
    s12%s(size(s1%s)+1:) = s2%s
  end function add_vs_vs

  function add_vs_s (s1, s2) result (s12)
    type(string), intent(in) :: s1
    character(len=*), intent(in) :: s2
    type(string) :: s12
    allocate(s12%s(size(s1%s)+len(s2)))
    s12%s(:size(s1%s)) = s1%s
    s12%s(size(s1%s)+1:) = transfer(s2, s12%s(size(s1%s)+1:))
  end function add_vs_s

  function add_s_vs (s1, s2) result (s12)
    character(len=*), intent(in) :: s1
    type(string), intent(in) :: s2
    type(string) :: s12
    allocate(s12%s(len(s1)+size(s2%s)))
    s12%s(:len(s1)) = transfer(s1, s12%s(:len(s1)))
    s12%s(len(s1)+1:) = s2%s
  end function add_s_vs

  subroutine ass_vs_s(s1, s2)
    type(string), intent(out) :: s1
    character(len=*), intent(in) :: s2
    allocate(s1%s(len(s2)))
    s1%s=transfer(s2,s1%s)
  end subroutine ass_vs_s

  subroutine ass_s_vs(s1, s2)
    type(string), intent(in) :: s2
    character(len=len(s2)), intent(out) :: s1
    s1 = transfer(s2%s, s1)
  end subroutine ass_s_vs

  function eq_s_vs(s1, s2) result(p)
    character(len=*), intent(in) :: s1
    type(string), intent(in) :: s2
    character(len=(size(s2%s))) :: s2s
    logical :: p
    s2s = transfer(s2%s,s2s)
    p = (s1==s2s)
  end function eq_s_vs

  function eq_vs_s(s2, s1) result(p)
    character(len=*), intent(in) :: s1
    type(string), intent(in) :: s2
    character(len=(size(s2%s))) :: s2s
    logical :: p
    s2s = transfer(s2%s,s2s)
    p = (s1==s2s)
  end function eq_vs_s

  function eq_vs_vs(s1, s2) result(p)
    type(string), intent(in) :: s1, s2
    logical :: p
    if (size(s1%s)==size(s2%s)) then
      p = all(s1%s==s2%s)
    else
      p = .false.
    endif
  end function eq_vs_vs

  function ne_s_vs(s1, s2) result(p)
    character(len=*), intent(in) :: s1
    type(string), intent(in) :: s2
    character(len=(size(s2%s))) :: s2s
    logical :: p
    s2s = transfer(s2%s,s2s)
    p = (s1/=s2s)
  end function ne_s_vs

  function ne_vs_s(s2, s1) result(p)
    character(len=*), intent(in) :: s1
    type(string), intent(in) :: s2
    character(len=(size(s2%s))) :: s2s
    logical :: p
    s2s = transfer(s2%s,s2s)
    p = (s1/=s2s)
  end function ne_vs_s

  function ne_vs_vs(s1, s2) result(p)
    type(string), intent(in) :: s1, s2
    logical :: p
    if (size(s1%s)==size(s2%s)) then
      p = all(s1%s/=s2%s)
    else
      p = .true.
    endif
  end function ne_vs_vs

  function stringify_vs(s1) result(s)
    type(string), intent(in) :: s1
    character(len=size(s1%s)) :: s
    s = transfer(s1%s,s)
  end function stringify_vs

  function stringify_s(s1) result(s)
    character(len=*), intent(in) :: s1
    character(len=len(s1)) :: s
    s = s1
  end function stringify_s

  function stringify_i(ind) result(s)
    integer, intent(in) :: ind
    type(string) :: s
    character(len=int(log10(real(max(abs(ind),1)))) + &
      merge(1, 2, ind>=0)) :: p
    write (p,'(i0)') ind
    s = p
  end function stringify_i
    
  elemental function len_vs(s) result(n)
    type(string), intent(in) :: s
    integer :: n
    if (associated(s%s)) then
      n = size(s%s)
    else
      n = 0
    endif
  end function len_vs

  function substr_s(s1, n1, n2) result(s2)
    character(len=*), intent(in) :: s1
    integer, intent(in) :: n1, n2
    type(string) :: s2
    integer :: i
    !if (n2 > len(s1)) n2 = len(s1)
    !if (n1 < 0) n1 = 0
    s2 = s1(n1:n2)
  end function substr_s

  function substr_vs(s1, n1, n2) result(s2)
    type(string), intent(in) :: s1
    integer, intent(in) :: n1, n2
    type(string) :: s2
    integer :: i
    !if (n2 > len(s1)) n2 = len(s1)
    !if (n1 < 0) n1 = 0
    allocate(s2%s(n2-n1+1))
    do i = n1, n2-1
      s2%s(i-n1+1) = s1%s(i)
    enddo
  end function substr_vs

  function index_vs_vs(vs1, vs2, back) result(n)
    type(string), intent(in) :: vs1, vs2
    logical, optional :: back
    integer :: n

    n = index(stringify(vs1), stringify(vs2), back)
  end function index_vs_vs

  function index_vs_s(vs1, s2, back) result(n)
    type(string), intent(in) :: vs1
    character(len=*), intent(in) :: s2
    logical, optional :: back
    integer :: n

    n = index(stringify(vs1), s2, back)
  end function index_vs_s

  function index_s_vs(s1, vs2, back) result(n)
    character(len=*), intent(in) :: s1
    type(string), intent(in) :: vs2
    logical, optional :: back
    integer :: n

    n = index(s1, stringify(vs2), back)
  end function index_s_vs


  function concatenate(s_array) result(vs)
    type(string), intent(in) :: s_array(:)
    type(string) :: vs

    integer :: i, vs_l

    vs_l = 0
    do i = 1, size(s_array)
      vs_l = vs_l + size(s_array(i)%s)
    enddo
    allocate(vs%s(vs_l))
    vs_l = 0
    do i = 1, size(s_array)
      vs%s(vs_l+1:vs_l+size(s_array(i)%s)) = s_array(i)%s
    enddo

  end function concatenate

end module m_strings

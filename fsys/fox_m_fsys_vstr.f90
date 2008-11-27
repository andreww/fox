module fox_m_fsys_vstr

implicit none

private

public :: vs
public :: new_vs
public :: destroy_vs
public :: add_chars
public :: as_chars
public :: operator(==)
public :: operator(/=)
public :: len

type vs
    private
    character, pointer :: chars(:) => null()
    integer :: str_len
    integer :: arr_len
end type vs

interface operator(==)
    module procedure comp_str_chars
    module procedure comp_chars_str
    module procedure comp_str_str
end interface operator(==)

interface operator(/=)
    module procedure diff_str_chars
    module procedure diff_chars_str
    module procedure diff_str_str
end interface operator(/=)

interface len
    module procedure vs_len
end interface len

contains

function new_vs(min_size, init_chars) result(my_vs)

    type(vs), pointer                         :: my_vs
    integer,          optional, intent(in)    :: min_size
    character(len=*), optional, intent(in)    :: init_chars
    integer                                   :: init_size

    init_size = 1
    if (present(init_chars)) then
       if (init_chars/="") init_size = len(init_chars)
    endif
    
    if (present(min_size)) then
       if (init_size.lt.min_size) init_size = min_size
    endif

    ! Set up a vs of the needed size
    allocate(my_vs)
    allocate(my_vs%chars(init_size))
    my_vs%arr_len = init_size
    my_vs%str_len = 0

    if (present(init_chars)) then
        if (init_chars/="") call add_chars(my_vs, init_chars)
    endif

end function new_vs

subroutine add_chars(my_vs, chars)

    type(vs),                   intent(inout) :: my_vs
    character(len=*),           intent(in)    :: chars
    integer                                   :: size_needed
    integer                                   :: i

 
    ! Check size and grow if needed.
    if ((my_vs%arr_len - my_vs%str_len) .lt. len(chars)) then
        size_needed = my_vs%arr_len * 2
        do
           if ((size_needed - my_vs%str_len) .gt. len(chars)) exit
           size_needed = size_needed * 2
        enddo
        call grow_vs(my_vs, newsize=size_needed)
    endif
        
    ! Add the chars # FIXME: use transfer? 
    do i = 1, len(chars)
        my_vs%chars(my_vs%str_len+i) = chars(i:i)
    enddo
    my_vs%str_len = my_vs%str_len + len(chars)

end subroutine add_chars   

pure function  as_chars(my_vs) result(chars)
 
    type(vs),                   intent(in)    :: my_vs
    character(len=my_vs%str_len)              :: chars
    integer                                   :: i

    do i = 1, my_vs%str_len
       chars(i:i) = my_vs%chars(i)
    enddo

end function as_chars

subroutine destroy_vs(my_vs)

    type(vs), pointer,              intent(inout) :: my_vs

    if (.not.associated(my_vs)) stop "This is wrong"

    deallocate(my_vs%chars)
    deallocate(my_vs)

end subroutine destroy_vs

function vs_len(my_vs) result (my_len)


    type(vs), intent(in)          :: my_vs
    integer                       :: my_len

    my_len = my_vs%str_len 

end function vs_len

subroutine grow_vs(my_vs, newsize)

    type(vs), intent(inout)       :: my_vs
    integer, optional, intent(in) :: newsize
    integer                       :: my_newsize

    character, pointer            :: new_chars(:) => null()
    character, pointer            :: old_chars(:) => null()

    if (present(newsize)) then
        if ((newsize.gt.my_vs%arr_len)) then
            my_newsize = newsize
        else
            stop "grow_vs: optional newsize argument is too small"
        endif
    else
       my_newsize = my_vs%arr_len * 2
    endif

   old_chars => my_vs%chars
   allocate(new_chars(my_newsize))
   new_chars(1:my_vs%arr_len) = old_chars(1:my_vs%arr_len)
   my_vs%chars => new_chars
   deallocate (old_chars)

   my_vs%arr_len = my_newsize

end subroutine grow_vs

pure function comp_str_chars(my_vs, chars) result(comp)

   type(vs), intent(in)         :: my_vs
   character(len=*), intent(in) :: chars
   logical                      :: comp

! For now, this is implemented ny casting to chars
! I think this can be optimied quite a lot.
   comp = (as_chars(my_vs)==chars)

end function comp_str_chars

pure function comp_chars_str(chars, my_vs) result(comp)

   type(vs), intent(in)         :: my_vs
   character(len=*), intent(in) :: chars
   logical                      :: comp

   comp = comp_str_chars(my_vs, chars)

end function comp_chars_str

pure function comp_str_str(my_vs_a, my_vs_b) result(comp)

   type(vs), intent(in)         :: my_vs_a
   type(vs), intent(in)         :: my_vs_b
   logical                      :: comp

! For now, this is implemented ny casting to chars
! I think this can be optimied quite a lot.
   comp = (as_chars(my_vs_a)==as_chars(my_vs_b))

end function comp_str_str

pure function diff_str_chars(my_vs, chars) result(comp)

   type(vs), intent(in)         :: my_vs
   character(len=*), intent(in) :: chars
   logical                      :: comp

! For now, this is implemented ny casting to chars
! I think this can be optimied quite a lot.
   comp = (as_chars(my_vs)/=chars)

end function diff_str_chars

pure function diff_chars_str(chars, my_vs) result(comp)

   type(vs), intent(in)         :: my_vs
   character(len=*), intent(in) :: chars
   logical                      :: comp

   comp = diff_str_chars(my_vs, chars)

end function diff_chars_str

pure function diff_str_str(my_vs_a, my_vs_b) result(comp)

   type(vs), intent(in)         :: my_vs_a
   type(vs), intent(in)         :: my_vs_b
   logical                      :: comp

! For now, this is implemented ny casting to chars
! I think this can be optimied quite a lot.
   comp = (as_chars(my_vs_a)/=as_chars(my_vs_b))

end function diff_str_str

end module fox_m_fsys_vstr

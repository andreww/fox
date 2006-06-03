module m_common_elstack

use m_common_error, only : FoX_error, FoX_fatal
use pxf

implicit none
private

!
! Simple stack to keep track of which elements have appeared so far
!
! Initial stack size:
integer, parameter, private            :: STACK_SIZE_INIT = 10
! Multiplier when stack is exceeded:
real, parameter, private            :: STACK_SIZE_MULT = 1.5

type, private :: elstack_item
  character, dimension(:), pointer :: data
end type

type, public :: elstack_t
private
  integer                                   :: n_items
  type(elstack_item), pointer, dimension(:) :: stack
end type elstack_t

public  :: push_elstack, pop_elstack, init_elstack, destroy_elstack, reset_elstack, print_elstack
public  :: get_top_elstack, is_empty, get_elstack_signature
public  :: len

interface len
  module procedure number_of_items
end interface
private :: number_of_items

interface is_empty
  module procedure is_empty_elstack
end interface
private :: is_empty_elstack

CONTAINS

!-----------------------------------------------------------------
subroutine init_elstack(elstack)
  type(elstack_t), intent(inout)  :: elstack

! We go from 0 (and initialize the 0th string to "")
! in order that we can safely check the top of an
! empty stack
  allocate(elstack%stack(0:STACK_SIZE_INIT))
  elstack%n_items = 0
  allocate(elstack%stack(0)%data(0))

end subroutine init_elstack

subroutine destroy_elstack(elstack)
  type(elstack_t), intent(inout)  :: elstack
  integer :: i
  do i = 0, elstack % n_items
    deallocate(elstack%stack(i)%data)
  enddo
  deallocate(elstack%stack)
end subroutine destroy_elstack

!-----------------------------------------------------------------
subroutine reset_elstack(elstack)
  type(elstack_t), intent(inout)  :: elstack
  integer :: i

  call destroy_elstack(elstack)
  call init_elstack(elstack)

end subroutine reset_elstack

!-----------------------------------------------------------------
subroutine resize_elstack(elstack)
  type(elstack_t), intent(inout)  :: elstack
  type(elstack_item), dimension(0:ubound(elstack%stack,1)) :: temp
  integer :: i, s, dataLength

  s = ubound(elstack%stack, 1)

  do i = 0, s
     temp(i)%data => elstack%stack(i)%data
  enddo
  deallocate(elstack%stack)
  allocate(elstack%stack(0:nint(s*STACK_SIZE_MULT)))
  do i = 0, s
     elstack%stack(i)%data => temp(i)%data
  enddo

end subroutine resize_elstack

!-----------------------------------------------------------------
function is_empty_elstack(elstack) result(answer)
type(elstack_t), intent(in)  :: elstack
logical                    :: answer

answer = (elstack%n_items == 0)
end function is_empty_elstack

!-----------------------------------------------------------------
function number_of_items(elstack) result(n)
type(elstack_t), intent(in)  :: elstack
integer                      :: n

n = elstack%n_items
end function number_of_items

!-----------------------------------------------------------------
subroutine push_elstack(item,elstack)
character(len=*), intent(in)      :: item
type(elstack_t), intent(inout)  :: elstack

integer :: n

n = elstack%n_items
n = n + 1
if (n == size(elstack%stack)) then
  call resize_elstack(elstack)
endif
allocate(elstack%stack(n)%data(len(item)))
elstack%stack(n)%data = transfer(item, elstack%stack(n)%data)
elstack%n_items = n

end subroutine push_elstack

!-----------------------------------------------------------------
function pop_elstack(elstack) result(item)
type(elstack_t), intent(inout)     :: elstack
character(len=merge(size(elstack%stack(elstack%n_items)%data), 0, elstack%n_items > 0)) :: item

integer :: n

n = elstack%n_items
if (n == 0) then
      call FoX_fatal("Element stack empty")
endif
item = transfer(elstack%stack(n)%data, item)
deallocate(elstack%stack(n)%data)
elstack%n_items = n - 1

end function pop_elstack

!-----------------------------------------------------------------
function get_top_elstack(elstack) result(item)
!
! Get the top element of the stack, *without popping it*.
!
type(elstack_t), intent(in)        :: elstack
character(len=merge(size(elstack%stack(elstack%n_items)%data), 0, elstack%n_items > 0)) :: item 

integer   :: i, n

n = elstack%n_items

if (n == 0) then
  call FoX_error("Element stack empty")
endif
item=""
do i=1, len(item)
   item(i:i)=elstack%stack(n)%data(i)
enddo
!item = transfer(elstack%stack(n)%data, item)

end function get_top_elstack

!-----------------------------------------------------------------
subroutine print_elstack(elstack,unit)
type(elstack_t), intent(in)   :: elstack
integer, intent(in)           :: unit
integer   :: i

do i = elstack%n_items, 1, -1
  write(unit=unit,fmt=*) elstack%stack(i)%data
enddo

end subroutine print_elstack

!-------------------------------------------------------------
function get_elstack_signature(elstack) result(string)
type(elstack_t), intent(in)   :: elstack
!FIXME TOHW character(len=sum(size(elstack%stack(:elstack%n_items)%data))+elstack%n_items) :: string
character(len=200) :: string
integer   :: i, length, j

j = 0
do i = 1, elstack%n_items
   length = size(elstack%stack(i)%data)
   string(j+1:j+1) = "/"
   j = j+1
   string(j+1:j+length) = transfer(elstack%stack(i)%data, string(j+1:j+length))
   j = j + length
enddo

end function get_elstack_signature

end module m_common_elstack

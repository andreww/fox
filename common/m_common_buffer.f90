module m_common_buffer

use m_common_error, only : FoX_error

implicit none
private

! At this point we use a fixed-size buffer. 
! Note however that buffer overflows will only be
! triggered by overly long *unbroken* pcdata values, or
! by overly long attribute values. Hopefully
! element or attribute names are "short enough".
! There is code in the parser module  m_fsm to avoid buffer overflows
! caused by pcdata values.
!
! This module is re-used from the parser package.
! Most of the routines are superfluous at this point.
!
! In a forthcoming implementation it could be made dynamical...

! MAX_BUFF_SIZE cannot be bigger than the maximum available
! record length for a compiler. In practice, this means
! 1024 seems to be the biggest available size.

integer, parameter :: MAX_BUFF_SIZE  = 1024
integer, parameter :: BUFF_SIZE_WARNING  = 0.9 * MAX_BUFF_SIZE

type buffer_t
private
      integer                       :: size
      character(len=MAX_BUFF_SIZE)  :: str
end type buffer_t


public :: MAX_BUFF_SIZE

public :: buffer_t

public :: add_to_buffer
public :: print_buffer, str, char, len
public :: operator (.equal.)
public :: buffer_to_character
public :: buffer_nearly_full, reset_buffer

!----------------------------------------------------------------
interface add_to_buffer
      module procedure add_str_to_buffer
end interface
private :: add_char_to_buffer, add_str_to_buffer

interface operator (.equal.)
      module procedure compare_buffers, compare_buffer_str, &
             compare_str_buffer
end interface
private :: compare_buffers, compare_buffer_str, compare_str_buffer

interface str
      module procedure buffer_to_str
end interface
interface char                 ! Experimental
      module procedure buffer_to_str
end interface
private :: buffer_to_str

interface len
   module procedure buffer_length
end interface
private :: buffer_length

CONTAINS
!==================================================================

!----------------------------------------------------------------
function compare_buffers(a,b) result(equal)     ! .equal. generic
type(buffer_t), intent(in)  :: a
type(buffer_t), intent(in)  :: b
logical                     :: equal

equal = ((a%size == b%size) .and. (a%str(1:a%size) == b%str(1:b%size)))

end function compare_buffers

!----------------------------------------------------------------
function compare_buffer_str(buffer,str) result(equal) ! .equal. generic
type(buffer_t), intent(in)   :: buffer
character(len=*), intent(in) :: str
logical                      :: equal

equal = (buffer%str(1:buffer%size) == str)

end function compare_buffer_str

!----------------------------------------------------------------
function compare_str_buffer(str,buffer) result(equal) ! .equal. generic
character(len=*), intent(in) :: str
type(buffer_t), intent(in)   :: buffer
logical                     :: equal

equal = (buffer%str(1:buffer%size) == str)

end function compare_str_buffer

!----------------------------------------------------------------
subroutine add_char_to_buffer(c,buffer)
character(len=1), intent(in)   :: c
type(buffer_t), intent(inout)  :: buffer

integer   :: n
buffer%size = buffer%size + 1
n = buffer%size

if (n > MAX_BUFF_SIZE) then
  call FoX_error("Buffer overflow: long unbroken string of pcdata or attribute value...")
endif

buffer%str(n:n) = c
end subroutine add_char_to_buffer

!------ ----------------------------------------------------------
subroutine add_str_to_buffer(s,buffer)
character(len=*), intent(in)   :: s
type(buffer_t), intent(inout)  :: buffer

integer   :: n, len_s, last_pos

len_s = len(s)
last_pos = buffer%size
buffer%size = buffer%size + len_s
n = buffer%size

if (n> MAX_BUFF_SIZE) then
  call FoX_error("Buffer overflow: long unbroken string of pcdata or attribute value.")
endif

buffer%str(last_pos+1:n) = s
end subroutine add_str_to_buffer

subroutine add_to_buffer_escaping_markup(s,buf)
character(len=*), intent(in)      ::   s
type(buffer_t), intent(inout)     ::   buf

integer           :: len_s, i
character(len=1)  :: c

len_s = len(s)
i = 0
do 
 if (i==len_s) exit
 i = i + 1
 c = s(i:i)
 if (c == "<") then
    call add_to_buffer("&lt;",buf)
 else if (c == "&") then
    call add_to_buffer("&amp;",buf)
 else if (c == "'") then
    call add_to_buffer("&quot;",buf)
 else if (c == '"') then
    call add_to_buffer("&apos;",buf)
 else
    call add_to_buffer(c,buf)
 endif
enddo

end subroutine add_to_buffer_escaping_markup

!----------------------------------------------------------------
subroutine reset_buffer(buffer)
type(buffer_t), intent(inout)  :: buffer

buffer%size = 0
buffer%str=""

end subroutine reset_buffer

!----------------------------------------------------------------
subroutine print_buffer(buffer)
type(buffer_t), intent(in)  :: buffer

integer :: i

do i = 1, buffer%size
      write(unit=6,fmt="(a1)",advance="no") buffer%str(i:i)
enddo

end subroutine print_buffer
!----------------------------------------------------------------
! This is better... but could it lead to memory leaks?
!
function buffer_to_str(buffer) result(str)
type(buffer_t), intent(in)          :: buffer
character(len=buffer%size)          :: str

str = buffer%str(1:buffer%size)
end function buffer_to_str
!----------------------------------------------------------------
!
subroutine buffer_to_character(buffer,str)
type(buffer_t), intent(in)          :: buffer
character(len=*), intent(out)       :: str

str = buffer%str(1:buffer%size)
end subroutine buffer_to_character
!----------------------------------------------------------------
function buffer_nearly_full(buffer) result(warn)
type(buffer_t), intent(in)          :: buffer
logical                             :: warn

warn = buffer%size > BUFF_SIZE_WARNING

end function buffer_nearly_full

!----------------------------------------------------------------
function buffer_length(buffer) result(length)
type(buffer_t), intent(in)          :: buffer
integer                             :: length

length = buffer%size 

end function buffer_length


end module m_common_buffer

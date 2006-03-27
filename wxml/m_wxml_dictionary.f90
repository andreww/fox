module m_wxml_dictionary

  use m_wxml_escape, only : check_Name
  use m_wxml_error,  only : wxml_fatal
  use m_wxml_array_str

  implicit none

private
!
integer, parameter, private    :: MAX_ITEMS = 30

type, private :: dict_item
  character(len=1), pointer, dimension(:) :: key
  character(len=1), pointer, dimension(:) :: value
end type dict_item

type, public :: wxml_dictionary_t
private
      integer                               :: number_of_items ! = 0
      type(dict_item), dimension(MAX_ITEMS) :: items
end type wxml_dictionary_t

!
! Building procedures
!
public :: init_dict
public :: reset_dict
public :: add_item_to_dict
!
! Query and extraction procedures
!
public  :: len
interface len
   module procedure number_of_entries
end interface
public  :: number_of_entries
public  :: get_key 
public  :: get_value
public  :: has_key
public  :: print_dict
!
interface get_value
   module procedure get_value_by_key, get_value_by_index
end interface

CONTAINS

!------------------------------------------------------
function number_of_entries(dict) result(n)
type(wxml_dictionary_t), intent(in)   :: dict
integer                          :: n

n = dict%number_of_items

end function number_of_entries

!------------------------------------------------------
function has_key(dict,key) result(found)
type(wxml_dictionary_t), intent(in)   :: dict
character(len=*), intent(in)     :: key
logical                          :: found

integer  ::  i
found = .false.
do  i = 1, dict%number_of_items
    if (dict%items(i)%key .equal. key) then
      found = .true.
      exit
    endif
enddo
end function has_key

pure function get_key_index(dict,key) result(ind)
type(wxml_dictionary_t), intent(in)   :: dict
character(len=*), intent(in)     :: key
integer                          :: ind

integer  ::  i
ind = 0
do  i = 1, dict%number_of_items
    if (dict%items(i)%key .equal. key) then
      ind = i
      exit
    endif
enddo
end function get_key_index

!------------------------------------------------------
function get_value_by_key(dict,key,status) result(value)
type(wxml_dictionary_t), intent(in)       :: dict
character(len=*), intent(in)              :: key
integer, optional, intent(out)            :: status
character(len = merge(size(dict%items(get_key_index(dict, key))%value), 0, (get_key_index(dict, key) > 0))) :: value
!
integer  :: i

if (present(status)) status = -1
do  i = 1, dict%number_of_items
    if (dict%items(i)%key .equal. key) then
      call assign_array_to_str(value,dict%items(i)%value)
      if(present(status)) status = 0
      exit
    endif
enddo

end function get_value_by_key

function get_value_by_index(dict,i,status) result(value)
type(wxml_dictionary_t), intent(in)       :: dict
integer, intent(in)                       :: i
integer, optional, intent(out)            :: status
character(len = merge(size(dict%items(i)%value), 0, (i>0 .and. i<=dict%number_of_items))) :: value

if (i>0 .and. i<=dict%number_of_items) then
  call assign_array_to_str(value,dict%items(i)%value)
  if (present(status)) status = 0
else
  if (present(status)) status = -1
endif

end function get_value_by_index

function get_key(dict, i, status) result(key)
!
! return the ith key
!
type(wxml_dictionary_t), intent(in)       :: dict
integer, intent(in)                       :: i
integer, optional, intent(out)            :: status
character(len = merge(size(dict%items(i)%key), 0, (i>0 .and. i<=dict%number_of_items))) :: key

if (i>0 .and. i<=dict%number_of_items)then
  call assign_array_to_str(key,dict%items(i)%key)
  if (present(status)) status = 0
else
  if (present(status)) status = -1
endif

end function get_key

subroutine add_item_to_dict(dict, key, value)

  type(wxml_dictionary_t), intent(inout) :: dict
  character(len=*), intent(in)          :: key
  character(len=*), intent(in)          :: value

  integer  :: n

  n = dict%number_of_items
  if (n == MAX_ITEMS) then
    write(unit=0,fmt=*) "Dictionary capacity exceeded !"
    RETURN
  endif

  if (.not.check_Name(key)) then
    call wxml_fatal('attribute name is invalid')
  endif

  n = n + 1
  allocate(dict%items(n)%key(len(key)))
  call assign_str_to_array(dict%items(n)%key,key)
  allocate(dict%items(n)%value(len(value)))
  call assign_str_to_array(dict%items(n)%value,value)

  dict%number_of_items = n

end subroutine add_item_to_dict

!------------------------------------------------------
subroutine init_dict(dict)
  type(wxml_dictionary_t), intent(out)   :: dict

  integer :: i
  
  do i = 1, MAX_ITEMS
    nullify(dict%items(i)%key)
    nullify(dict%items(i)%key)
  enddo

  dict % number_of_items = 0

end subroutine init_dict
  
!------------------------------------------------------
subroutine reset_dict(dict)
  type(wxml_dictionary_t), intent(inout)   :: dict
  
  integer :: i
  do i = 1, dict%number_of_items
    deallocate(dict%items(i)%key)
    deallocate(dict%items(i)%value)
  enddo

  dict%number_of_items = 0

end subroutine reset_dict

!------------------------------------------------------
subroutine print_dict(dict)
type(wxml_dictionary_t), intent(in)   :: dict

integer  :: i

do i = 1, dict%number_of_items
      print *, dict%items(i)%key, " = ", dict%items(i)%value
enddo

end subroutine print_dict

end module m_wxml_dictionary

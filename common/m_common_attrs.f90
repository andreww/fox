module m_common_attrs

  !use m_wxml_escape, only : check_Name
  use m_common_array_str, only : assign_str_to_array, assign_array_to_str
  use m_common_error, only : FoX_error

  implicit none
  private
  
  !Initial length of dictionary
  integer, parameter, private    :: DICT_INIT_LEN = 10 
  !Multiplier if we need to extend it.
  real, parameter, private       :: DICT_LEN_MULT = 1.5

  type dict_item
     character(len=1), pointer, dimension(:) :: nsURI
     character(len=1), pointer, dimension(:) :: localName
     character(len=1), pointer, dimension(:) :: key
     character(len=1), pointer, dimension(:) :: value
  end type dict_item
  
  type dictionary_t
     private
     integer                                :: number_of_items ! = 0
     type(dict_item), dimension(:), pointer :: items
  end type dictionary_t

  public :: dictionary_t

  ! Building procedures
  
  public :: init_dict
  public :: reset_dict
  public :: add_item_to_dict
  public :: destroy_dict
  !these last two only because SAX needs them
  public :: add_key_to_dict
  public :: add_value_to_dict

  ! Query and extraction procedures
  
  public  :: len
  public  :: get_key 
  public  :: get_value
  public  :: remove_key
  public  :: has_key
  public  :: print_dict

  ! Namespaces
  public :: get_nsURI
  public :: get_localName
  public :: set_nsURI
  public :: set_localName
  
 
  interface len
     module procedure number_of_entries
  end interface
  interface get_value
     module procedure get_value_by_key, get_value_by_index
  end interface
  interface remove_key
     module procedure remove_key_by_index
  end interface

  interface get_nsURI
     module procedure get_nsURI_by_index
  end interface
  interface get_localName
     module procedure get_localName_by_index
  end interface
  interface set_nsURI
     module procedure set_nsURI_by_index
  end interface
  interface set_localName
     module procedure set_localName_by_index_s
     module procedure set_localName_by_index_vs
  end interface
  
contains

  !------------------------------------------------------
  function number_of_entries(dict) result(n)
    type(dictionary_t), intent(in)   :: dict
    integer                          :: n
    
    n = dict%number_of_items
    
  end function number_of_entries
  
  !------------------------------------------------------
  function has_key(dict,key) result(found)
    type(dictionary_t), intent(in)   :: dict
    character(len=*), intent(in)     :: key
    logical                          :: found
    
    integer  ::  i
    found = .false.
    do  i = 1, dict%number_of_items
       if (key == transfer(dict%items(i)%key,key)) then
          found = .true.
          exit
       endif
    enddo
  end function has_key
  
  pure function get_key_index(dict,key) result(ind)
    type(dictionary_t), intent(in)   :: dict
    character(len=*), intent(in)     :: key
    integer                          :: ind
    
    integer  ::  i
    ind = 0
    do  i = 1, dict%number_of_items
       if (key == transfer(dict%items(i)%key,key)) then
          ind = i
          exit
       endif
    enddo
  end function get_key_index
  
  !------------------------------------------------------
  function get_value_by_key(dict,key,status) result(value)
    type(dictionary_t), intent(in)       :: dict
    character(len=*), intent(in)              :: key
    integer, optional, intent(out)            :: status
    character(len = merge(size(dict%items(get_key_index(dict, key))%value), 0, (get_key_index(dict, key) > 0))) :: value
    !
    integer  :: i
    
    if (present(status)) status = -1
    do  i = 1, dict%number_of_items
       if (key == transfer(dict%items(i)%key, key)) then
          call assign_array_to_str(value,dict%items(i)%value)
          if (present(status)) status = 0
          exit
       endif
    enddo

  end function get_value_by_key

  subroutine remove_key_by_index(dict, key, status)
    type(dictionary_t), intent(inout)       :: dict
    integer, intent(in)                     :: key
    integer, optional, intent(out)          :: status
    
    integer :: i
    type(dict_item), dimension(dict%number_of_items-1) :: tempDict

    if (i<0 .or. i>dict%number_of_items) then
       if (present(status)) status = 0
    else
       if (present(status)) status = 1
    endif

    do i = 1, key-1
       tempDict(i)%key => dict%items(i)%key
       tempDict(i)%value => dict%items(i)%value
       tempDict(i)%nsURI => dict%items(i)%nsURI
       tempDict(i)%localName => dict%items(i)%localName
    enddo
    deallocate(dict%items(i)%key)
    deallocate(dict%items(i)%value)
    deallocate(dict%items(i)%nsURI)
    deallocate(dict%items(i)%localName)
    do i = key+1, dict%number_of_items
       tempDict(i-1)%key => dict%items(i)%key
       tempDict(i-1)%value => dict%items(i)%value
       tempDict(i-1)%nsURI => dict%items(i)%nsURI
       tempDict(i-1)%localName => dict%items(i)%localName
    enddo
    !NB we don't resize here, because dictionaries only get
    !resized with MULT and LEN and stuff here.
    dict%number_of_items = dict%number_of_items - 1
    do i = 1, dict%number_of_items
       dict%items(i)%key => tempDict(i)%key
       dict%items(i)%value => tempDict(i)%value
       dict%items(i)%nsURI => tempDict(i)%nsURI
       dict%items(i)%localName => tempDict(i)%localName
    enddo
  end subroutine remove_key_by_index
  
  function get_value_by_index(dict,i,status) result(value)
    type(dictionary_t), intent(in)       :: dict
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
    type(dictionary_t), intent(in)       :: dict
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
    
    type(dictionary_t), intent(inout) :: dict
    character(len=*), intent(in)          :: key
    character(len=*), intent(in)          :: value
    
    integer  :: n
    
    if (has_key(dict, key)) then
       call Fox_Error('Duplicate attribute')
    endif
    
    n = dict%number_of_items
    if (n == size(dict%items)) then
       call resize_dict(dict)
    endif
    
    !if (.not.check_Name(key)) then
    !  call wxml_fatal('attribute name is invalid')
    !endif
    
    n = n + 1
    allocate(dict%items(n)%key(len(key)))
    call assign_str_to_array(dict%items(n)%key,key)
    allocate(dict%items(n)%value(len(value)))
    call assign_str_to_array(dict%items(n)%value,value)
    allocate(dict%items(n)%nsURI(0))
    allocate(dict%items(n)%localName(0))
    
    dict%number_of_items = n
    
  end subroutine add_item_to_dict
  
  subroutine add_key_to_dict(dict, key)
    type(dictionary_t), intent(inout) :: dict
    character(len=*), intent(in)      :: key

    integer  :: n

    if (has_key(dict, key)) then
       call FoX_error('Duplicate attribute')
    endif

    n = dict%number_of_items
    if (n == size(dict%items)) then
       call resize_dict(dict)
    endif

    !if (.not.check_Name(key)) then
    !  call wxml_fatal('attribute name is invalid')
    !endif

    n = n + 1
    !call pxfflush(6)
    allocate(dict%items(n)%key(len(key)))
    call assign_str_to_array(dict%items(n)%key,key)
    dict%number_of_items = n
  end subroutine add_key_to_dict

  subroutine add_value_to_dict(dict, value)
    type(dictionary_t), intent(inout) :: dict
    character(len=*), intent(in)      :: value

    integer  :: n
    n = dict%number_of_items

    allocate(dict%items(n)%value(len(value)))
    call assign_str_to_array(dict%items(n)%value,value)
    allocate(dict%items(n)%nsURI(0))
    allocate(dict%items(n)%localName(0))
  end subroutine add_value_to_dict

  subroutine set_nsURI_by_index(dict, i, nsURI)
    type(dictionary_t), intent(inout) :: dict
    integer, intent(in) :: i
    character(len=*) :: nsURI

    if (associated(dict%items(i)%nsURI)) &
         deallocate(dict%items(i)%nsURI)
    allocate(dict%items(i)%nsURI(len(nsURI)))
    dict%items(i)%nsURI = transfer(nsURI, dict%items(i)%nsURI)
  end subroutine set_nsURI_by_index

  subroutine set_localName_by_index_s(dict, i, localName)
    type(dictionary_t), intent(inout) :: dict
    integer, intent(in) :: i
    character(len=*) :: localName

    if (associated(dict%items(i)%localName)) &
         deallocate(dict%items(i)%localName)
    allocate(dict%items(i)%localName(len(localName)))
    dict%items(i)%localName = transfer(localName, dict%items(i)%localName)
  end subroutine set_localName_by_index_s

  subroutine set_localName_by_index_vs(dict, i, localName)
    type(dictionary_t), intent(inout) :: dict
    integer, intent(in) :: i
    character(len=1), dimension(:) :: localName

    if (associated(dict%items(i)%localName)) &
         deallocate(dict%items(i)%localName)
    allocate(dict%items(i)%localName(size(localName)))
    dict%items(i)%localName = localName
  end subroutine set_localName_by_index_vs

  pure function get_nsURI_by_index(dict, i) result(nsURI)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    character(len=size(dict%items(i)%nsURI)) :: nsURI
    
    nsURI = transfer(dict%items(i)%nsURI, nsURI)
  end function get_nsURI_by_index

  pure function get_localName_by_index(dict, i) result(localName)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    character(len=size(dict%items(i)%localName)) :: localName
    
    localName = transfer(dict%items(i)%localName, localName)
  end function get_localName_by_index
    
  pure function get_nsURI_by_keyname(dict, keyname) result(nsURI)
    type(dictionary_t), intent(in) :: dict
    character(len=*), intent(in) :: keyname
    character(len=merge(size(dict%items(get_key_index(dict, keyname))%nsURI), 0, (get_key_index(dict, keyname) > 0))) :: nsURI
    integer :: i

    i=get_key_index(dict, keyname)
    nsURI = transfer(dict%items(i)%nsURI, nsURI)

  end function get_nsURI_by_keyname

  pure function get_localName_by_keyname(dict, keyname) result(localName)
    type(dictionary_t), intent(in) :: dict
    character(len=*), intent(in) :: keyname
    character(len=merge(size(dict%items(get_key_index(dict, keyname))%localName), &
         0, (get_key_index(dict, keyname) > 0))) :: localName
    integer :: i

    i=get_key_index(dict, keyname)
    localName = transfer(dict%items(i)%localName, localName)

  end function get_localName_by_keyname

  !------------------------------------------------------
  subroutine init_dict(dict)
    type(dictionary_t), intent(out)   :: dict
    
    integer :: i
    
    allocate(dict%items(DICT_INIT_LEN))
    do i = 1, DICT_INIT_LEN
       nullify(dict%items(i)%key)
       nullify(dict%items(i)%value)
       nullify(dict%items(i)%nsURI)
       nullify(dict%items(i)%localName)
    enddo
    
    dict % number_of_items = 0
    
  end subroutine init_dict

  subroutine resize_dict(dict)
    type(dictionary_t), intent(inout) :: dict
    type(dict_item), dimension(size(dict%items)) :: tempDict
    integer :: i, l_d_new, l_d_old

    l_d_old = size(dict%items)
    do i = 1, l_d_old
       tempDict(i)%key => dict%items(i)%key
       tempDict(i)%value => dict%items(i)%value
       tempDict(i)%nsURI => dict%items(i)%nsURI
       tempDict(i)%localName => dict%items(i)%localName
    enddo
    deallocate(dict%items)
    l_d_new = l_d_old * DICT_LEN_MULT
    allocate(dict%items(l_d_new))
    do i = 1, l_d_old
       dict%items(i)%key => tempDict(i)%key
       dict%items(i)%value => tempDict(i)%value
       dict%items(i)%nsURI => tempDict(i)%nsURI
       dict%items(i)%localName => tempDict(i)%localName
    enddo

  end subroutine resize_dict

  subroutine destroy_dict(dict)
    type(dictionary_t), intent(inout) :: dict
    integer :: i
    do i = 1, dict%number_of_items
       deallocate(dict%items(i)%key)
       deallocate(dict%items(i)%value)
       deallocate(dict%items(i)%nsURI)
       deallocate(dict%items(i)%localName)
    enddo
    deallocate(dict%items)
  end subroutine destroy_dict
  
  !------------------------------------------------------
  subroutine reset_dict(dict)
    type(dictionary_t), intent(inout)   :: dict
    
    integer :: i
    do i = 1, dict%number_of_items
       deallocate(dict%items(i)%key)
       deallocate(dict%items(i)%value)
       deallocate(dict%items(i)%nsURI)
       deallocate(dict%items(i)%localName)
    enddo
    
    dict%number_of_items = 0
    
  end subroutine reset_dict
  
  !------------------------------------------------------
  subroutine print_dict(dict)
    type(dictionary_t), intent(in)   :: dict
    
    integer  :: i
    
    do i = 1, dict%number_of_items
       print*, dict%items(i)%key, " = {", dict%items(i)%nsURI, '}', dict%items(i)%localName
    enddo
    
  end subroutine print_dict
  
end module m_common_attrs

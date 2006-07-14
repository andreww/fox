module m_common_attrs

  !use m_wxml_escape, only : check_Name
  use m_common_array_str, only : str_vs, vs_str
  use m_common_charset, only : whitespace, initialNameChars, nameChars, operator(.in.)
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
     character(len=1), pointer, dimension(:) :: prefix
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

  public :: parse_string_to_dict

  ! Query and extraction procedures
  
  public  :: len
  public  :: get_key 
  public  :: get_value
  public  :: remove_key
  public  :: has_key
  public  :: print_dict

  ! Namespaces
  public :: get_nsURI
  public :: get_prefix
  public :: get_localName
  public :: set_nsURI
  public :: set_prefix
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
  interface get_prefix
     module procedure get_prefix_by_index
  end interface
  interface get_localName
     module procedure get_localName_by_index
  end interface
  interface set_nsURI
     module procedure set_nsURI_by_index
  end interface
  interface set_prefix
     module procedure set_prefix_by_index
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
          value = str_vs(dict%items(i)%value)
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
       tempDict(i)%prefix => dict%items(i)%prefix
       tempDict(i)%localName => dict%items(i)%localName
    enddo
    deallocate(dict%items(i)%key)
    deallocate(dict%items(i)%value)
    deallocate(dict%items(i)%nsURI)
    deallocate(dict%items(i)%prefix)
    deallocate(dict%items(i)%localName)
    do i = key+1, dict%number_of_items
       tempDict(i-1)%key => dict%items(i)%key
       tempDict(i-1)%value => dict%items(i)%value
       tempDict(i-1)%nsURI => dict%items(i)%nsURI
       tempDict(i-1)%prefix => dict%items(i)%prefix
       tempDict(i-1)%localName => dict%items(i)%localName
    enddo
    !NB we don't resize here, because dictionaries only get
    !resized with MULT and LEN and stuff here.
    dict%number_of_items = dict%number_of_items - 1
    do i = 1, dict%number_of_items
       dict%items(i)%key => tempDict(i)%key
       dict%items(i)%value => tempDict(i)%value
       dict%items(i)%nsURI => tempDict(i)%nsURI
       dict%items(i)%prefix => tempDict(i)%prefix
       dict%items(i)%localName => tempDict(i)%localName
    enddo
  end subroutine remove_key_by_index
  
  function get_value_by_index(dict,i,status) result(value)
    type(dictionary_t), intent(in)       :: dict
    integer, intent(in)                       :: i
    integer, optional, intent(out)            :: status
    character(len = merge(size(dict%items(i)%value), 0, (i>0 .and. i<=dict%number_of_items))) :: value
    
    if (i>0 .and. i<=dict%number_of_items) then
       value = str_vs(dict%items(i)%value)
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
       key = str_vs(dict%items(i)%key)
       if (present(status)) status = 0
    else
       if (present(status)) status = -1
    endif
    
  end function get_key
  
  subroutine add_item_to_dict(dict, key, value, prefix, nsURI)
    
    type(dictionary_t), intent(inout) :: dict
    character(len=*), intent(in)           :: key
    character(len=*), intent(in)           :: value
    character(len=*), intent(in), optional :: prefix
    character(len=*), intent(in), optional :: nsURI
    
    integer  :: n
    
    if (has_key(dict, key)) then
       call Fox_Error('Duplicate attribute')
    endif

    if (present(prefix) .eqv. .not.present(nsURI)) &
       call Fox_Error('Namespace improperly specified')
    
    n = dict%number_of_items
    if (n == size(dict%items)) then
       call resize_dict(dict)
    endif
    
    !if (.not.check_Name(key)) then
    !  call wxml_fatal('attribute name is invalid')
    !endif
    
    n = n + 1
    allocate(dict%items(n)%value(len(value)))
    dict%items(n)%value = vs_str(value)
    if (present(prefix)) then
      allocate(dict%items(n)%key(len(prefix)+1+len(key)))
      dict%items(n)%key = vs_str(prefix//":"//key)
      allocate(dict%items(n)%prefix(len(prefix)))
      dict%items(n)%prefix = vs_str(prefix)
      allocate(dict%items(n)%nsURI(len(nsURI)))
      dict%items(n)%nsURI = vs_str(nsURI)
      allocate(dict%items(n)%localname(len(key)))
      dict%items(n)%localname = vs_str(key)
    else
      allocate(dict%items(n)%key(len(key)))
      dict%items(n)%key = vs_str(key)
      allocate(dict%items(n)%localname(len(key)))
      dict%items(n)%localname = vs_str(key)
      allocate(dict%items(n)%prefix(0))
      allocate(dict%items(n)%nsURI(0))
    endif
    
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
    dict%items(n)%key = vs_str(key)
    dict%number_of_items = n
  end subroutine add_key_to_dict

  subroutine add_value_to_dict(dict, value)
    type(dictionary_t), intent(inout) :: dict
    character(len=*), intent(in)      :: value

    integer  :: n
    n = dict%number_of_items

    allocate(dict%items(n)%value(len(value)))
    dict%items(n)%value = vs_str(value)
    allocate(dict%items(n)%prefix(0))
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

  subroutine set_prefix_by_index(dict, i, prefix)
    type(dictionary_t), intent(inout) :: dict
    integer, intent(in) :: i
    character(len=*) :: prefix

    if (associated(dict%items(i)%prefix)) &
         deallocate(dict%items(i)%prefix)
    allocate(dict%items(i)%nsURI(len(prefix)))
    dict%items(i)%prefix = vs_str(prefix)
  end subroutine set_prefix_by_index

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

  pure function get_prefix_by_index(dict, i) result(prefix)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    character(len=size(dict%items(i)%prefix)) :: prefix
    
    prefix = transfer(dict%items(i)%prefix, prefix)
  end function get_prefix_by_index

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

  pure function get_prefix_by_keyname(dict, keyname) result(prefix)
    type(dictionary_t), intent(in) :: dict
    character(len=*), intent(in) :: keyname
    character(len=merge(size(dict%items(get_key_index(dict, keyname))%prefix), 0, (get_key_index(dict, keyname) > 0))) :: prefix
    integer :: i

    i=get_key_index(dict, keyname)
    prefix = str_vs(dict%items(i)%prefix)

  end function get_prefix_by_keyname

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
       deallocate(dict%items(i)%prefix)
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
       deallocate(dict%items(i)%prefix)
       deallocate(dict%items(i)%localName)
    enddo
    
    dict%number_of_items = 0
    
  end subroutine reset_dict
  
  !------------------------------------------------------
  subroutine print_dict(dict)
    type(dictionary_t), intent(in)   :: dict
    
    integer  :: i
    
    do i = 1, dict%number_of_items
       write(*,'(7a)') str_vs(dict%items(i)%key), " [ {", str_vs(dict%items(i)%nsURI), &
          "}", str_vs(dict%items(i)%localName), " ]  = ", str_vs(dict%items(i)%value)
    enddo
    
  end subroutine print_dict

  subroutine parse_string_to_dict(string, dict, status)
    character(len=*), intent(in) :: string
    type(dictionary_t), intent(out) :: dict
    integer, intent(out) :: status

    !Parse a string to a dictionary of (namespace-unaware) attributes.
    ! status will be non-zero if we have an error

    integer :: i, n, state
    character(len=1) :: c, quotechar
    character(len=1), pointer :: name(:)
    character(len=1), pointer :: value(:)
    character(len=1), pointer :: tmp(:)

    integer, parameter :: OUTSIDE            = 0
    integer, parameter :: IN_NAME            = 1
    integer, parameter :: WAITING_FOR_EQUALS = 2
    integer, parameter :: FOUND_EQUALS       = 3
    integer, parameter :: IN_VALUE           = 4
    integer, parameter :: DONE_VALUE         = 5
    
    allocate(name(0))
    allocate(value(0))

    call init_dict(dict)

    state = OUTSIDE

    do i = 1, len(string)
      c = string(i:i)
      select case (state)
      case (OUTSIDE)
        if (c .in. whitespace) then
          cycle
        elseif (c .in. initialNameChars) then
          state = IN_NAME
          deallocate(name)
          allocate(name(1))
          name(1) = c
        else
          status = i
          exit
        endif
      case (IN_NAME)
        if (c.in.nameChars) then
          n = size(name)
          tmp => name
          allocate(name(n+1))
          name(:n) = tmp
          deallocate(tmp)
          name(n+1) = c
        elseif (c.in.whitespace) then
          state = WAITING_FOR_EQUALS
        elseif (c == '=') then
          state = FOUND_EQUALS
        else
          status = i
          exit
        endif
      case (WAITING_FOR_EQUALS)
        if (c.in.whitespace) then
          cycle
        elseif (c == '=') then
          state = FOUND_EQUALS
        else
          status = i
          exit
        endif
      case (FOUND_EQUALS)
        if (c.in.whitespace) then
          cycle
        elseif (c == "'") then
          quotechar = "'"
          state = IN_VALUE
        elseif (c == '"') then
          quotechar = '"'
          state = IN_VALUE
        else
          status = i
          exit
        endif
      case (IN_VALUE)  
        if (c == quotechar) then
          call add_item_to_dict(dict, str_vs(name), str_vs(value))
          deallocate(name)
          allocate(name(0))
          deallocate(value)
          allocate(value(0))
          state = DONE_VALUE
        elseif (c == '<') then
          status = i
          exit
          ! We do not check for & here - leave that for elsewhere
        else
          n = size(value)
          tmp => value
          allocate(value(n+1))
          value(:n) = tmp
          deallocate(tmp)
          value(n+1) = c
        endif
      case (DONE_VALUE)
        if (c.in.whitespace) then
          state = OUTSIDE
        else
          status = i
          exit
        endif
      end select
    enddo

    if (state /= OUTSIDE .and. state /= DONE_VALUE) then
      status = len(string)
      call reset_dict(dict)
    else
      status = 0
    endif

    deallocate(name)
    deallocate(value)
          
  end subroutine parse_string_to_dict
    
  
end module m_common_attrs

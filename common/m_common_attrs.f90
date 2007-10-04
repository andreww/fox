module m_common_attrs

  use m_common_array_str, only : str_vs, vs_str_alloc
  use m_common_element, only: ATT_CDATA, ATT_ID, ATT_IDREF, &
    ATT_IDREFS, ATT_ENTITY, ATT_ENTITIES, ATT_NMTOKEN, &
    ATT_NMTOKENS, ATT_NOTATION, ATT_ENUM, ATT_CDANO, ATT_CDAMB, &
    ATT_TYPELENGTHS
  use m_common_error, only : FoX_error, FoX_fatal

  implicit none
  private

  !Initial length of dictionary
  integer, parameter :: DICT_INIT_LEN = 10 
  !Multiplier if we need to extend it.
  real, parameter :: DICT_LEN_MULT = 1.5

  type dict_item
    character(len=1), pointer, dimension(:) :: nsURI => null()
    character(len=1), pointer, dimension(:) :: localName => null()
    character(len=1), pointer, dimension(:) :: prefix => null()
    character(len=1), pointer, dimension(:) :: key => null()
    character(len=1), pointer, dimension(:) :: value => null()
    logical :: specified
    integer :: type = 11
  end type dict_item

  type dictionary_t
    private
    integer                                :: number_of_items = 0
    type(dict_item), dimension(:), pointer :: items => null()
  end type dictionary_t

  public :: dictionary_t

  ! Building procedures

  public :: init_dict
  public :: reset_dict
  public :: add_item_to_dict
  public :: destroy_dict

  ! Query and extraction procedures

  ! SAX names:
  public :: getIndex
  public :: getLength
  public :: getLocalName
  public :: getQName
  public :: getURI
  public :: getValue
  public :: getType
  public :: getSpecified
  public :: hasKey

  public :: len
  public :: get_key 
  public :: get_value
  public :: remove_key
  public :: has_key
  public :: print_dict

  ! Namespaces
  public :: get_prefix
  public :: get_localName
  public :: set_nsURI
  public :: set_prefix
  public :: set_localName

  ! For internal FoX use only:
  public :: getWhitespaceHandling

  interface len
    module procedure getLength
  end interface

  interface hasKey
    module procedure has_key, has_key_ns
  end interface hasKey

  interface getIndex
    module procedure get_key_index, get_key_index_ns
  end interface

  interface getQName
    module procedure get_key
  end interface

  interface getValue
    module procedure get_value_by_key, get_value_by_index, get_value_by_key_ns
  end interface
  interface get_value
    module procedure get_value_by_key, get_value_by_index
  end interface
  interface remove_key
    module procedure remove_key_by_index
  end interface

  interface getURI
    module procedure get_nsURI_by_index
  end interface
  interface get_prefix
    module procedure get_prefix_by_index
  end interface
  interface getLocalName
    module procedure get_localName_by_index
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

  interface getType
    module procedure getType_by_index, getType_by_keyname
  end interface

  interface getSpecified
    module procedure getSpecified_by_index
  end interface

contains

  function getLength(dict) result(n)
    type(dictionary_t), intent(in)   :: dict
    integer                          :: n

    n = dict%number_of_items

  end function getLength


  function has_key(dict, key) result(found)
    type(dictionary_t), intent(in)   :: dict
    character(len=*), intent(in)     :: key
    logical                          :: found

    integer  ::  i
    found = .false.
    do  i = 1, dict%number_of_items
      if (key == str_vs(dict%items(i)%key)) then
        found = .true.
        exit
      endif
    enddo
  end function has_key

  function has_key_ns(dict, uri, localname) result(found)
    type(dictionary_t), intent(in)   :: dict
    character(len=*), intent(in)     :: uri, localname
    logical                          :: found

    integer  ::  i
    found = .false.
    do  i = 1, dict%number_of_items
      if (uri==str_vs(dict%items(i)%nsURI) &
        .and.localname==str_vs(dict%items(i)%localname)) then
        found = .true.
        exit
      endif
    enddo
  end function has_key_ns

  pure function get_key_index(dict,key) result(ind)
    type(dictionary_t), intent(in)   :: dict
    character(len=*), intent(in)     :: key
    integer                          :: ind

    integer  ::  i
    ind = -1
    do  i = 1, dict%number_of_items
      if (key == str_vs(dict%items(i)%key)) then
        ind = i
        exit
      endif
    enddo
  end function get_key_index

  pure function get_key_index_ns(dict, uri, localname) result(ind)
    type(dictionary_t), intent(in)   :: dict
    character(len=*), intent(in)     :: uri, localname
    integer                          :: ind

    integer  ::  i
    ind = -1
    do  i = 1, dict%number_of_items
      if (uri==str_vs(dict%items(i)%nsURI) &
        .and. localname==str_vs(dict%items(i)%localname)) then
        ind = i
        exit
      endif
    enddo
  end function get_key_index_ns


  function get_value_by_key(dict,key,status) result(value)
    type(dictionary_t), intent(in)       :: dict
    character(len=*), intent(in)              :: key
    integer, optional, intent(out)            :: status
    character(len = merge(size(dict%items(get_key_index(dict, key))%value), 0, (get_key_index(dict, key) > 0))) :: value
    !
    integer  :: i

    if (present(status)) status = -1
    do  i = 1, dict%number_of_items
      if (key == str_vs(dict%items(i)%key)) then
        value = str_vs(dict%items(i)%value)
        if (present(status)) status = 0
        exit
      endif
    enddo

  end function get_value_by_key

  function get_value_by_key_ns(dict, uri, localname, status) result(value)
    type(dictionary_t), intent(in)       :: dict
    character(len=*), intent(in)         :: uri, localname
    integer, optional, intent(out)       :: status
    character(len = merge(size(dict%items(get_key_index_ns(dict, uri, localname))%value), 0, &
      (get_key_index_ns(dict, uri, localname) > 0))) :: value
    !
    integer  :: i

    if (present(status)) status = -1
    do  i = 1, dict%number_of_items
       if (uri==str_vs(dict%items(i)%nsURI) &
         .and.localname==str_vs(dict%items(i)%localname)) then
          value = str_vs(dict%items(i)%value)
          if (present(status)) status = 0
          exit
       endif
    enddo

  end function get_value_by_key_ns

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
  
  subroutine add_item_to_dict(dict, key, value, prefix, nsURI, type, itype, specified)
    
    type(dictionary_t), intent(inout) :: dict
    character(len=*), intent(in)           :: key
    character(len=*), intent(in)           :: value
    character(len=*), intent(in), optional :: prefix
    character(len=*), intent(in), optional :: nsURI
    character(len=*), intent(in), optional :: type
    integer, intent(in), optional :: itype
    logical, intent(in), optional :: specified
    
    integer  :: n

    if (present(prefix) .eqv. .not.present(nsURI)) &
       call FoX_Error('Namespace improperly specified')
    
    n = dict%number_of_items
    if (n == size(dict%items)) then
       call resize_dict(dict)
    endif
    
    n = n + 1
    dict%items(n)%value => vs_str_alloc(value)

    if (present(prefix)) then
      dict%items(n)%key => vs_str_alloc(prefix//":"//key)
      dict%items(n)%localname => vs_str_alloc(key)
      dict%items(n)%prefix => vs_str_alloc(prefix)
      dict%items(n)%nsURI => vs_str_alloc(nsURI)
    else
      dict%items(n)%key => vs_str_alloc(key)
      dict%items(n)%localname => vs_str_alloc(key)
      allocate(dict%items(n)%prefix(0))
      allocate(dict%items(n)%nsURI(0))
    endif
    if (present(type)) then
      if (present(itype)) &
        call FoX_fatal("internal library error in add_item_to_dict")
      select case(type)
      case ('CDATA')
        dict%items(n)%type = ATT_CDATA
      case ('ID')
        dict%items(n)%type = ATT_ID
      case ('IDREF')
        dict%items(n)%type = ATT_IDREF
      case ('IDREFS')
        dict%items(n)%type = ATT_IDREFS
      case ('NMTOKEN')
        dict%items(n)%type = ATT_NMTOKEN
      case ('NMTOKENS')
        dict%items(n)%type = ATT_NMTOKENS
      case ('ENTITY')
        dict%items(n)%type = ATT_ENTITY
      case ('ENTITIES')
        dict%items(n)%type = ATT_ENTITIES
      case ('NOTATION')
        dict%items(n)%type = ATT_NOTATION
      case ('CDANO')
        dict%items(n)%type = ATT_CDANO
      case ('CDAMB')
        dict%items(n)%type = ATT_CDAMB
      end select
    elseif (present(itype)) then
      dict%items(n)%type = itype
    else
      dict%items(n)%type = ATT_CDAMB
    endif
    if (present(specified)) then
      dict%items(n)%specified = specified
    else
      dict%items(n)%specified = .true.
    endif

    dict%number_of_items = n

  end subroutine add_item_to_dict
  
  subroutine set_nsURI_by_index(dict, i, nsURI)
    type(dictionary_t), intent(inout) :: dict
    integer, intent(in) :: i
    character(len=*), intent(in) :: nsURI

    if (associated(dict%items(i)%nsURI)) &
         deallocate(dict%items(i)%nsURI)
    dict%items(i)%nsURI => vs_str_alloc(nsURI)
  end subroutine set_nsURI_by_index

  subroutine set_prefix_by_index(dict, i, prefix)
    type(dictionary_t), intent(inout) :: dict
    integer, intent(in) :: i
    character(len=*), intent(in) :: prefix

    if (associated(dict%items(i)%prefix)) &
         deallocate(dict%items(i)%prefix)
    dict%items(i)%prefix => vs_str_alloc(prefix)
  end subroutine set_prefix_by_index

  subroutine set_localName_by_index_s(dict, i, localName)
    type(dictionary_t), intent(inout) :: dict
    integer, intent(in) :: i
    character(len=*), intent(in) :: localName

    if (associated(dict%items(i)%localName)) &
         deallocate(dict%items(i)%localName)
    dict%items(i)%localName => vs_str_alloc(localName)
  end subroutine set_localName_by_index_s

  subroutine set_localName_by_index_vs(dict, i, localName)
    type(dictionary_t), intent(inout) :: dict
    integer, intent(in) :: i
    character(len=1), dimension(:), intent(in) :: localName

    if (associated(dict%items(i)%localName)) &
         deallocate(dict%items(i)%localName)
    allocate(dict%items(i)%localName(size(localName)))
    dict%items(i)%localName = localName
  end subroutine set_localName_by_index_vs

  pure function get_nsURI_by_index(dict, i) result(nsURI)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    character(len=size(dict%items(i)%nsURI)) :: nsURI
    
    nsURI = str_vs(dict%items(i)%nsURI)
  end function get_nsURI_by_index

  pure function get_prefix_by_index(dict, i) result(prefix)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    character(len=size(dict%items(i)%prefix)) :: prefix
    
    prefix = str_vs(dict%items(i)%prefix)
  end function get_prefix_by_index

  pure function get_localName_by_index(dict, i) result(localName)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    character(len=size(dict%items(i)%localName)) :: localName
    
    localName = str_vs(dict%items(i)%localName)
  end function get_localName_by_index
    
  pure function get_nsURI_by_keyname(dict, keyname) result(nsURI)
    type(dictionary_t), intent(in) :: dict
    character(len=*), intent(in) :: keyname
    character(len=merge(size(dict%items(get_key_index(dict, keyname))%nsURI), 0, (get_key_index(dict, keyname) > 0))) :: nsURI
    integer :: i

    i = get_key_index(dict, keyname)
    nsURI = str_vs(dict%items(i)%nsURI)
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
    localName = str_vs(dict%items(i)%localName)

  end function get_localName_by_keyname

  function getType_by_index(dict, i) result(type)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    character(len=ATT_TYPELENGTHS(merge(dict%items(i)%type,0,i<=size(dict%items)))) :: type

    if (i<=size(dict%items)) then
      select case(dict%items(i)%type)
      case (ATT_CDATA)
        type='CDATA'
      case (ATT_ID)
        type='ID'
      case (ATT_IDREF)
        type='IDREF'
      case (ATT_IDREFS)
        type='IDREFS'
      case (ATT_NMTOKEN)
        type='NMTOKENS'
      case (ATT_ENTITY)
        type='ENTITY'
      case (ATT_ENTITIES)
        type='ENTITIES'
      case (ATT_NOTATION)
        type='NOTATION'
      case (ATT_CDANO)
        type='CDATA'
      case (ATT_CDAMB)
        type='CDATA'
      end select
    else
      type = ''
    endif

  end function getType_by_index

  function getType_by_keyname(dict, keyname) result(type)
    type(dictionary_t), intent(in) :: dict
    character(len=*), intent(in) :: keyname
    character(len=ATT_TYPELENGTHS( &
      merge(dict%items(get_key_index(dict, keyname))%type, 0, get_key_index(dict, keyname)>0) &
      )) :: type

    if (get_key_index(dict, keyname)>0) then
      select case(dict%items(get_key_index(dict, keyname))%type)
      case (ATT_CDATA)
        type='CDATA'
      case (ATT_ID)
        type='ID'
      case (ATT_IDREF)
        type='IDREF'
      case (ATT_IDREFS)
        type='IDREFS'
      case (ATT_NMTOKEN)
        type='NMTOKENS'
      case (ATT_ENTITY)
        type='ENTITY'
      case (ATT_ENTITIES)
        type='ENTITIES'
      case (ATT_NOTATION)
        type='NOTATION'
      case (ATT_CDANO)
        type='CDATA'
      case (ATT_CDAMB)
        type='CDATA'
      end select
    else
      type = ''
    endif

  end function getType_by_keyname

  function getSpecified_by_index(dict, i) result(p)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    logical :: p

    if (i>0 .and. i<=dict%number_of_items) then
      p = dict%items(i)%specified
    else
      p = .false.
    endif
  end function getSpecified_by_index

  function getWhitespaceHandling(dict, i) result(j)
    type(dictionary_t), intent(in) :: dict
    integer, intent(in) :: i
    integer :: j

    if (i<=size(dict%items)) then
      select case(dict%items(i)%type)
      case (ATT_CDATA)
        j = 0 !
      case (ATT_CDAMB)
        j = 1
      case default
        j = 2
      end select
    else
      j = 2
    endif

  end function getWhitespaceHandling

  subroutine init_dict(dict)
    type(dictionary_t), intent(out)   :: dict
    
    integer :: i

    allocate(dict%items(DICT_INIT_LEN))
    do i = 1, DICT_INIT_LEN
       nullify(dict%items(i)%key)
       nullify(dict%items(i)%value)
       nullify(dict%items(i)%prefix)
       nullify(dict%items(i)%nsURI)
       nullify(dict%items(i)%localName)
    enddo
    
    dict%number_of_items = 0
    
  end subroutine init_dict

  subroutine resize_dict(dict)
    type(dictionary_t), intent(inout) :: dict
    type(dict_item), dimension(size(dict%items)) :: tempDict
    integer :: i, l_d_new, l_d_old

    l_d_old = size(dict%items)
    do i = 1, l_d_old
       tempDict(i)%key => dict%items(i)%key
       tempDict(i)%value => dict%items(i)%value
       tempDict(i)%prefix => dict%items(i)%prefix
       tempDict(i)%nsURI => dict%items(i)%nsURI
       tempDict(i)%localName => dict%items(i)%localName
       tempDict(i)%type = dict%items(i)%type
       tempDict(i)%specified = dict%items(i)%specified
    enddo
    deallocate(dict%items)
    l_d_new = l_d_old * DICT_LEN_MULT
    allocate(dict%items(l_d_new))
    do i = 1, l_d_old
       dict%items(i)%key => tempDict(i)%key
       dict%items(i)%value => tempDict(i)%value
       dict%items(i)%nsURI => tempDict(i)%nsURI
       dict%items(i)%prefix => tempDict(i)%prefix
       dict%items(i)%localName => tempDict(i)%localName
       dict%items(i)%type = tempDict(i)%type
       dict%items(i)%specified = tempDict(i)%specified
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
    if (associated(dict%items)) deallocate(dict%items)

    dict%number_of_items = 0
  end subroutine destroy_dict
  

  subroutine reset_dict(dict)
    type(dictionary_t), intent(inout)   :: dict
    
    integer :: i
    do i = 1, dict%number_of_items
       deallocate(dict%items(i)%key)
       deallocate(dict%items(i)%value)
       deallocate(dict%items(i)%prefix)
       deallocate(dict%items(i)%nsURI)
       deallocate(dict%items(i)%localName)
    enddo
    
    dict%number_of_items = 0
    
  end subroutine reset_dict
  
  
  subroutine print_dict(dict)
    type(dictionary_t), intent(in)   :: dict
    
    integer  :: i
    
    do i = 1, dict%number_of_items
       write(*,'(7a)') str_vs(dict%items(i)%key), " [ {", str_vs(dict%items(i)%nsURI), &
          "}", str_vs(dict%items(i)%localName), " ]  = ", str_vs(dict%items(i)%value)
    enddo
    
  end subroutine print_dict

end module m_common_attrs

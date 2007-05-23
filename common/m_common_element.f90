module m_common_element

  ! Structure and manipulation of element specification

  use m_common_array_str, only: str_vs, vs_str_alloc, &
    string_list, init_string_list, destroy_string_list, add_string
  use m_common_charset, only: isInitialNameChar, isNameChar, &
    upperCase, operator(.in.), XML_WHITESPACE
  use m_common_error, only: error_stack, add_error

  implicit none
  private

  integer, parameter :: ST_START               = 0
  integer, parameter :: ST_EMPTYANY            = 1
  integer, parameter :: ST_FIRSTCHILD          = 2
  integer, parameter :: ST_END                 = 3
  integer, parameter :: ST_PCDATA              = 4
  integer, parameter :: ST_NAME                = 5
  integer, parameter :: ST_CHILD               = 6
  integer, parameter :: ST_AFTERBRACKET        = 7
  integer, parameter :: ST_AFTERLASTBRACKET    = 8
  integer, parameter :: ST_SEPARATOR           = 9
  integer, parameter :: ST_AFTERNAME           = 10
  integer, parameter :: ST_ATTTYPE             = 11
  integer, parameter :: ST_AFTER_NOTATION      = 12
  integer, parameter :: ST_NOTATION_LIST       = 13
  integer, parameter :: ST_ENUMERATION         = 14
  integer, parameter :: ST_ENUM_NAME           = 15
  integer, parameter :: ST_AFTER_ATTTYPE_SPACE = 16
  integer, parameter :: ST_AFTER_ATTTYPE       = 17
  integer, parameter :: ST_DEFAULT_DECL        = 18
  integer, parameter :: ST_AFTERDEFAULTDECL    = 19
  integer, parameter :: ST_DEFAULTVALUE        = 20

  integer, parameter :: ATT_NULL = 0

  integer, parameter :: ATT_CDATA = 1
  integer, parameter :: ATT_ID = 2
  integer, parameter :: ATT_IDREF = 3
  integer, parameter :: ATT_IDREFS = 4
  integer, parameter :: ATT_ENTITY = 5
  integer, parameter :: ATT_ENTITIES = 6
  integer, parameter :: ATT_NMTOKEN = 7
  integer, parameter :: ATT_NMTOKENS = 8
  integer, parameter :: ATT_NOTATION = 9
  integer, parameter :: ATT_ENUM = 10
  integer, parameter :: ATT_CDANO = 11
  integer, parameter :: ATT_CDAMB = 12
  integer, parameter :: ATT_TYPELENGTHS(0:12) = (/0,5,2,5,6,7,8,6,8,8,4,5,5/)

  integer, parameter :: ATT_REQUIRED = 1
  integer, parameter :: ATT_IMPLIED = 2
  integer, parameter :: ATT_DEFAULT = 4
  integer, parameter :: ATT_FIXED = 3


  type attribute_t
    character, pointer :: name(:) => null()
    integer :: attType = ATT_NULL 
    integer :: attDefault = ATT_NULL
    type(string_list) :: enumerations
    character, pointer :: default(:) => null()
  end type attribute_t

  type attribute_list
    type(attribute_t), pointer :: list(:) => null()
  end type attribute_list

  type element_t
    character, pointer :: name(:) => null()
    logical :: empty = .false.
    logical :: any = .false.
    logical :: mixed = .false.
    logical :: space = .false.
    character, pointer :: model(:) => null()
    type(attribute_list) :: attlist
  end type element_t

  type element_list
    type(element_t), pointer :: list(:) => null()
  end type element_list


  public :: element_t
  public :: element_list

  public :: attribute_t
  public :: attribute_list

  public :: init_element_list
  public :: destroy_element_list
  public :: existing_element
  public :: get_element
  public :: add_element

  public :: parse_dtd_element

  public :: init_attribute_list
  public :: destroy_attribute_list

  public :: parse_dtd_attlist

  public :: report_declarations

  public :: get_att_type
  public :: get_default_atts

  public :: ATT_NULL
  public :: ATT_CDATA
  public :: ATT_ID 
  public :: ATT_IDREF
  public :: ATT_IDREFS
  public :: ATT_ENTITY
  public :: ATT_ENTITIES
  public :: ATT_NMTOKEN
  public :: ATT_NMTOKENS
  public :: ATT_NOTATION
  public :: ATT_ENUM
  public :: ATT_CDANO
  public :: ATT_CDAMB
  public :: ATT_TYPELENGTHS

contains

  subroutine init_element_list(e_list)
    type(element_list), intent(inout) :: e_list

    allocate(e_list%list(0))
  end subroutine init_element_list

  subroutine destroy_element_list(e_list)
    type(element_list), intent(inout) :: e_list

    integer :: i

    do i = 1, size(e_list%list)
      deallocate(e_list%list(i)%name)
      if (associated(e_list%list(i)%model)) deallocate(e_list%list(i)%model)
      call destroy_attribute_list(e_list%list(i)%attlist)
    enddo
    deallocate(e_list%list)
  end subroutine destroy_element_list

  function existing_element(e_list, name) result(p)
    type(element_list), intent(in) :: e_list
    character(len=*), intent(in) :: name
    logical :: p

    integer :: i

    p = .false.
    do i = 1, size(e_list%list)
      if (str_vs(e_list%list(i)%name)==name) then
        p = .true.
        exit
      endif
    enddo
  end function existing_element

  function get_element(e_list, name) result(e)
    type(element_list), intent(in) :: e_list
    character(len=*), intent(in) :: name
    type(element_t), pointer :: e

    integer :: i

    do i = 1, size(e_list%list)
      if (str_vs(e_list%list(i)%name)==name) then
        e => e_list%list(i)
        exit
      endif
    enddo
  end function get_element

  function add_element(e_list, name) result(e)
    type(element_list), intent(inout) :: e_list
    character(len=*), intent(in) :: name
    type(element_t), pointer :: e

    type(element_t), pointer :: temp(:)
    integer :: i

    temp => e_list%list

    allocate(e_list%list(size(temp)+1))
    do i = 1, size(temp)
      e_list%list(i)%name => temp(i)%name
      e_list%list(i)%model => temp(i)%model
      e_list%list(i)%empty = temp(i)%empty
      e_list%list(i)%any = temp(i)%any
      e_list%list(i)%mixed = temp(i)%mixed
      e_list%list(i)%space = temp(i)%space
      e_list%list(i)%attlist%list => temp(i)%attlist%list
    enddo
    deallocate(temp)
    e => e_list%list(i)
    e%name => vs_str_alloc(name)
    call init_attribute_list(e%attlist)

  end function add_element

  subroutine parse_dtd_element(contents, xv, stack, element)
    character(len=*), intent(in) :: contents
    integer, intent(in) :: xv
    type(error_stack), intent(inout) :: stack
    type(element_t), intent(inout), optional :: element

    integer :: state
    integer :: i, nbrackets
    logical :: mixed, empty, any
    character :: c
    character, pointer :: order(:), name(:), temp(:)
    logical :: mixed_additional

    order => null()
    name => null()
    temp => null()

    any = .false.
    empty = .false.
    mixed = .false.
    nbrackets = 0
    mixed_additional = .false.
    state = ST_START

    do i = 1, len(contents) + 1
      if (i<=len(contents)) then
        c = contents(i:i)
      else
        c = ' '
      endif

      if (state==ST_START) then
        !print*,'ST_START'
        if (c.in.XML_WHITESPACE) then
          continue
        elseif (c.in.'EMPTYANY') then
          allocate(name(1))
          name(1) = c
          state = ST_EMPTYANY
        elseif (c=='(') then
          allocate(order(1))
          order(1) = ''
          nbrackets = 1
          state = ST_FIRSTCHILD
        else
          call add_error(stack, &
            'Unexpected character "'//c//'" at start of ELEMENT specification')
          goto 100
        endif

      elseif (state==ST_EMPTYANY) then
        !print*,'ST_EMPTYANY'
        if (c.in.upperCase) then
          temp => name
          allocate(name(size(temp)+1))
          name(:size(temp)) = temp
          name(size(name)) = c
          deallocate(temp)
        elseif (c.in.XML_WHITESPACE) then
          if (str_vs(name)=='EMPTY') then
            empty = .true.
            ! check do we have any notations FIXME
          elseif (str_vs(name)=='ANY') then
            any = .true.
          else
            call add_error(stack, &
              'Unexpected ELEMENT specification; expecting EMPTY or ANY')
            goto 100
          endif
          deallocate(name)
          state = ST_END
        else
          call add_error(stack, &
            'Unexpected ELEMENT specification; expecting EMPTY or ANY')
          goto 100
        endif

      elseif (state==ST_FIRSTCHILD) then
        !print*,'ST_FIRSTCHILD'
        if (c.in.XML_WHITESPACE) cycle
        if (c=='#') then
          mixed = .true.
          state = ST_PCDATA
          allocate(name(0))
        elseif (isInitialNameChar(c, xv)) then
          allocate(name(1))
          name(1) = c
          state = ST_NAME
        elseif (c=='(') then
          deallocate(order)
          allocate(order(2))
          order = ''
          nbrackets = 2
          state = ST_CHILD
        else
          call add_error(stack, &
            'Unexpected character in ELEMENT specification')
          goto 100
        endif

      elseif (state==ST_PCDATA) then
        !print*,'ST_PCDATA'
        if (c.in.'PCDATA') then
          temp => name
          allocate(name(size(temp)+1))
          name(:size(temp)) = temp
          name(size(name)) = c
          deallocate(temp)
        elseif (c.in.XML_WHITESPACE) then
          if (str_vs(name)=='PCDATA') then
            deallocate(name)
          else
            call add_error(stack, &
              'Unexpected token after #')
            goto 100
          endif
          state = ST_SEPARATOR
        elseif (c==')') then
          if (str_vs(name)=='PCDATA') then
            deallocate(name)
            nbrackets = 0
            state = ST_AFTERLASTBRACKET
            deallocate(order)
          else
            call add_error(stack, &
              'Unexpected token after #')
            goto 100
          endif
        elseif (c=='|') then
          if (str_vs(name)=='PCDATA') then
            ! continue
            deallocate(name)
          else
            call add_error(stack, &
              'Unexpected token after #')
            goto 100
          endif
          order(1) = '|'
          state = ST_CHILD
        elseif (c==',') then
          call add_error(stack, &
            'Ordered specification not allowed for Mixed elements')
          goto 100
        else
          call add_error(stack, &
            'Unexpected character in ELEMENT specification')
          goto 100
        endif

      elseif (state==ST_NAME) then
        !print*,'ST_NAME'
        if (isNameChar(c, xv)) then
          temp => name
          allocate(name(size(temp)+1))
          name(:size(temp)) = temp
          name(size(name)) = c
          deallocate(temp)
        elseif (c=='?') then
          deallocate(name)
          if (mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            goto 100
          else
            state = ST_SEPARATOR
          endif
        elseif (c=='+') then
          deallocate(name)
          if (mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            goto 100
          else
            state = ST_SEPARATOR
          endif
        elseif (c=='*') then
          deallocate(name)
          if (mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            goto 100
          else
            state = ST_SEPARATOR
          endif
        elseif (c.in.XML_WHITESPACE) then
          deallocate(name)
          if (mixed) mixed_additional = .true.
          state = ST_SEPARATOR
        elseif (c==',') then
          deallocate(name)
          if (order(nbrackets)=='') then
            order(nbrackets)=','
          elseif (order(nbrackets)=='|') then
            call add_error(stack, &
              'Cannot mix ordered and unordered elements')
            goto 100
          endif
          state = ST_CHILD
        elseif (c=='|') then
          deallocate(name)
          if (order(nbrackets)=='') then
            order(nbrackets)='|'
          elseif (order(nbrackets)==',') then
            call add_error(stack, &
              'Cannot mix ordered and unordered elements')
            goto 100
          endif
          if (mixed) mixed_additional = .true.
          state = ST_CHILD
        elseif (c==')') then
          deallocate(name)
          if (mixed) mixed_additional = .true.
          nbrackets = nbrackets - 1
          if (nbrackets==0) then
            state = ST_AFTERLASTBRACKET
            deallocate(order)
          else
            temp => order
            allocate(order(nbrackets))
            order = temp(:size(order))
            deallocate(temp)
            state = ST_AFTERBRACKET
          endif
        else
          call add_error(stack, &
            'Unexpected character found after element name')
          goto 100
        endif

      elseif (state==ST_CHILD) then
        !print*,'ST_CHILD'
        if (c.in.XML_WHITESPACE) cycle
        if (c=='#') then
          call add_error(stack, &
            '# forbidden except as first child element')
          goto 100
        elseif (isInitialNameChar(c, xv)) then
          allocate(name(1))
          name(1) = c
          state = ST_NAME
        elseif (c=='(') then
          if (mixed) then
            call add_error(stack, &
              'Nested brackets forbidden for Mixed content')
            goto 100
          endif
          nbrackets = nbrackets + 1
          temp => order
          allocate(order(nbrackets))
          order(:size(temp)) = temp
          deallocate(temp)
          order(nbrackets) = ''
        else
          call add_error(stack, &
            'Unexpected character "'//c//'" found after (')
          goto 100
        endif

      elseif (state==ST_SEPARATOR) then
        !print*,'ST_SEPARATOR'
        if (c.in.XML_WHITESPACE) cycle
        if (c=='#') then
          call add_error(stack, &
            '#PCDATA must be first in list')
          goto 100
        elseif (c==',') then
          if (order(nbrackets)=='') then
            order(nbrackets)=','
          elseif (order(nbrackets)=='|') then
            call add_error(stack, &
              'Cannot mix ordered and unordered elements')
            goto 100
          endif
          state = ST_CHILD
        elseif (c=='|') then
          if (order(nbrackets)=='') then
            order(nbrackets)='|'
          elseif (order(nbrackets)==',') then
            call add_error(stack, &
              'Cannot mix ordered and unordered elements')
            goto 100
          endif
          state = ST_CHILD
        elseif (c==')') then
          nbrackets = nbrackets - 1
          if (nbrackets==0) then
            state = ST_AFTERLASTBRACKET
            deallocate(order)
          else
            temp => order
            allocate(order(nbrackets))
            order = temp(:size(order))
            deallocate(temp)
            state = ST_AFTERBRACKET
          endif
        else
          call add_error(stack, &
            'Unexpected character found in element declaration.')
          goto 100
        endif

      elseif (state==ST_AFTERBRACKET) then
        !print*,'ST_AFTERBRACKET'
        if (c=='*') then
          state = ST_SEPARATOR
        elseif (c=='+') then
          state = ST_SEPARATOR
        elseif (c=='?') then
          state = ST_SEPARATOR
        elseif (c.in.XML_WHITESPACE) then
          state = ST_SEPARATOR
        elseif (c ==',') then
          if (order(nbrackets)=='') then
            order(nbrackets) = ','
          elseif (order(nbrackets)=='|') then
            call add_error(stack, &
              'Cannot mix ordered and unordered elements')
            goto 100
          endif
          state = ST_CHILD
        elseif (c =='|') then
          if (order(nbrackets)=='') then
            order(nbrackets) = '|'
          elseif (order(nbrackets)==',') then
            call add_error(stack, &
              "Cannot mix ordered and unordered elements")
            goto 100 
          endif
          state = ST_CHILD
        elseif (c==')') then
          nbrackets = nbrackets - 1
          if (nbrackets==0) then
            deallocate(order)
            state = ST_AFTERLASTBRACKET
          else
            temp => order
            allocate(order(nbrackets))
            order = temp(:size(order))
            deallocate(temp)
            state = ST_AFTERBRACKET
          endif
        else
          call add_error(stack, &
            'Unexpected character "'//c//'"found after ")"')
          goto 100
        endif

      elseif (state==ST_AFTERLASTBRACKET) then
        !print*,'ST_AFTERLASTBRACKET'
        if (c=='*') then
          state = ST_END
        elseif (c=='+') then
          if (mixed) then
            call add_error(stack, &
              '+ operator disallowed for Mixed elements')
            goto 100
          else
            state = ST_END
          endif
        elseif (c=='?') then
          if (mixed) then
            call add_error(stack, &
              '? operator disallowed for Mixed elements')
            goto 100
          else
            state = ST_END
          endif
        elseif (c.in.XML_WHITESPACE) then
          if (mixed) then
            if (mixed_additional) then
              call add_error(stack, &
                'Missing "*" at end of Mixed element specification')
              goto 100
            endif
          endif
          state = ST_END
        else
          call add_error(stack, &
            'Unexpected character "'//c//'" found after final ")"')
          goto 100
        endif

      elseif (state==ST_END) then
        !print*,'ST_END'
        if (c.in.XML_WHITESPACE) then
          continue
        else
          call add_error(stack, &
            'Unexpected token found after end of element specification')
          goto 100
        endif

      endif

    enddo

    if (state/=ST_END) then
      if (associated(order)) deallocate(order)
      if (associated(name)) deallocate(name)
      goto 100
    endif

    if (present(element)) then
      element%any = any
      element%empty = empty
      element%mixed = mixed
      element%model => vs_str_alloc(trim(strip_spaces(contents)))
    endif
    return

100 if (associated(order)) deallocate(order)
    if (associated(name)) deallocate(name)

    contains
      function strip_spaces(s1) result(s2)
        character(len=*) :: s1
        character(len=len(s1)) :: s2
        integer :: i, i2
        i2 = 1
        do i = 1, len(s1)
          if (s1(i:i).in.XML_WHITESPACE) cycle
          s2(i2:i2) = s1(i:i)
          i2 = i2 + 1
        end do
        s2(i2:) = ''
      end function strip_spaces
  end subroutine parse_dtd_element


  subroutine init_attribute_list(a_list)
    type(attribute_list), intent(inout) :: a_list

    allocate(a_list%list(0))
  end subroutine init_attribute_list

  subroutine destroy_attribute_list(a_list)
    type(attribute_list), intent(inout) :: a_list

    integer :: i

    do i = 1, size(a_list%list)
      deallocate(a_list%list(i)%name)
      if (associated(a_list%list(i)%default)) deallocate(a_list%list(i)%default)
      call destroy_string_list(a_list%list(i)%enumerations)
    enddo
    deallocate(a_list%list)

  end subroutine destroy_attribute_list

  function existing_attribute(a_list, name) result(p)
    type(attribute_list), intent(inout) :: a_list
    character(len=*), intent(in) :: name
    logical :: p

    integer :: i
    p = .false.
    do i = 1, size(a_list%list)
      p = (str_vs(a_list%list(i)%name)==name)
      if (p) exit
    enddo
  end function existing_attribute

  function add_attribute(a_list, name) result(a)
    type(attribute_list), intent(inout) :: a_list
    character(len=*), intent(in) :: name
    type(attribute_t), pointer :: a

    integer :: i
    type(attribute_t), pointer :: temp(:)

    temp => a_list%list
    allocate(a_list%list(size(temp)+1))
    do i = 1, size(temp)
      a_list%list(i)%name => temp(i)%name
      a_list%list(i)%atttype = temp(i)%atttype
      a_list%list(i)%attdefault = temp(i)%attdefault
      a_list%list(i)%default => temp(i)%default
      a_list%list(i)%enumerations%list => temp(i)%enumerations%list
    enddo
    deallocate(temp)
    a => a_list%list(i)

    a%name => vs_str_alloc(name)
    call init_string_list(a%enumerations)

  end function add_attribute
  
  function get_attribute(a_list, name) result(a)
    type(attribute_list), intent(inout) :: a_list
    character(len=*), intent(in) :: name
    type(attribute_t), pointer :: a

    integer :: i
    do i = 1, size(a_list%list)
      if (str_vs(a_list%list(i)%name)==name) then
        a => a_list%list(i)
        exit
      endif
    enddo
  end function get_attribute

  subroutine parse_dtd_attlist(contents, xv, stack, elem)
    character(len=*), intent(in) :: contents
    integer :: xv
    type(error_stack), intent(inout) :: stack
    type(element_t), intent(inout), optional :: elem

    integer :: i
    integer :: state
    character :: c, q
    character, pointer :: name(:), type(:), default(:), value(:), temp(:)

    type(attribute_t), pointer :: ca
    type(attribute_t), target :: ignore_att
    call init_string_list(ignore_att%enumerations)
    ! We need ignore_att to process but not take account of duplicate attributes

    ! elem is optional so we can not record declarations if necessary.

    name => null()
    type => null()
    default => null()
    value => null()
    temp => null()

    state = ST_START

    do i = 1, len(contents) + 1
      if (i<=len(contents)) then
        c = contents(i:i)
      else
        c = " "
      endif

      if (state==ST_START) then
        !print*,'ST_START'
        if (c.in.XML_WHITESPACE) cycle
        if (isInitialNameChar(c, xv)) then
          name => vs_str_alloc(c)
          state = ST_NAME
        else
          call add_error(stack, &
            'Unexpected character in Attlist')
          goto 200
        endif

      elseif (state==ST_NAME) then
        !print*,'ST_NAME'
        if (isNameChar(c, xv)) then
          temp => name
          allocate(name(size(temp)+1))
          name(:size(temp)) = temp
          name(size(name)) = c
          deallocate(temp)
        elseif (c.in.XML_WHITESPACE) then
          if (present(elem)) then
            if (existing_attribute(elem%attlist, str_vs(name))) then
              if (associated(ignore_att%name)) deallocate(name)
              if (associated(ignore_att%default)) deallocate(default)
              call destroy_string_list(ignore_att%enumerations)
              call init_string_list(ignore_att%enumerations)
              ca => ignore_att
            else
              ca => add_attribute(elem%attlist, str_vs(name))
            endif
          else
            if (associated(ignore_att%name)) deallocate(ignore_att%name)
            if (associated(ignore_att%default)) deallocate(ignore_att%default)
            call destroy_string_list(ignore_att%enumerations)
            call init_string_list(ignore_att%enumerations)
            ca => ignore_att
          endif
          deallocate(name) 
          state = ST_AFTERNAME
        else
          call add_error(stack, &
            'Unexpected character in Attlist Name')
          goto 200
        endif

      elseif (state==ST_AFTERNAME) then
        !print*,'ST_AFTERNAME'
        if (c.in.XML_WHITESPACE) cycle
        if (c.in.upperCase) then
          type => vs_str_alloc(c)
          state = ST_ATTTYPE
        elseif (c=='(') then
          allocate(value(0))
          ca%attType = ATT_ENUM
          state = ST_ENUMERATION
        else
          call add_error(stack, &
            'Unexpected error after Attlist Name')
          goto 200
        endif

      elseif (state==ST_ATTTYPE) then
        !print*,'ST_ATTTYPE'
        if (c.in.upperCase) then
          temp => type
          allocate(type(size(temp)+1))
          type(:size(temp)) = temp
          type(size(type)) = c
          deallocate(temp)
        elseif (c.in.XML_WHITESPACE) then
          if (str_vs(type)=='CDATA') then
            ca%attType = ATT_CDATA
            state = ST_AFTER_ATTTYPE
          elseif (str_vs(type)=='ID') then
            ca%attType = ATT_ID
            state = ST_AFTER_ATTTYPE
          elseif (str_vs(type)=='IDREF') then
            ca%attType = ATT_IDREF
            state = ST_AFTER_ATTTYPE
          elseif (str_vs(type)=='IDREFS') then
            ca%attType = ATT_IDREFS
            state = ST_AFTER_ATTTYPE
          elseif (str_vs(type)=='ENTITY') then
            ca%attType = ATT_ENTITY
            state = ST_AFTER_ATTTYPE
          elseif (str_vs(type)=='ENTITIES') then
            ca%attType = ATT_ENTITIES
            state = ST_AFTER_ATTTYPE
          elseif (str_vs(type)=='NMTOKEN') then
            ca%attType = ATT_NMTOKEN
            state = ST_AFTER_ATTTYPE
          elseif (str_vs(type)=='NMTOKENS') then
            ca%attType = ATT_NMTOKENS
            state = ST_AFTER_ATTTYPE
          elseif (str_vs(type)=='NOTATION') then
            ca%attType = ATT_NOTATION
            state = ST_AFTER_NOTATION
          else
            call add_error(stack, &
              'Unknown AttType')
            goto 200
          endif
          deallocate(type)
        else
          call add_error(stack, &
            'Unexpected character in AttType')
          goto 200
        endif

      elseif (state==ST_AFTER_NOTATION) then
        !print*,'ST_AFTER_NOTATION'
        if (c.in.XML_WHITESPACE) cycle
        if (c=='(') then
          state = ST_NOTATION_LIST
        else
          call add_error(stack, &
            'Unexpected character after Notation')
          goto 200
        endif

      elseif (state==ST_NOTATION_LIST) then
        !print*,'ST_NOTATION_LIST'
        if (c.in.XML_WHITESPACE) cycle
        if (isInitialNameChar(c, xv)) then
          value => vs_str_alloc(c)
          state = ST_ENUM_NAME
        else
          call add_error(stack, &
            'Unexpected character in Notation list')
          goto 200
        endif

      elseif (state==ST_ENUMERATION) then
        !print*,'ST_ENUMERATION'
        if (c.in.XML_WHITESPACE) cycle
        if (isNameChar(c, xv)) then
          temp => value
          allocate(value(size(temp)+1))
          value(:size(temp)) = temp
          value(size(value)) = c
          deallocate(temp)
          state = ST_ENUM_NAME
        elseif (c=='|') then
          call add_error(stack, &
            "Missing token in Enumeration")
          goto 200
        elseif (c==')') then
          call add_error(stack, &
            "Missing tokens in Enumeration")
          goto 200
        else
          call add_error(stack, &
            'Unexpected character in attlist enumeration')
          goto 200
        endif

      elseif (state==ST_ENUM_NAME) then
        !print*,'ST_ENUM_NAME'
        if (isNameChar(c, xv)) then
          temp => value
          allocate(value(size(temp)+1))
          value(:size(temp)) = temp
          value(size(value)) = c
          deallocate(temp)
        elseif (c.in.XML_WHITESPACE) then
          call add_string(ca%enumerations, str_vs(value))
          deallocate(value)
          state = ST_SEPARATOR
        elseif (c=='|') then
          call add_string(ca%enumerations, str_vs(value))
          deallocate(value)
          if (ca%attType==ATT_NOTATION) then
            state = ST_NOTATION_LIST
          else
            allocate(value(0))
            state = ST_ENUMERATION
          endif
        elseif (c==')') then
          if (size(value)==0) then
            call add_error(stack, &
              'Missing token in Enumeration list')
            goto 200
          endif
          call add_string(ca%enumerations, str_vs(value))
          deallocate(value)
          state = ST_AFTER_ATTTYPE_SPACE
        else
          call add_error(stack, &
            'Unexpected character in attlist enumeration')
          goto 200
        endif

      elseif (state==ST_SEPARATOR) then
        !print*,'ST_SEPARATOR'
        if (c.in.XML_WHITESPACE) cycle
        if (c=='|') then
          if (ca%attType==ATT_NOTATION) then
            state = ST_NOTATION_LIST
          else
            allocate(value(0))
            state = ST_ENUMERATION
          endif
        elseif (c==')') then
          state = ST_AFTER_ATTTYPE_SPACE
        else
          call add_error(stack, &
            'Unexpected character in attlist enumeration')
          goto 200
        endif

      elseif (state==ST_AFTER_ATTTYPE_SPACE) then
        if (.not.(c.in.XML_WHITESPACE)) then
          call add_error(stack, &
            'Missing xwhitespace in attlist enumeration')
          goto 200
        endif
        state = ST_AFTER_ATTTYPE

      elseif (state==ST_AFTER_ATTTYPE) then
        !print*,'ST_AFTER_ATTTYPE'
        if (c.in.XML_WHITESPACE) cycle
        if (c=='#') then
          allocate(default(0))
          state = ST_DEFAULT_DECL
        elseif (c=='"') then
          ca%attDefault = ATT_DEFAULT
          q = c
          allocate(value(0))
          state = ST_DEFAULTVALUE
        elseif (c=="'") then
          ca%attDefault = ATT_FIXED
          q = c
          allocate(value(0))
          state = ST_DEFAULTVALUE
        else
          call add_error(stack, &
            'Unexpected character after AttType')
          goto 200
        endif

      elseif (state==ST_DEFAULT_DECL) then
        !print*,'ST_DEFAULT_DECL'
        if (c.in.upperCase) then
          temp => default
          allocate(default(size(temp)+1))
          default(:size(temp)) = temp
          default(size(default)) = c
          deallocate(temp)
        elseif (c.in.XML_WHITESPACE) then
          if (str_vs(default)=='REQUIRED') then
            ca%attdefault = ATT_REQUIRED
            deallocate(default)
            state = ST_START
          elseif (str_vs(default)=='IMPLIED') then
            ca%attdefault = ATT_IMPLIED
            deallocate(default)
            state = ST_START
          elseif (str_vs(default)=='FIXED') then
            ca%attdefault = ATT_FIXED
            deallocate(default)
            state = ST_AFTERDEFAULTDECL
          else
            call add_error(stack, &
              'Unknown Default declaration')
            goto 200
          endif
        else
          call add_error(stack, &
            'Unexpected character in Default declaration')
          goto 200
        endif

      elseif (state==ST_AFTERDEFAULTDECL) then
        !print*,'ST_AFTERDEFAULTDECL'
        if (c.in.XML_WHITESPACE) cycle
        if (c=='"') then
          q = c
          allocate(value(0))
          state = ST_DEFAULTVALUE
        elseif (c=="'") then
          q = c
          allocate(value(0))
          state = ST_DEFAULTVALUE
        else
          call add_error(stack, &
            'Unexpected character after Default declaration')
          goto 200
        endif

      elseif (state==ST_DEFAULTVALUE) then
        !print*,'ST_DEFAULTVALUE'
        if (c==q) then
          ca%default => value
          nullify(value)
          state = ST_START
        else
          temp => value
          allocate(value(size(temp)+1))
          value(:size(temp)) = temp
          value(size(value)) = c
          deallocate(temp)
        endif

      endif

    enddo

    if (state/=ST_START) then
      call add_error(stack, &
        'Incomplete Attlist declaration')
      goto 200
    endif

    if (associated(ignore_att%name)) deallocate(ignore_att%name)
    if (associated(ignore_att%default)) deallocate(ignore_att%default)
    call destroy_string_list(ignore_att%enumerations)

    return

200 if (associated(name)) deallocate(name)
    if (associated(type)) deallocate(type)
    if (associated(default)) deallocate(default)
    if (associated(value)) deallocate(value)
    if (associated(ignore_att%name)) deallocate(name)
    if (associated(ignore_att%default)) deallocate(default)
    call destroy_string_list(ignore_att%enumerations)

  end subroutine parse_dtd_attlist


  subroutine report_declarations(elem, attributeDecl_handler)
    type(element_t), intent(in) :: elem
    interface
      subroutine attributeDecl_handler(eName, aName, type, mode, value)
        character(len=*), intent(in) :: eName
        character(len=*), intent(in) :: aName
        character(len=*), intent(in) :: type
        character(len=*), intent(in), optional :: mode
        character(len=*), intent(in), optional :: value
      end subroutine attributeDecl_handler
    end interface

    integer :: i
    character(len=11)  :: type
    character(len=8) :: mode
    type(attribute_t), pointer :: a

    do i = 1, size(elem%attlist%list)
      a => elem%attlist%list(i)

      select case (a%attType)
      case (ATT_CDATA)
        type = 'CDATA'
      case (ATT_ID)
        type = 'ID'
      case (ATT_IDREF)
        type = 'IDREF'
      case (ATT_IDREFS)
        type = 'IDREFS'
      case (ATT_NMTOKEN)
        type = 'NMTOKENS'
      case (ATT_NMTOKENS)
        type = 'NMTOKENS'
      case (ATT_ENTITY)
        type = 'ENTITY'
      case (ATT_ENTITIES)
        type = 'ENTITIES'
      end select
      select case (a%attDefault)
      case (ATT_REQUIRED)
        mode = "REQUIRED"
      case (ATT_IMPLIED)
        mode = "IMPLIED"
      case (ATT_FIXED)
        mode = "FIXED"
      end select

      if (a%attType==ATT_NOTATION) then
        if (a%attDefault==ATT_DEFAULT) then
          if (associated(a%default)) then
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              'NOTATION '//make_token_group(a%enumerations), value=str_vs(a%default))
          else
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              'NOTATION '//make_token_group(a%enumerations))
          endif
        else
          if (associated(a%default)) then
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              'NOTATION '//make_token_group(a%enumerations), mode=trim(mode), &
              value=str_vs(a%default))
          else
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              'NOTATION '//make_token_group(a%enumerations), mode=trim(mode))
          endif
        endif
      elseif (a%attType==ATT_ENUM) then
        if (a%attDefault==ATT_DEFAULT) then
          if (associated(a%default)) then
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              make_token_group(a%enumerations), value=str_vs(a%default))
          else
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              make_token_group(a%enumerations))
          endif
        else
          if (associated(a%default)) then
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              make_token_group(a%enumerations), mode=trim(mode), &
              value=str_vs(a%default))
          else
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              make_token_group(a%enumerations), mode=trim(mode))
          endif
        endif
      else
        if (a%attDefault==ATT_DEFAULT) then
          if (associated(a%default)) then
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              trim(type), value=str_vs(a%default))
          else
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              trim(type))
          endif
        else
          if (associated(a%default)) then
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              trim(type), mode=trim(mode), value=str_vs(a%default))
          else
            call attributeDecl_handler(str_vs(elem%name), str_vs(a%name), &
              trim(type), mode=trim(mode))
          endif
        endif
      endif
    enddo


  end subroutine report_declarations

  pure function make_token_group_len(s_list) result(n)
    type(string_list), intent(in) :: s_list
    integer :: n

    integer :: i
    n = size(s_list%list) + 1
    do i = 1, size(s_list%list)
      n = n + size(s_list%list(i)%s)
    enddo
  end function make_token_group_len

  function make_token_group(s_list) result(s)
    type(string_list), intent(in) :: s_list
    character(len=make_token_group_len(s_list)) :: s
    
    integer :: i, m, n
    s(1:1) = '('
    n = 2
    do i = 1, size(s_list%list)-1
      m = size(s_list%list(i)%s)
      s(n:n+m) = str_vs(s_list%list(i)%s)//'|'
      n = n + m + 1
    enddo
    s(n:) = str_vs(s_list%list(i)%s)//')'
  end function make_token_group


  function get_att_type(e_list, eName, aName) result(i)
    type(element_list), intent(in) :: e_list
    character(len=*), intent(in) :: eName
    character(len=*), intent(in) :: aName
    integer :: i
    
    type(element_t), pointer :: e
    type(attribute_t), pointer :: a

    if (existing_element(e_list, eName)) then
      e => get_element(e_list, eName)
      if (existing_attribute(e%attlist, aName)) then
        a => get_attribute(e%attlist, aName)
        i = a%attType
      else
        i = ATT_NULL
      endif
    else
      i = ATT_NULL
    endif
  end function get_att_type


  function get_default_atts(a_list) result(s_list)
    type(attribute_list), intent(in) :: a_list
    type(string_list) :: s_list
    
    integer :: i
    type(attribute_t), pointer :: a

    call init_string_list(s_list)
    do i = 1, size(a_list%list)
      a => a_list%list(i)
      if (a%attDefault==ATT_DEFAULT) then
        call add_string(s_list, str_vs(a%name))
        call add_string(s_list, str_vs(a%default))
      endif
    enddo
  end function get_default_atts
  
end module m_common_element

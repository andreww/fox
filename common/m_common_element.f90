module m_common_element

  ! Structure and manipulation of element specification

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_charset, only: isInitialNameChar, isNameChar, &
    upperCase, operator(.in.), XML_WHITESPACE
  use m_common_error, only: error_stack, add_error

  implicit none
  private

  integer, parameter :: ST_START            = 0
  integer, parameter :: ST_EMPTYANY         = 1
  integer, parameter :: ST_FIRSTCHILD       = 2
  integer, parameter :: ST_END              = 3
  integer, parameter :: ST_PCDATA           = 4
  integer, parameter :: ST_NAME             = 5
  integer, parameter :: ST_CHILD            = 6
  integer, parameter :: ST_AFTERBRACKET     = 7
  integer, parameter :: ST_AFTERLASTBRACKET = 8
  integer, parameter :: ST_SEPARATOR        = 9


  type element_t
    character, pointer :: name(:) => null()
    logical :: empty = .false.
    logical :: any = .false.
    logical :: mixed = .false.
    logical :: space = .false.
    character, pointer :: model(:) => null()
    !   type(attribute_t), pointer :: attlist => null()
  end type element_t

  type element_list
    type(element_t), pointer :: list(:) => null()
  end type element_list

  public :: element_t
  public :: element_list

  public :: init_element_list
  public :: destroy_element_list
  public :: existing_element
  public :: get_element
  public :: add_element

  public :: parse_dtd_element

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
      e_list%list(i)%empty = temp(i)%empty
      e_list%list(i)%any = temp(i)%any
      e_list%list(i)%mixed = temp(i)%mixed
      e_list%list(i)%space = temp(i)%space
    enddo
    e_list%list(i)%name => vs_str_alloc(name)
    e => e_list%list(i)
  end function add_element

  subroutine parse_dtd_element(contents, element, xv, stack)
    character(len=*), intent(in) :: contents
    type(element_t), intent(inout) :: element
    integer, intent(in) :: xv
    type(error_stack), intent(inout) :: stack

    integer :: state
    integer :: i, j, nbrackets
    character :: c
    character, pointer :: order(:), name(:), temp(:)
    logical :: mixed_additional

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
        print*,'ST_START'
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
        print*,'ST_EMPTYANY'
        if (c.in.upperCase) then
          temp => name
          allocate(name(size(temp)+1))
          name(:size(temp)) = temp
          name(size(name)) = c
          deallocate(temp)
        elseif (c.in.XML_WHITESPACE) then
          if (str_vs(name)=='EMPTY') then
            element%empty = .true.
            ! check do we have any notations FIXME
          elseif (str_vs(name)=='ANY') then
            element%any = .true.
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
        print*,'ST_FIRSTCHILD'
        if (c.in.XML_WHITESPACE) cycle
        if (c=='#') then
          element%mixed = .true.
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
            'Unexpected character "'//c//'"in ELEMENT specification')
          goto 100
        endif

      elseif (state==ST_PCDATA) then
        print*,'ST_PCDATA'
        if (c.in.'PCDATA') then
          temp => name
          allocate(name(size(temp)+1))
          name(:size(temp)) = temp
          name(size(name)) = c
          deallocate(temp)
        elseif (c==')') then
          if (str_vs(name)=='PCDATA') then
            ! continue
            deallocate(name)
            nbrackets = 0
            state = ST_AFTERLASTBRACKET
          else
            call add_error(stack, &
              'Unexpected token after #')
          endif
        elseif (c=='|') then
          order(1) = '|'
          state = ST_CHILD
        elseif (c==',') then
          call add_error(stack, &
            'Ordered specification not allowed for Mixed elements')
          goto 100
        else
          call add_error(stack, &
            'Unexpected character "'//c//'"in ELEMENT specification')
          goto 100
        endif

      elseif (state==ST_NAME) then
        print*,'ST_NAME'
        if (isNameChar(c, xv)) then
          temp => name
          allocate(name(size(temp)+1))
          name(:size(temp)) = temp
          name(size(name)) = c
          deallocate(temp)
        elseif (c=='?') then
          if (element%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            goto 100
          else
            state = ST_SEPARATOR
          endif
        elseif (c=='+') then
          if (element%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            goto 100
          else
            state = ST_SEPARATOR
          endif
        elseif (c=='*') then
          if (element%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            goto 100
          else
            state = ST_SEPARATOR
          endif
        elseif (c.in.XML_WHITESPACE) then
          if (element%mixed) mixed_additional = .true.
          state = ST_SEPARATOR
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
          if (element%mixed) mixed_additional = .true.
          state = ST_CHILD
        elseif (c==')') then
          nbrackets = nbrackets - 1
          temp => order
          allocate(order(nbrackets))
          order = temp(:size(order))
          deallocate(temp)
          if (element%mixed) mixed_additional = .true.
          if (nbrackets==0) then
            state = ST_AFTERLASTBRACKET
          else
            state = ST_AFTERBRACKET
          endif
        else
          call add_error(stack, &
            'Unexpected character "'//c//'" found after element name')
          goto 100
        endif

      elseif (state==ST_CHILD) then
        print*,'ST_CHILD'
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
          if (element%mixed) then
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
        print*,'ST_SEPARATOR'
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
          temp => order
          allocate(order(nbrackets))
          order = temp(:size(order))
          deallocate(temp)
          if (nbrackets==0) then
            state = ST_AFTERLASTBRACKET
          else
            state = ST_AFTERBRACKET
          endif
        endif

      elseif (state==ST_AFTERBRACKET) then
        print*,'ST_AFTERBRACKET'
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
          temp => order
          allocate(order(nbrackets))
          order = temp(:size(order))
          deallocate(temp)
          if (nbrackets==0) then
            state = ST_AFTERLASTBRACKET
          else
            state = ST_AFTERBRACKET
          endif
        else
          call add_error(stack, &
            'Unexpected character "'//c//'"found after ")"')
          goto 100
        endif

      elseif (state==ST_AFTERLASTBRACKET) then
        print*,'ST_AFTERLASTBRACKET'
        if (c=='*') then
          state = ST_END
        elseif (c=='+') then
          if (element%mixed) then
            call add_error(stack, &
              '+ operator disallowed for Mixed elements')
            goto 100
          else
            state = ST_END
          endif
        elseif (c=='?') then
          if (element%mixed) then
            call add_error(stack, &
              '? operator disallowed for Mixed elements')
            goto 100
          else
            state = ST_END
          endif
        elseif (c.in.XML_WHITESPACE) then
          if (element%mixed) then
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
        print*,'ST_END'
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
      call add_error(stack, &
        'Incomplete element specification')
    endif

    element%model => vs_str_alloc(contents)

    return

100 if (associated(order)) deallocate(order)
    if (associated(name)) deallocate(name)

  end subroutine parse_dtd_element

end module m_common_element

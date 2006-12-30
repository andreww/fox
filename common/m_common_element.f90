module m_common_element

  ! Structure and manipulation of element specification

  implicit none
  private


  type element_t
    character, pointer :: name(:) => null()
    logical :: empty = .false
    logical :: any = .false.
    logical :: mixed = .false.
    logical :: space = .false.
    !   type(attribute_t), pointer :: attlist => null()
  end type element_t

  public :: element_t
  public :: parse_dtd_element

contains

  subroutine parse_dtd_element(contents, stack, elementName, element)
    character(len=*), intent(in) :: contents
    type(error_stack), intent(inout) :: stack
    character(len=*), intent(in) :: elementName
    type(element_t), intent(inout) :: el

    integer :: i, j, nbrackets
    character, allocatable :: order(:), name(:)
    character, pointer :: temp(:)

    element%name => vs_str_alloc(elementName)

    nbrackets = 0
    state = START

    do i = 1, len(contents) + 1
      if (i<=len(contents)) then
        c = contents(i:i)
      else
        c = ' '
      endif

      if (state==START) then
        if (i.in.XML_WHITESPACE) cycle
        if (c.in.'EMPTYANY') then
          allocate(name(1))
          name(1) = contents(i:i)
          state = EMPTYANY
        elseif (c=='(') then
          allocate(order(1))
          order(1) = ''
          nbrackets = 1
          state = VERYFIRSTCHILD
        else
          call add_error(stack, &
            'Unexpected character "'//c//'" at start of ELEMENT specification')
          return
        endif

      elseif (state==EMPTYANY) then
        if (c.in.'EMPTYANY') then
          temp => name
          allocate(name(size(temp)+1))
          name(size(name)) = contents(i:i)
          deallocate(temp)
        elseif (c.in.XML_WHITESPACE)
          if (str_vs(name)=='EMPTY') then
            el%empty = .true.
          elseif (str_vs(name)=='ANY') then
            el%any = .true.
          else
            call add_error(stack, &
              'Unexpected ELEMENT specification; expecting EMPTY or ANY')
            return
          endif
          deallocate(name)
          state = END
        else
          call add_error(stack, &
            'Unexpected ELEMENT specification; expecting EMPTY or ANY')
          return
        endif

      elseif (state==VERYFIRSTCHILD) then
        if (c.in.XML_WHITESPACE) cycle
        if (c=='#') then
          el%mixed = .true.
          state = PCDATA
        elseif ! a name
          state = NAME
        elseif (c=='(') then
          deallocate(order)
          allocate(order(2))
          order = ''
          nbrackets = 2
          state = CHILD
        else
          call add_error(stack, &
            'Unexpected character "'//c//'"in ELEMENT specification')
          return
        endif

      elseif (state==PCDATA) then
        if (c.in.'PCDATA') then
          ! record that & carry on
        elseif (c==')') then
          ! check if we have PCDATA
          ! decrement brackets
          if (nbrackets==0) then
            state = AFTERBRACKET
          else
            state = NEXTCHILD
          endif
        elseif (c=='|') then
          order(1) = '|'
          state = NEXTCHILD
        elseif (contents(i:i)==',') then
          call add_error(stack, &
            'Ordered specification not allowed for Mixed elements')
          return
        else
          call add_error(stack, &
            'Unexpected character "'//c//'"in ELEMENT specification')
          return
        endif

      elseif (state==NAME) then
        if ! name
          ! carry on recording
        elseif (c=='?') then
          if (el%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            return
          else
            state = SEPARATOR
          endif
        elseif (c=='+') then
          if (el%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            return
          else
            state = SEPARATOR
          endif
        elseif (c=='*') then
          if (el%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            return
          else
            state = SEPARATOR
          endif
        elseif (c.in.XML_WHITESPACE) then
          state = SEPARATOR
        elseif (c=='(') then
          nbrackets = nbrackets + 1
          temp => order
          allocate(order(nbrackets))
          do j = 1, nbrackets - 1
            order(j) = temp(j)
          enddo
          deallocate(temp)
          order(nbrackets) = ''
          state = CHILD
        else
          call add_error(stack, &
            'Unexpected character "'//c//'" found after element name')
          return
        endif

      elseif (state==CHILD) then
        if (i.in.XML_WHITESPACE) cycle
        if (contents(i:i)=='#') then
          call add_error(stack, &
            '# forbidden except inside first bracket')
          return
        elseif ! a name
          state = NAME
        elseif (contents(i:i)=='(') then
          nbrackets = nbrackets + 1
          temp => order
          allocate(order(nbrackets))
          do j = 1, nbrackets - 1
            order(j) = temp(j)
          enddo
          deallocate(temp)
          order(nbrackets) = ''
        else
          call add_error(stack, &
            'Unexpected character "'//c//'" found after (')
          return
        endif

      elseif (state==SEPARATOR) then
        if (i.in.XML_WHITESPACE) cycle
        if (contents(i:i)=='#') then
          ! error
        elseif (contents(i:i)==',') then
          if (order(nbrackets)=='') then
            order(nbrackets==',')
          elseif (order(nbrackets)=='|') then
            call add_error(stack, &
              'Cannot mix ordered and unordered elements')
          return
          state = CHILD
        elseif (contents(i:i)=='|') then
          if (order(nbrackets)=='') then
            order(nbrackets=='|')
          elseif (order(nbrackets)==',') then
            call add_error(stack, &
              'Cannot mix ordered and unordered elements')
          return
          state = CHILD
        elseif (contents(i:i)==')') then
          nbrackets = nbrackets - 1
          temp => order
          allocate(order(nbrackets))
          do j = 1, nbrackets
            order(j) = temp(j)
          enddo
          deallocate(temp)
          state = AFTERBRACKET
        endif

      elseif (state==AFTERBRACKET) then
        if (c=='*') then
          if (nbrackets==0) state = END
          continue
        elseif (c=='+') then
          if (el%mixed) then
            call add_error(stack, &
              '+ operator disallowed for Mixed elements')
            return
          else
            if (nbrackets==0) state = END
          endif
        elseif (c=='?') then
          if (el%mixed) then
            call add_error(stack, &
              '? operator disallowed for Mixed elements')
            return
          else
            if (nbrackets==0) state = END
          endif
        elseif (c.in.XML_WHITESPACE) then
          if (el%mixed) then
            !FIXME did we have additional elements?
          endif
          if (nbrackets==0) then
            state = END
          else
            state = SEPARATOR
          endif
        elseif (c ==',') then
          if (nbrackets==0) then
            call add_error(stack, &
              'Unexpected "," outside final bracket')
            return
          else
            if (order(nbrackets)=='') then
              order(nbrackets) = ','
            elseif (order(nbrackets)=='|') then
              call add_error(stack, &
                'Cannot mix ordered and unordered elements')
              return
            endif
          endif
          state = CHILD
        elseif (c =='|') then
          if (nbrackets==0) then
            call add_error(stack, &
              'Unexpected "," outside final bracket')
            return
          else
            if (order(nbrackets)=='') then
              order(nbrackets) = '|'
            elseif (order(nbrackets)==',') then
              call add_error(stack, &
                "Cannot mix ordered and unordered elements")
              return
            endif
          endif
          state = CHILD
        else
          call add_error(stack, &
            'Unexpected character "'//c//'"found after ")"')
          return
        endif

      elseif (state==END) then
        if (c.in.XML_WHITESPACE) then
          continue
        else
          call add_error(stack, &
            'Unexpected token found after end of element specification')
        endif

      endif

    enddo

    
    if (state/=END) then
      call add_error(stack, &
        'Incomplete element specification')
    endif

  end subroutine parse_element

end module m_common_element

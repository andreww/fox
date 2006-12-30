module m_common_element

  ! Structure and manipulation of element specification

  implicit none
  private


  type element_t
    character, pointer :: name(:) => null*(
    logical :: empty = .false
    logical :: any = .false.
    logical :: mixed = .false.
    logical :: space = .false.
    !   type(attribute_t), pointer :: attlist => null()
  end type element_t

contains

  subroutine parse_dtd_element(contents, stack, element)
    character(len=*), intent(in) :: contents
    type(error_stack), intent(inout) :: stack
    type(element_t), intent(inout) :: el

    integer :: nbrackets
    character, allocatable :: order(:), name(:)
    character, pointer :: temp(:)

    nbrackets = 0
    state = START

    do i = 1, len(contents)
      c = contents(i:i)

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
          exit
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
            exit
          endif
          state = END
        else
          call add_error(stack, &
            'Unexpected ELEMENT specification; expecting EMPTY or ANY')
          exit
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
          exit
        endif

      elseif (state==PCDATA) then
        if (c.in.'PCDATA') then
          ! record that & carry on
        elseif (c==')') then
          ! check if we have PCDATA
          ! decrement brackets
          if (nbrackets==0) then
            state = END
          else
            state = NEXTCHILD
          endif
        elseif (c=='|') then
          order(1) = '|'
          state = NEXTCHILD
        elseif (contents(i:i)==',') then
          call add_error(stack, &
            'Ordered specification not allowed for Mixed elements')
          exit
        else
          call add_error(stack, &
            'Unexpected character "'//c//'"in ELEMENT specification')
          exit
        endif

      elseif (state==NAME) then
        if ! name
          ! carry on recording
        elseif (c=='?') then
          if (el%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            exit
          else
            state = SEPARATOR
          endif
        elseif (c=='+') then
          if (el%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            exit
          else
            state = SEPARATOR
          endif
        elseif (c=='*') then
          if (el%mixed) then
            call add_error(stack, &
              'Repeat operators forbidden for Mixed elements')
            exit
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
          exit
        endif

      elseif (state==CHILD) then
        if (i.in.XML_WHITESPACE) cycle
        if (contents(i:i)=='#') then
          call add_error(stack, &
            '# forbidden except inside first bracket')
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
          exit
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
          endif
          state = CHILD
        elseif (contents(i:i)=='|') then
          if (order(nbrackets)=='') then
            order(nbrackets=='|')
          elseif (order(nbrackets)==',') then
            call add_error(stack, &
              'Cannot mix ordered and unordered elements')
          endif
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
          continue
        elseif (c=='+') then
          if (el%mixed) then
            call add_error(stack, &
              '+ operator disallowed for Mixed elements')
            exit
          endif
        elseif (c=='?') then
          if (el%mixed) then
            call add_error(stack, &
              '? operator disallowed for Mixed elements')
            exit
          endif
        elseif (c.in.XML_WHITESPACE) then
          if (nbrackets==0) then
            state = END
          else
            state = SEPARATOR
          endif
        elseif (c ==',') then
          if (nbrackets==0) then
            call add_error(stack, &
              'Unexpected "," outside final bracket')
            exit
          else
            if (order(nbrackets)=='') then
              order(nbrackets) = ','
            elseif (order(nbrackets)=='|') then
              call add_error(stack, &
                'Cannot mix ordered and unordered elements')
              exit
            endif
          endif
          state = CHILD
        elseif (c =='|') then
          if (nbrackets==0) then
            call add_error(stack, &
              'Unexpected "," outside final bracket')
            exit
          else
            if (order(nbrackets)=='') then
              order(nbrackets) = '|'
            elseif (order(nbrackets)==',') then
              call add_error(stack, &
                "Cannot mix ordered and unordered elements")
              exit
            endif
          endif
          state = CHILD
        else
          call add_error(stack, &
            'Unexpected character "'//c//'"found after ")"')
          exit
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

  end subroutine parse_element

end module m_common_element

! XML tokenizer

module m_sax_tokenizer

  use m_sax_reader, only: file_buffer_t
  use m_sax_types, only: sax_parser_t

contains

  subroutine sax_tokenizer(fx, fb, signal)
    type(sax_parser_t), intent(in) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: signal


    if (fx%discard_whitespace) then
      c = get_next_character_discarding_whitespace(fb, iostat)
      if (iostat/=0) return

    elseif (fx%long_token) then
      select case(fx%state)
      case (fx%state==ST_PI)
        call get_characters_until_all_of(fb, '?>', iostat)
      case (fx%state==ST_COMMENT)
        call get_characters_until_all_of(fb, '--', iostat)
      case (fx%state==ST_CDATA)
        call get_characters_until_all_of(fb, ']]>', iostat)
      case (fx%state==ST_CHARDATA)
        call get_characters_until_one_of(fb, '<&', iostat)
      end select
      if (iostat/=0) return
      allocate(fx%token(len_namebuffer(fb)))
      fx%token = vs_str(retrieve_namebuffer(fb))

    else
      c = get_next_character_discarding_whitespace(fb, iostat)
      if (iostat/=0) return
      if (c.in.'#>[]?+*()|)'//XML_WHITESPACE) then
        !No further investigation needed, that's the token
        allocate(fx%token(1))
        fx%token = c

      elseif (c.in.initialNameChars) then
        call get_characters_until_not_one_of(fb, nameChars, iostat)
        if (iostat/=0) return
        allocate(fx%token(len_namebuffer(fb)+1))
        fx%token = c//vs_str(retrieve_namebuffer(fb))

      else
        select case(c)

        case ('<')
          c = get_next_character_discarding_whitespace(fb, iostat)
          if (iostat/=0) return
          if (c=='?') then
            allocate(fx%token(2))
            fx%token = '<?'
          elseif (c=='!') then
            allocate(fx%token(2))
            fx%token = '<?'
          elseif (c=='/') then
            allocate(fx%token(2))
            fx%token = '</'
          elseif (c.in.initialNameChar) then
            call put_characters(fb, 1)
            allocate(fx%token(1))
            fx%token = '<'
          else
            !make an error
          endif

        case ('/')
          c = get_next_character_discarding_whitespace(fb, iostat)
          if (iostat/=0) return
          if (c=='>') then
            allocate(fx%token(2))
            fx%token = '/>'
          else
            !make an error
          endif

        case ('%') then
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) then
            !make an error happen
            return
          endif
          if (c.in.whitespace) then
            token = '%'
          elseif (c.in.fx%initialNameChars) then
            call get_characters_until_not_one_of(fb, fx%nameChars, iostat)
            if (iostat/=0) then
              !make an error happen
              return
            endif
            c = get_characters(fb, 1, iostat)
            if (c/=';') then
              !make an error happen
              return
            endif
            !expand & reinvoke parser, truncating entity list
          endif
        case ('&')
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) then
            !make an error happen
            return
          endif
          if (c.in.whitespace) then
            !make an error happen
          elseif (c.in.fx%initialNameChars) then
            call get_characters_until_not_one_of(fb, fx%nameChars, iostat)
            if (iostat/=0) then
              !make an error happen
              return
            endif
            c = get_characters(fb, 1, iostat)
            if (c/=';') then
              !make an error happen
              return
            endif
            !expand & reinvoke parser, truncating entity list
          endif

        case ('"')
          if (fx%context = CTXT_IN_DTD .and. some other condition) then
            call get_characters_until_one_of(fb, '"', iostat)
            if (iostat/=0) return
            call get_namebuffer(fb, fx)
            
          elseif (fx%context = CTXT_IN_CONTENT .an.d some other condition) then
            call get_characters_until_one_of(fb, '"', iostat)
            if (iostat/=0) return
            call get_namebuffer(fb, fx)
            ! expand entities
            ! normalize text

          else
            ! make an error
          endif

        case ("'")
          if (fx%context = CTXT_IN_DTD .and. some other condition) then
            call get_characters_until_one_of(fb, "'", iostat)
            if (iostat/=0) return
            call get_namebuffer(fb, fx)
            
          elseif (fx%context = CTXT_IN_CONTENT .and. some other condition) then
            call get_characters_until_one_of(fb, "'", iostat)
            if (iostat/=0) return
            call get_namebuffer(fb, fx)
            ! expand entities
            ! normalize text

          else
            ! make an error
          endif
          
        case default 
          ! make an error

        end select

      end if ! more than one-char token

    end if ! preserve whitespace

  end subroutine sax_tokenizer

end module m_sax_tokenizer

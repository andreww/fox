! XML tokenizer

module m_sax_tokenizer

  type sax_parser_t
    logical :: discard_whitespace
    integer :: context
    integer :: state
    type(dtd_parser_t) :: dtd_parser
    character, dimension(:), pointer :: token
  end type sax_parser_t

contains

  subroutine sax_tokenizer(fx, fb, signal)
    type(sax_parser_t), intent(in) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: signal


    if (fx%discard_whitespace) then
      c = get_next_character_discarding_whitespace(fb, iostat)
      if (iostat/=0) return
      
    if (fx%preserve_whitespace) then
      if (fx%state==ST_PI) then
        call get_characters_until_all_of(fb, '?>', iostat)
      elseif (fx%state==ST_COMMENT) then
        call get_characters_until_all_of(fb, '--', iostat)
      elseif (fx%state==ST_CDATA) then
        call get_characters_until_all_of(fb, ']]>', iostat)
      elseif (fx%state==ST_CHARDATA) then
        call get_characters_until_one_of(fb, '<&', iostat)
      endif
      if (iostat/=0) then
        !make an error happen
        continue
      endif
      allocate(fx%token(len_namebuffer(fb)))
      fx%token = vs_str(retrieve_namebuffer(fb))

    else
      c = get_next_character_discarding_whitespace(fb, iostat)
      if (iostat/=0); return
      if (c.in.'>[]?+*()|)') then
        !No further investigation needed, that's the token
        allocate(fx%token(1))
        fx%token = c
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

        case default !It's probably a name ...

          call get_next_character_until_not_one_of(fb, XML_WHITESPACE, iostat)
          if (iostat/=0) then
            !make an error
            return
          endif
          allocate(fx%token(len_namebuffer(fb)))
          fx%token = vs_str(retrieve_namebuffer(fb))
        end select

      end if ! more than one-char token

    end if ! preserve whitespace

  end subroutine sax_tokenizer

end module m_sax_tokenizer

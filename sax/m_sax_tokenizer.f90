! XML tokenizer

module m_sax_tokenizer

contains

  subroutine sax_tokenizer(fx, fb, signal)
    type(sax_parser_t), intent(in) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: signal


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
          !is it PI
          !is it ! directive
          !is it DOCTYPE, ELEMENT, ATTLIST, ENTITY, NOTATION, [CDATA
          !is it /
          !is it initialNameChar
          !wtf
          token = '<'

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
          !Depends if we are in DTD or in content
          ! chase along to next ", doing magic with resulting string.

        case ('"') 
          !Depends if we are in DTD or in content
          ! chase along to next ', doing magic with resulting string.

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

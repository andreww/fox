! XML tokenizer

module m_sax_tokenizer

  use m_common_array_str, only: vs_str, str_vs, vs_str_alloc, devnull
  use m_common_charset, only: XML_WHITESPACE, &
    XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML1_0, XML1_1, operator(.in.), upperCase, digits, hexdigits, &
    isInitialNameChar, isXML1_0_NameChar, isXML1_1_NameChar
  use m_common_error, only: ERR_WARNING, add_error, in_error
  use m_common_entities, only: existing_entity, &
    is_unparsed_entity, is_external_entity, expand_entity_text, &
    expand_char_entity, add_internal_entity, pop_entity_list
  use m_common_namecheck, only: checkName, checkCharacterEntityReference

  use m_sax_reader, only: file_buffer_t, rewind_file, &
    read_char, read_chars, push_chars, get_characters, put_characters, &
    get_characters_until_all_of, &
    get_characters_until_one_of, &
    get_characters_until_not_one_of, &
    get_characters_until_condition
  use m_sax_types ! everything, really

  implicit none
  private

  public :: sax_tokenize
  public :: parse_xml_declaration
  public :: normalize_text

contains

  subroutine sax_tokenize(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat

    character :: c, c2

    if (associated(fx%token)) deallocate(fx%token)
    if (associated(fx%next_token)) then
      iostat = 0
      fx%token => fx%next_token
      nullify(fx%next_token)
      return
    endif

    if (fx%state==ST_START_PI) then
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) return
      if (.not.isInitialNameChar(c, fx%xml_version)) then
        call add_error(fx%error_stack, &
          'Invalid PI Name')
        return
      endif
      if (fx%xml_version==XML1_0) then
        call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
      elseif (fx%xml_version==XML1_1) then
        call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
      endif
      fx%token => vs_str_alloc(c//str_vs(fb%namebuffer))
      deallocate(fb%namebuffer)

    elseif (fx%state==ST_PI_CONTENTS) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
      if (iostat/=0) return
      if (size(fb%namebuffer)==0) then
        !token had better be '?>'
        deallocate(fb%namebuffer)
        fx%token => vs_str_alloc(get_characters(fb, 2, iostat))
        if (iostat/=0) return
        if (str_vs(fx%token)/='?>') then
          call add_error(fx%error_stack, "Unexpected token in PI")
          return
        endif
      else
        ! Otherwise token is all chars after whitespace until '?>'
        deallocate(fb%namebuffer)
        call get_characters_until_all_of(fb, '?>', iostat)
        if (iostat/=0) return
        fx%next_token => vs_str_alloc(get_characters(fb, 2, iostat))
        if (iostat/=0) return
        fx%token => fb%namebuffer
        nullify(fb%namebuffer)
      endif

    elseif (fx%state==ST_BANG_TAG) then
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) return
      if (c=='-') then
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='-') then
          fx%token => vs_str_alloc('--')
        else
          call add_error(fx%error_stack, &
            'Unexpected token after <!')
          return
        endif
      elseif (c=='[') then
        fx%token => vs_str_alloc('[')
      elseif (c.in.upperCase) then
        call get_characters_until_not_one_of(fb, upperCase, iostat)
        if (iostat/=0) return
        fx%token => vs_str_alloc(c//str_vs(fb%namebuffer))
        deallocate(fb%namebuffer)
      else
        call add_error(fx%error_stack, &
          'Unexpected token after <!')
        return
      endif

    elseif (fx%state==ST_START_COMMENT) then
      call get_characters_until_all_of(fb, '--', iostat)
      if (iostat/=0) return
      fx%next_token => vs_str_alloc(get_characters(fb, 2, iostat))
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)

    elseif (fx%state==ST_COMMENT_END_2) then
      fx%token => vs_str_alloc(get_characters(fb, 1, iostat))
      if (iostat/=0) return

    elseif (fx%state==ST_CDATA_CONTENTS) then
      call get_characters_until_all_of(fb, ']]>', iostat)
      if (iostat/=0) return
      fx%next_token => vs_str_alloc(get_characters(fb, 3, iostat))
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)

    elseif (fx%state==ST_CHAR_IN_CONTENT) then
      call get_characters_until_one_of(fb, '<&', iostat)
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)

    elseif (fx%state==ST_DTD_ELEMENT_CONTENTS) then
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) return
      if (c.in.XML_WHITESPACE) then
        call get_characters_until_all_of(fb, '>', iostat)
        if (iostat/=0) return
        fx%next_token => vs_str_alloc(get_characters(fb, 1, iostat))
        if (iostat/=0) return
        fx%token => fb%namebuffer
        nullify(fb%namebuffer)
      else
        call add_error(fx%error_stack, &
          'Missing whitespace in element declaration')
      endif

    elseif (fx%state==ST_DTD_ATTLIST_CONTENTS) then
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) return
      if (c.in.XML_WHITESPACE) then
        call get_characters_until_all_of(fb, '>', iostat)
        if (iostat/=0) return
        fx%next_token => vs_str_alloc(get_characters(fb, 1, iostat))
        if (iostat/=0) return
        fx%token => fb%namebuffer
        nullify(fb%namebuffer)
      elseif (c==">") then
        fx%token => vs_str_alloc(">")
      else
        call add_error(fx%error_stack, &
          'Missing whitespace in attlist declaration')
      endif

    elseif (fx%state==ST_IN_TAG) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
      if (iostat/=0) return
      if (size(fb%namebuffer)==0) then
        deallocate(fb%namebuffer) ! which only contains whitespace now.
        ! This had better be the end of the tag.
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='>') then
          fx%token => vs_str_alloc('>')
        elseif (c=='/') then
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='>') then
            fx%token => vs_str_alloc('/>')
          endif
        else
          call add_error(fx%error_stack, "Unexpected character in tag.")
        endif
      else
        deallocate(fb%namebuffer) ! which only contains whitespace now.
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='>') then
          fx%token => vs_str_alloc('>')
        elseif (c=='/') then
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='>') then
            fx%token => vs_str_alloc('/>')
          endif
        elseif (isInitialNameChar(c, fx%xml_version)) then 
          if (fx%xml_version==XML1_0) then
            call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
          elseif (fx%xml_version==XML1_1) then
            call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
          endif
          fx%token => vs_str_alloc(c//str_vs(fb%namebuffer))
          deallocate(fb%namebuffer)
        else
          call add_error(fx%error_stack, "Unexpected character in tag.")
        endif
      endif

    elseif (fx%state==ST_ATT_EQUALS) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
      if (iostat/=0) return
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) return
      if (c/='"'.and.c/="'") then
        call add_error(fx%error_stack, "Expecting "" or '")
        return
      endif
      call get_characters_until_all_of(fb, c, iostat)
      if (iostat/=0) return
      fx%token => normalize_text(fx, fb%namebuffer)
      ! Next character is either quotechar or eof.
      c = get_characters(fb, 1, iostat)
      ! Either way, we return

    elseif (fx%context==CTXT_IN_DTD) then
      
      if (fx%whitespace==WS_FORBIDDEN) then
        ! This MUST be the end of a comment
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        fx%token => vs_str_alloc(c)
        ! it will either be a <!DIRECTIVE or a <?PINAME
        !call get_characters_until_one_of(fb, XML_WHITESPACE, iostat)
        !if (iostat/=0) return
        !fx%token => fb%namebuffer
        !nullify(fb%namebuffer)

      elseif (fx%whitespace==WS_MANDATORY) then
        !We are still allowed a '>' without space, ... check first.
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
        if (iostat/=0) return
        if (size(fb%namebuffer)==0) then
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='>') then
            fx%token => vs_str_alloc(c)
          else
            call add_error(fx%error_stack, "Required whitespace omitted in DTD")
          endif
          return
        endif
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        ! do some stuff
        if (c.in."%>[]") then
          fx%token => vs_str_alloc(c)
        elseif (c=='<') then
          !it's a comment or a PI ... or a DTD keyword.
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='?') then
            fx%token => vs_str_alloc('<?')
          elseif (c=='!') then
            fx%token => vs_str_alloc('<!')
          endif
        elseif (c=='"'.or.c=="'") then ! grab until next quote
          call get_characters_until_all_of(fb, c, iostat)
          if (iostat/=0) return
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          fx%token => vs_str_alloc(c//str_vs(fb%namebuffer)//c)
          deallocate(fb%namebuffer)
        else !it must be a NAME for some reason
          call put_characters(fb, 1)
          if (fx%xml_version==XML1_0) then
            call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
          elseif (fx%xml_version==XML1_1) then
            call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
          endif
          fx%token => fb%namebuffer
          nullify(fb%namebuffer)
        endif

      elseif (fx%whitespace==WS_DISCARD) then
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
        if (iostat/=0) return
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c.in.">[]") then
          fx%token => vs_str_alloc(c)
        elseif (c=='<') then
          !it's a comment or a PI ... or a DTD keyword.
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='?') then
            fx%token => vs_str_alloc('<?')
          elseif (c=='!') then
            fx%token => vs_str_alloc('<!')
          else
            call add_error(fx%error_stack, &
              "Tokenizing error in the DTD")
            return
          endif
        elseif (c=='%') then
          !It ought to be a parameter entity reference
          if (fx%xml_version==XML1_0) then
            call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
          elseif (fx%xml_version==XML1_1) then
            call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
          endif
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c/=';') then
            call add_error(fx%error_stack, "Illegal parameter entity reference.")
            return
          endif
          fx%token => vs_str_alloc('%'//str_vs(fb%namebuffer)//';')
        endif
      endif
      return


    else

      if (fx%whitespace==WS_DISCARD) then
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
        if (iostat/=0) return

      elseif (fx%whitespace==WS_MANDATORY) then
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
        if (size(fb%namebuffer)==0) then
          call add_error(fx%error_stack, 'Tokenizer expected whitespace')
          return
        endif
      endif

      c = get_characters(fb, 1, iostat)
      if (iostat/=0) return

      if (c.in.'>=[') then
        !No further investigation needed, that's the token
        fx%token => vs_str_alloc(c)

      elseif (isInitialNameChar(c, fx%xml_version)) then
        if (fx%xml_version==XML1_0) then
          call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
        elseif (fx%xml_version==XML1_1) then
          call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
        endif
        if (iostat/=0) return
        fx%token => vs_str_alloc(c//str_vs(fb%namebuffer))
        deallocate(fb%namebuffer)

      else
        select case(c)

        case ('<')
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='?') then
            fx%token => vs_str_alloc('<?')
          elseif (c=='!') then
            fx%token => vs_str_alloc('<!')
          elseif (c=='/') then
            fx%token => vs_str_alloc('</')
          elseif (isInitialNameChar(c, fx%xml_version)) then
            call put_characters(fb, 1)
            fx%token => vs_str_alloc('<')
          else
            call add_error(fx%error_stack,"Unexpected character found.")
          endif

        case ('/')
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='>') then
            fx%token => vs_str_alloc('/>')
          else
            call add_error(fx%error_stack, "Unexpected character after /")
          endif

        case ('&')
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='#') then
            c = get_characters(fb, 1, iostat)
            if (iostat/=0) return
            if (c=='x') then
              call get_characters_until_not_one_of(fb, hexdigits, iostat)
              if (size(fb%namebuffer)==0) then
                call add_error(fx%error_stack, 'Illegal character entity reference')
                return
              endif
              if (iostat/=0) return
              c2 = get_characters(fb, 1, iostat)
              if (iostat/=0) return
              if (c2/=';') then
                call add_error(fx%error_stack, &
                  'Illegal character entity reference')
                return
              endif
              fx%token => vs_str_alloc('&#x'//str_vs(fb%namebuffer)//';')
              deallocate(fb%namebuffer)
            elseif (c.in.digits) then
              call get_characters_until_not_one_of(fb, digits, iostat)
              if (iostat/=0) return
              c2 = get_characters(fb, 1, iostat)
              if (iostat/=0) return
              if (c2/=';') then
                call add_error(fx%error_stack, 'Illegal character entity reference')
                return
              endif
              fx%token => vs_str_alloc('&#'//c//str_vs(fb%namebuffer)//';')
              deallocate(fb%namebuffer)
            else
              call add_error(fx%error_stack, 'Illegal character entity reference')
            endif
          elseif (isInitialNameChar(c, fx%xml_version)) then
            if (fx%xml_version==XML1_0) then
              call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
            elseif (fx%xml_version==XML1_1) then
              call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
            endif
            if (iostat/=0) return
            c2 = get_characters(fb, 1, iostat)
            if (iostat/=0) return
            if (c2/=';') then
              call add_error(fx%error_stack, "Illegal general entity reference")
              return
            endif
            fx%token => vs_str_alloc('&'//c//str_vs(fb%namebuffer)//';')
            deallocate(fb%namebuffer)
          else
            call add_error(fx%error_stack, &
              'Illegal general entity reference')
            return
          endif

        case ('"')
          if (fx%context==CTXT_IN_CONTENT) then !.an.d some other condition) then
            call get_characters_until_one_of(fb, '"', iostat)
            if (iostat/=0) return
            deallocate(fb%namebuffer)
            fx%token => vs_str_alloc('"'//str_vs(fb%namebuffer)//'"')
          else
            ! make an error
          endif

        case ("'")
          if (fx%context==CTXT_IN_CONTENT) then! .and. some other condition) then
            call get_characters_until_one_of(fb, "'", iostat)
            if (iostat/=0) return
            deallocate(fb%namebuffer)
            fx%token = vs_str_alloc("'"//str_vs(fb%namebuffer)//"'")

          else

            call add_error(fx%error_stack, "Unrecognized token.")
            return
          endif

        case default 
          ! make an error

        end select

      end if ! more than one-char token
    endif

  end subroutine sax_tokenize


  subroutine parse_xml_declaration(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    ! Need to do this with read_* fns, not get_*
    ! FIXME below all read_chars should be error-checked.
    integer :: i
    character(len=*), parameter :: version="version", encoding="encoding", standalone="standalone"
    character :: c
    character(len=5) :: xml = "<?xml"
    character, allocatable :: ch(:)
    ! default values ...
    fx%xml_version = XML1_0
    allocate(fx%encoding(5))
    fx%encoding = vs_str("UTF-8")
    fx%standalone = .false.
    do i = 1, 5
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=xml(i:i)) then
        call rewind_file(fb)
        return
      endif
    enddo
    c = read_char(fb, iostat); if (iostat/=0) return
    if (.not.(c.in.XML_WHITESPACE)) then
      call rewind_file(fb)
      return
    endif
    do while (c.in.XML_WHITESPACE)
      c = read_char(fb, iostat); if (iostat/=0) return
    enddo
    call push_chars(fb, c)
    allocate(ch(7))
    ch = vs_str(read_chars(fb, 7, iostat)); if (iostat/=0) return
    if (str_vs(ch)/="version") then
      deallocate(ch)
      call add_error(fx%error_stack, "Expecting XML version"); return
    endif
    deallocate(ch)
    call check_version
    if (iostat/=0) return
    c = read_char(fb, iostat); if (iostat/=0) return
    if (.not.(c.in.XML_WHITESPACE).and.c/='?') then
      call add_error(fx%error_stack, "Missing whitespace in XML declaration");return
    endif
    do while (c.in.XML_WHITESPACE)
      c = read_char(fb, iostat); if (iostat/=0) return
    enddo
    if (c=='?') then
      c = read_char(fb, iostat); if (iostat/=0) return
      ! FIXME read_char io_eor handling
      if (c/='>') then
        call add_error(fx%error_stack, "Expecting > to end XML declaration"); return
      endif
      return
    endif
    call push_chars(fb, c)
    allocate(ch(8))
    ch = vs_str(read_chars(fb, 8, iostat)); if (iostat/=0) return
    if (str_vs(ch)/="encoding") then
      call push_chars(fb, str_vs(ch))
      deallocate(ch)
      allocate(ch(10))
      ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="standalone") then
        deallocate(ch)
        call add_error(fx%error_stack, "Expecting XML encoding or standalone declaration"); return
      endif
      deallocate(ch)
      call check_standalone
      if (iostat/=0) return
    else
      deallocate(ch)
      call check_encoding
      if (iostat/=0) return
      c = read_char(fb, iostat); if (iostat/=0) return
      if (.not.(c.in.XML_WHITESPACE).and.c/='?') then
        call add_error(fx%error_stack, "Missing whitespace in XML declaration");return
      endif
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c=='?') then
        c = read_char(fb, iostat); if (iostat/=0) return
        ! FIXME read_char io_eor handling
        if (c/='>') then
          call add_error(fx%error_stack, "Expecting > to end XML declaration"); return
        endif
        return
      endif
      call push_chars(fb, c)
      allocate(ch(10))
      ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="standalone") then
        deallocate(ch)
        call add_error(fx%error_stack, "Expecting XML encoding or standalone declaration"); return
      endif
      deallocate(ch)
      call check_standalone
      if (iostat/=0) return
    endif
    c = read_char(fb, iostat); if (iostat/=0) return
    if (.not.(c.in.XML_WHITESPACE).and.c/='?') then
      call add_error(fx%error_stack, "Missing whitespace in XML declaration");return
    endif
    do while (c.in.XML_WHITESPACE)
      c = read_char(fb, iostat); if (iostat/=0) return
    enddo
    if (c=='?') then
      c = read_char(fb, iostat); if (iostat/=0) return
      ! FIXME read_char io_eor handling
      if (c/='>') then
        call add_error(fx%error_stack, "Expecting > to end XML declaration"); return
      endif
    endif

    if (str_vs(fx%encoding)/="UTF-8") then
      call add_error(fx%error_stack, "Unknown character encoding in XML declaration", ERR_WARNING)
    endif

  contains

    subroutine check_version
      character :: c, quotechar
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call add_error(fx%error_stack, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_error(fx%error_stack, "Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1") then
        call add_error(fx%error_stack, "Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=".") then
        call add_error(fx%error_stack, "Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1".and.c/="0") then
        call add_error(fx%error_stack, "Unknown XML version"); return
      endif
      if (c=="1") then
        fx%xml_version = XML1_1
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call add_error(fx%error_stack, "Expecting "//quotechar); return
      endif
    end subroutine check_version

    subroutine check_encoding
      character :: c, quotechar
      character, dimension(:), pointer :: buf, tempbuf
      integer :: i
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call add_error(fx%error_stack, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_error(fx%error_stack, "Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (.not.(c.in.XML_INITIALENCODINGCHARS)) then
        call add_error(fx%error_stack, "Illegal character at start of encoding declaration."); return
      endif
      i = 1
      buf => vs_str_alloc(c)
      c = read_char(fb, iostat)
      if (iostat/=0) then
        deallocate(buf)
        return
      endif
      do while (c.in.XML_ENCODINGCHARS)
        tempbuf => buf
        i = i+1
        allocate(buf(i))
        buf(:i-1) = tempbuf
        deallocate(tempbuf)
        buf(i) = c
        c = read_char(fb, iostat)
        if (iostat/=0) then
          deallocate(buf)
          return
        endif
      enddo
      if (c/=quotechar) then
        call add_error(fx%error_stack, "Illegal character in XML encoding declaration; expecting "//quotechar); 
        deallocate(buf)
        return
      endif
      deallocate(fx%encoding)
      fx%encoding => buf
    end subroutine check_encoding

    subroutine check_standalone
      character :: c, quotechar
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call add_error(fx%error_stack, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_error(fx%error_stack, "Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c=="y") then
        c = read_char(fb, iostat); if (iostat/=0) return
        if (c=="e") then
          c = read_char(fb, iostat); if (iostat/=0) return
          if (c=="s") then
            fx%standalone = .true.
          else
            call add_error(fx%error_stack, "standalone accepts only 'yes' or 'no'"); return
          endif
        else
          call add_error(fx%error_stack, "standalone accepts only 'yes' or 'no'"); return
        endif
      elseif (c=="n") then
        c = read_char(fb, iostat); if (iostat/=0) return
        if (c=="o") then
          fx%standalone = .false.
        else
          call add_error(fx%error_stack, "standalone accepts only 'yes' or 'no'"); return
        endif
      else
        call add_error(fx%error_stack, "standalone accepts only 'yes' or 'no'"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call add_error(fx%error_stack, "Expecting "" or '"); return
      endif
    end subroutine check_standalone

  end subroutine parse_xml_declaration


  recursive function normalize_text(fx, s_in) result(s_out)
    type(sax_parser_t), intent(inout) :: fx
    character, dimension(:), intent(in) :: s_in
    character, dimension(:), pointer :: s_out

    character, dimension(:), pointer :: s_temp, s_temp2, s_ent, tempString
    integer :: i, i2, j

    ! Condense all whitespace, only if we are validating,
    ! Expand all &
    ! Complain about < and &

    allocate(s_temp(size(s_in))) ! in the first instance
    allocate(s_out(0)) ! in case we return early ...
    s_ent => null()
    tempString => null()

    i2 = 1
    i = 1
    do 
      if (i > size(s_in)) exit
      ! Firstly, all whitespace must become 0x20
      if (s_in(i).in.XML_WHITESPACE) then
        s_temp(i2) = " "
        ! Then, < is always illegal
        i = i + 1
        i2 = i2 + 1
      elseif (s_in(i)=='<') then
        call add_error(fx%error_stack, "Illegal < found in attribute.")
        goto 100
        ! Then, expand <
      elseif (s_in(i)=='&') then
        j = index(str_vs(s_in(i+1:)), ';')
        if (j==0) then
          call add_error(fx%error_stack, "Illegal & found in attribute")
          goto 100
        elseif (j==1) then
          call add_error(fx%error_stack, "No entity reference found")
          goto 100
        endif
        allocate(tempString(j-1))
        tempString = s_in(i+1:i+j-1)
        if (existing_entity(fx%predefined_e_list, str_vs(tempString))) then
            ! Expand immediately
          s_temp(i2) = expand_entity_text(fx%predefined_e_list, str_vs(tempString))
          i = i + j + 1
          i2 = i2 + 1
        elseif (checkCharacterEntityReference(str_vs(tempString), fx%xml_version)) then
          ! Expand all character entities
          s_temp(i2) = expand_char_entity(str_vs(tempString)) ! FIXME ascii
          i = i + j  + 1
          i2 = i2 + 1 ! fixme
        elseif (checkName(str_vs(tempString), fx%xml_version)) then
          if (existing_entity(fx%forbidden_ge_list, str_vs(tempString))) then
            call add_error(fx%error_stack, 'Recursive entity expansion')
            goto 100
          elseif (existing_entity(fx%ge_list, str_vs(tempString))) then
            !is it the right sort of entity?
            if (is_unparsed_entity(fx%ge_list, str_vs(tempString))) then
              call add_error(fx%error_stack, "Unparsed entity forbidden in attribute")
              goto 100
            elseif (is_external_entity(fx%ge_list, str_vs(tempString))) then
              call add_error(fx%error_stack, "External entity forbidden in attribute")
              goto 100
            endif
            call add_internal_entity(fx%forbidden_ge_list, str_vs(tempString), "", fx%xml_version)
            ! Recursively expand entity, checking for errors.
            s_ent => normalize_text(fx, vs_str(expand_entity_text(fx%ge_list, str_vs(tempString))))
            call devnull(pop_entity_list(fx%forbidden_ge_list))
            if (in_error(fx%error_stack)) then
              goto 100
            endif
            allocate(s_temp2(size(s_temp)+size(s_ent)-j))
            s_temp2(:i2-1) = s_temp(:i2-1)
            s_temp2(i2:i2+size(s_ent)-1) = s_ent
            deallocate(s_temp)
            s_temp => s_temp2
            nullify(s_temp2)
            i = i + j + 1
            i2 = i2 + size(s_ent)
            deallocate(s_ent)
          else
            s_temp(i2:i2+j) = s_in(i:i+j)
            i = i + j + 1
            i2 = i2 + j + 1
            if (.not.fx%skippedExternal.or.fx%standalone) then
              call add_error(fx%error_stack, "Undeclared entity encountered in standalone document.")
              goto 100
            endif
          endif
        else
          call add_error(fx%error_stack, "Illegal entity reference")
          goto 100
        endif
        deallocate(tempString)
      else
        s_temp(i2) = s_in(i)
        i = i + 1
        i2 = i2 + 1
      endif
    enddo
    
    deallocate(s_out)
    allocate(s_out(i2-1))
    s_out = s_temp(:i2-1)
100 deallocate(s_temp)
    if (associated(s_ent))  deallocate(s_ent)
    if (associated(tempString)) deallocate(tempString)

  end function normalize_text
    
end module m_sax_tokenizer

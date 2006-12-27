! XML tokenizer

module m_sax_tokenizer

  use m_common_array_str, only: vs_str, str_vs
  use m_common_charset, only: XML_WHITESPACE, &
    XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML1_0, XML1_1, operator(.in.), &
    isInitialNameChar, isNameChar, isXML1_0_NameChar, isXML1_1_NameChar
  use m_common_error, only: ERR_WARNING, ERR_ERROR, error_t
  use m_common_entities, only: entity_list, shallow_copy_entity_list_without, &
    shallow_destroy_entity_list, existing_entity, is_unparsed_entity, &
    is_external_entity, expand_entity_text, expand_char_entity
  use m_common_format, only: str
  use m_common_namecheck, only: checkName, checkCharacterEntityReference

  use m_sax_reader, only: file_buffer_t, rewind_file, &
    read_char, read_chars, push_chars, get_characters, put_characters, &
    get_characters_until_all_of, &
    get_characters_until_one_of, &
    get_characters_until_not_one_of, &
    get_characters_until_condition, &
    next_chars_are
  use m_sax_types ! everything, really

  implicit none
  private

  public :: sax_tokenize
  public :: parse_xml_declaration
  public :: add_parse_error

contains

  subroutine sax_tokenize(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat

    character :: c

    print*,'tokenizing... discard whitespace?', fx%whitespace

    if (associated(fx%token)) deallocate(fx%token)
    if (associated(fx%next_token)) then
      iostat = 0
      fx%token => fx%next_token
      nullify(fx%next_token)
      return
    endif

    if (fx%state==ST_PI_CONTENTS) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
      if (iostat/=0) return
      if (size(fb%namebuffer)==0) then
        !token had better be '?>'
        deallocate(fb%namebuffer)
        allocate(fx%token(2))
        fx%token = get_characters(fb, 2, iostat)
        if (iostat/=0) return
        if (str_vs(fx%token)/='?>') then
          call add_parse_error(fx, "Unexpected token in PI")
        endif
        return
      endif
      ! Otherwise token is all chars after whitespace until '?>'
      deallocate(fb%namebuffer)
      call get_characters_until_all_of(fb, '?>', iostat)
      if (iostat/=0) return
      allocate(fx%next_token(2))
      fx%next_token = vs_str(get_characters(fb, 2, iostat))
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)
      return

    elseif (fx%state==ST_START_COMMENT) then
      call get_characters_until_all_of(fb, '--', iostat)
      if (iostat/=0) return
      allocate(fx%next_token(2))
      fx%next_token = vs_str(get_characters(fb, 2, iostat))
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)
      return

    elseif (fx%state==ST_CDATA_CONTENTS) then
      call get_characters_until_all_of(fb, ']]>', iostat)
      if (iostat/=0) return
      allocate(fx%next_token(3))
      fx%next_token = vs_str(get_characters(fb, 3, iostat))
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)
      return

    elseif (fx%state==ST_CHAR_IN_CONTENT) then
      call get_characters_until_one_of(fb, '<&', iostat)
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)
      return

    elseif (fx%state==ST_IN_TAG) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
      if (iostat/=0) return
      if (size(fb%namebuffer)==0) then
        deallocate(fb%namebuffer)
        ! This had better be the end of the tag.
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='>') then
          allocate(fx%token(1))
          fx%token = vs_str('>')
        elseif (c=='/') then
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='>') then
            allocate(fx%token(2))
            fx%token = vs_str('/>')
          endif
        else
          call add_parse_error(fx, "Unexpected character in tag.")
          return
        endif
      else
        deallocate(fb%namebuffer)
        ! This had better be a name ...
        if (fx%xml_version==XML1_0) then
          call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
        elseif (fx%xml_version==XML1_1) then
          call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
        endif
        fx%token => fb%namebuffer
        nullify(fb%namebuffer)
        if (size(fx%token)==0) then
          call add_parse_error(fx, "Unexpected character in tag")
        endif
      endif
      return
      
    elseif (fx%state==ST_ATT_EQUALS) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
      if (iostat/=0) return
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) return
      if (c/='"'.and.c/="'") then
        call add_parse_error(fx, "Expecting "" or '")
        return
      endif
      call get_characters_until_all_of(fb, c, iostat)
      if (iostat/=0) return
      fx%token => normalize_text(fx, fx%ge_list, fb%namebuffer)
      ! if this is a confirmed CDATA attribute, normalize away
      ! all multiple spaces. FIXME actually, do that in parser.
      ! Next character is either quotechar or eof.
      c = get_characters(fb, 1, iostat)
      ! Either way, we return
      return

    elseif (fx%context==CTXT_IN_DTD) then
      print*,'context', fx%whitespace
      if (fx%whitespace==WS_MANDATORY) then
        !We are still allowed a '>' without space, ... check first.
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
        if (iostat/=0) return
        if (size(fb%namebuffer)==0) then
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='>') then
            allocate(fx%token(1))
            fx%token = c
          else
            call add_parse_error(fx, "Required whitespace omitted in DTD")
          endif
          return
        endif
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        ! do some stuff
        if (c.in."%#>[]+*()|,") then
          allocate(fx%token(1))
          fx%token = c
        elseif (c=='<') then
          !it's a comment or a PI ... or a DTD keyword.
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='?') then
            allocate(fx%token(2))
            fx%token = vs_str('<?')
          elseif (c=='!') then
            allocate(fx%token(2))
            fx%token = vs_str('<!')
          endif
        elseif (c=='"'.or.c=='"') then ! grab until next quote
          call get_characters_until_all_of(fb, c, iostat)
          if (iostat/=0) return
          allocate(fx%token(size(fb%namebuffer)+2))
          fx%token(1) = c
          fx%token(2:size(fx%token)-1) = fb%namebuffer
          deallocate(fb%namebuffer)
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          fx%token(size(fx%token)) = c
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
      elseif (fx%whitespace==WS_FORBIDDEN) then
        print*,'WS_FORBIDDEN'
        ! it must be ATTLIST, ELEMENT, ENTITY, NOTATION
        call get_characters_until_not_one_of(fb, 'ATLISENYMOAI', iostat)
        if (iostat/=0) return
        fx%token => fb%namebuffer
        nullify(fb%namebuffer)
      elseif (fx%whitespace==WS_DISCARD) then
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
        if (iostat/=0) return
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c.in."#>[]+*()|,") then
          allocate(fx%token(1))
          fx%token = c
        elseif (c=='<') then
          !it's a comment or a PI ... or a DTD keyword.
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) return
          if (c=='?') then
            allocate(fx%token(2))
            fx%token = vs_str('<?')
          elseif (c=='!') then
            allocate(fx%token(2))
            fx%token = vs_str('<!')
          endif
        endif
      endif
      return
      ! elseif (fx%state = ST_DTD_ATTLIST_CONTENTS
      ! elseif (fx%state = ST_DTD_ELEMENT_CONTENTS


    endif

    print*, 'statecontext', fx%state, fx%context, fx%whitespace

    if (fx%whitespace==WS_DISCARD) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
      if (iostat/=0) return

    elseif (fx%whitespace==WS_MANDATORY) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat)
      if (size(fb%namebuffer)==0) then
        call add_parse_error(fx, 'Tokenizer expected whitespace')
        return
      endif
    endif

    c = get_characters(fb, 1, iostat)
    if (iostat/=0) return

    if (c.in.'#>[]+*()|='//XML_WHITESPACE) then
      !No further investigation needed, that's the token
      allocate(fx%token(1))
      fx%token = c

    elseif (isInitialNameChar(c, fx%xml_version)) then
      if (fx%xml_version==XML1_0) then
        call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
      elseif (fx%xml_version==XML1_1) then
        call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
      endif
      if (iostat/=0) return
      allocate(fx%token(size(fb%namebuffer)+1))
      fx%token(1) = c
      fx%token(2:) = fb%namebuffer
      deallocate(fb%namebuffer)

    else
      select case(c)

      case ('<')
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='?') then
          allocate(fx%token(2))
          fx%token = vs_str('<?')
        elseif (c=='!') then
          allocate(fx%token(2))
          fx%token = vs_str('<!')
        elseif (c=='/') then
          allocate(fx%token(2))
          fx%token = vs_str('</')
        elseif (isInitialNameChar(c, fx%xml_version)) then
          call put_characters(fb, 1)
          allocate(fx%token(1))
          fx%token = '<'
        else
          call add_parse_error(fx,"Unexpected character found.")
        endif

      case ('/')
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='>') then
          allocate(fx%token(2))
          fx%token = vs_str('/>')
        else
          call add_parse_error(fx, "Unexpected character after /")
        endif

      case ('?')
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='>') then
          allocate(fx%token(2))
          fx%token = vs_str('?>')
        else
          call put_characters(fb, 1)
          allocate(fx%token(1))
          fx%token = '?'
        endif

      case ('-')
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='-') then
          allocate(fx%token(2))
          fx%token = vs_str('--')
        else
          call add_parse_error(fx, "Unexpected character after =") !FIXME is this right?
        endif

      case ('%')
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) then
          !make an error happen
          return
        endif
        if (c.in.XML_WHITESPACE) then
          allocate(fx%token(1))
          fx%token = '%'
        elseif (isInitialNameChar(c, fx%xml_version)) then
          if (fx%xml_version==XML1_0) then
            call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
          elseif (fx%xml_version==XML1_1) then
            call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
          endif
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
        if (c.in.XML_WHITESPACE) then
          !make an error happen
        elseif (isInitialNameChar(c, fx%xml_version)) then
          if (fx%xml_version==XML1_0) then
            call get_characters_until_condition(fb, isXML1_0_NameChar, .false., iostat)
          elseif (fx%xml_version==XML1_1) then
            call get_characters_until_condition(fb, isXML1_1_NameChar, .false., iostat)
          endif
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
        if (fx%context==CTXT_IN_DTD) then! .and. some other condition) then
          call get_characters_until_one_of(fb, '"', iostat)
          if (iostat/=0) return
          allocate(fx%token(size(fb%namebuffer)+2))
          fx%token(1) = '"'
          fx%token(2:size(fx%token)-1) = fb%namebuffer
          fx%token(size(fx%token)) = '"'
          deallocate(fb%namebuffer)
          ! inefficient copying ...

        elseif (fx%context==CTXT_IN_CONTENT) then !.an.d some other condition) then
          call get_characters_until_one_of(fb, '"', iostat)
          if (iostat/=0) return
          allocate(fx%token(size(fb%namebuffer)+2))
          fx%token(1) = '"'
          fx%token(2:size(fx%token)-1) = fb%namebuffer
          fx%token(size(fx%token)) = '"'
          deallocate(fb%namebuffer)
          ! inefficient copying ...
          ! normalize text

        else
          ! make an error
        endif

      case ("'")
        if (fx%context==CTXT_IN_DTD) then! .and. some other condition) then
          call get_characters_until_one_of(fb, "'", iostat)
          if (iostat/=0) return
          allocate(fx%token(size(fb%namebuffer)+2))
          fx%token(1) = "'"
          fx%token(2:size(fx%token)-1) = fb%namebuffer
          fx%token(size(fx%token)) = "'"
          deallocate(fb%namebuffer)
          ! inefficient copying ...

        elseif (fx%context==CTXT_IN_CONTENT) then! .and. some other condition) then
          call get_characters_until_one_of(fb, "'", iostat)
          if (iostat/=0) return
          allocate(fx%token(size(fb%namebuffer)+2))
          fx%token(1) = "'"
          fx%token(2:size(fx%token)-1) = fb%namebuffer
          fx%token(size(fx%token)) = "'"
          deallocate(fb%namebuffer)
          ! inefficient copying ...
          ! normalize text

        else

          call add_parse_error(fx, "Unrecognized token.")
          return
        endif

      case default 
        ! make an error

      end select

    end if ! more than one-char token

  end subroutine sax_tokenize


  subroutine parse_xml_declaration(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    ! Need to do this with read_* fns, not get_*
    ! FIXME below all read_chars should be error-checked.
    integer :: i
    character(len=*), parameter :: version="version", encoding="encoding", standalone="standalone"
    character :: c, quotation_mark
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
      call add_parse_error(fx, "Expecting XML version"); return
    endif
    deallocate(ch)
    call check_version
    if (iostat/=0) return
    c = read_char(fb, iostat); if (iostat/=0) return
    do while (c.in.XML_WHITESPACE)
      c = read_char(fb, iostat); if (iostat/=0) return
    enddo
    if (c=='?') then
      c = read_char(fb, iostat); if (iostat/=0) return
      ! FIXME read_char io_eor handling
      if (c/='>') then
        call add_parse_error(fx, "Expecting > to end XML declaration"); return
      endif
      return
    endif
    call push_chars(fb, c)
    allocate(ch(8))
    ch = vs_str(read_chars(fb, 8, iostat)); if (iostat/=0) return
    if (str_vs(ch)/="encoding") then
      call push_chars(fb, "encoding")
      deallocate(ch)
      allocate(ch(10))
      ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="standalone") then
        deallocate(ch)
        call add_parse_error(fx, "Expecting XML encoding or standalone declaration"); return
      endif
      deallocate(ch)
      call check_standalone
      if (iostat/=0) return
    else
      deallocate(ch)
      call check_encoding
      if (iostat/=0) return
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c=='?') then
        c = read_char(fb, iostat); if (iostat/=0) return
        ! FIXME read_char io_eor handling
        if (c/='>') then
          call add_parse_error(fx, "Expecting > to end XML declaration"); return
        endif
        return
      endif
      call push_chars(fb, c)
      allocate(ch(10))
      ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="standalone") then
        deallocate(ch)
        call add_parse_error(fx, "Expecting XML encoding or standalone declaration"); return
      endif
      deallocate(ch)
      call check_standalone
      if (iostat/=0) return
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c=='?') then
        c = read_char(fb, iostat); if (iostat/=0) return
        ! FIXME read_char io_eor handling
        if (c/='>') then
          call add_parse_error(fx, "Expecting > to end XML declaration"); return
        endif
      endif
    endif

    if (str_vs(fx%encoding)/="UTF-8") then
      call add_parse_error(fx, "Unknown character encoding in XML declaration", ERR_WARNING)
    endif

  contains

    subroutine check_version
      character :: c, quotechar
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call add_parse_error(fx, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_parse_error(fx, "Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1") then
        call add_parse_error(fx, "Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=".") then
        call add_parse_error(fx, "Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1".and.c/="0") then
        call add_parse_error(fx, "Unknown XML version"); return
      endif
      if (c=="1") then
        fx%xml_version = XML1_1
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call add_parse_error(fx, "Expecting "//quotechar); return
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
        call add_parse_error(fx, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_parse_error(fx, "Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (.not.(c.in.XML_INITIALENCODINGCHARS)) then
        call add_parse_error(fx, "Illegal character at start of encoding declaration."); return
      endif
      i = 1
      allocate(buf(1))
      buf(1) = c
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
        call add_parse_error(fx, "Illegal character in XML encoding declaration; expecting "//quotechar); return
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
        call add_parse_error(fx, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_parse_error(fx, "Expecting "" or '"); return
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
            call add_parse_error(fx, "standalone accepts only 'yes' or 'no'"); return
          endif
        else
          call add_parse_error(fx, "standalone accepts only 'yes' or 'no'"); return
        endif
      elseif (c=="n") then
        c = read_char(fb, iostat); if (iostat/=0) return
        if (c=="o") then
          fx%standalone = .false.
        else
          call add_parse_error(fx, "standalone accepts only 'yes' or 'no'"); return
        endif
      else
        call add_parse_error(fx, "standalone accepts only 'yes' or 'no'"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call add_parse_error(fx, "Expecting "" or '"); return
      endif
    end subroutine check_standalone

  end subroutine parse_xml_declaration


  recursive function normalize_text(fx, e_list, s_in) result(s_out)
    type(sax_parser_t), intent(inout) :: fx
    type(entity_list), intent(in) :: e_list !WTF
    character, dimension(:), intent(in) :: s_in
    character, dimension(:), pointer :: s_out

    type(entity_list) :: shortened_entity_list
    character, dimension(:), pointer :: s_temp, s_temp2, s_ent
    integer :: i, i2, j
    logical :: w

    ! Condense all whitespace, only if we are validating,
    ! Expand all &
    ! Complain about < and &

    allocate(s_temp(size(s_in))) ! in the first instance

    i2 = 1
    i = 1
    do 
      print*,'i ',i,' size ',size(s_IN)
      if (i > size(s_in)) exit
      ! Firstly, all whitespace must become 0x20
      if (s_in(i).in.XML_WHITESPACE) then
        s_temp(i2) = " "
        ! Then, < is always illegal
        i = i + 1
        i2 = i2 + 1
      elseif (s_in(i)=='<') then
        call add_parse_error(fx, "Illegal < found in attribute.")
        return
        ! Then, expand <
      elseif (s_in(i)=='&') then
        j = index(str_vs(s_in(i+1:)), ';')
        if (j==0) then
          call add_parse_error(fx, "Illegal & found in attribute")
          return
        elseif (j==1) then
          call add_parse_error(fx, "No entity reference found")
          return
        elseif (checkCharacterEntityReference(str_vs(s_in(i+1:i+j-1)))) then
          ! Expand all character entities
          s_temp(i2) = expand_char_entity(str_vs(s_in(i+1:i+j-1))) ! FIXME ascii
          i = i + j  + 1
          i2 = i2 + 1 ! fixme
        elseif (checkName(str_vs(s_in(i+1:i+j-1)))) then
          ! It looks like an entity, is it a good Name?
          if (str_vs(s_in(i+1:i+j-1))=='lt') then
            s_temp(i2) = '<' 
            i = i + j + 1
            i2 = i2 + 1
          elseif (str_vs(s_in(i+1:i+j-1))=='amp') then
            s_temp(i2) = '&'
            i = i + j + 1
            i2 = i2 + j + 1
          elseif (existing_entity(e_list, str_vs(s_in(i+1:i+j-1)))) then
            !is it the right sort of entity?
            if (is_unparsed_entity(e_list, str_vs(s_in(i+1:i+j-1)))) then
              call add_parse_error(fx, "Unparsed entity forbidden in attribute")
              return
            elseif (is_external_entity(e_list, str_vs(s_in(i+1:i+j-1)))) then
              call add_parse_error(fx, "External entity forbidden in attribute")
              return
            endif
            ! shorten entity list
            shortened_entity_list = shallow_copy_entity_list_without(e_list, str_vs(s_in(i+1:i+j-1)))
            ! Recursively expand entity, checking for errors.
            s_ent => normalize_text(fx, shortened_entity_list, expand_entity_text(e_list, str_vs(s_in(i+1:i+j-1))))
            call shallow_destroy_entity_list(shortened_entity_list)
            if (fx%error) return
            allocate(s_temp2(size(s_temp)+size(s_ent)-(j+1)))
            s_temp2(:i2) = s_temp
            s_temp2(i2+1:) = s_ent
            deallocate(s_temp)
            s_temp => s_temp2
            i = i + j + 1
            i2 = i2 + size(s_ent)
            deallocate(s_ent)
          else
            s_temp(i2:i2+j) = s_in(i:i+j)
            i = i + j + 1
            i2 = i2 + j + 1
            if (fx%standalone) then
              ! or possibly otherwise? empty DTD?:
              call add_parse_error(fx, "Undeclared entity encountered in standalone document.")
              return
            endif
          endif
        else
          call add_parse_error(fx, "Illegal entity reference")
          return
        endif
      else
        s_temp(i2) = s_in(i)
        i = i + 1
        i2 = i2 + 1
      endif
    enddo
    
    allocate(s_out(i2-1))
    s_out = s_temp(:i2-1)
    deallocate(s_temp)

  end function normalize_text
    
  subroutine add_parse_error(fx, msg, severity)
    type(sax_parser_t), intent(inout) :: fx
    character(len=*), intent(in) :: msg
    integer, optional, intent(in) :: severity

    type(error_t), dimension(:), pointer :: tempStack
    integer :: i, n

    n = fx%parse_stack

    if (.not.fx%error) then
      fx%error = .true.
      allocate(fx%error_stack(0)%msg(len(msg)))
      fx%error_stack(0)%msg = vs_str(msg)
    else
      allocate(tempStack(0:n))
      
      do i = 0, n - 1
        tempStack(i)%msg => fx%error_stack(i)%msg
      enddo
      allocate(tempStack(n)%msg(len(msg)))
      tempStack(n)%msg = vs_str(msg)
      deallocate(fx%error_stack)
      fx%error_stack => tempStack
    endif

    if (present(severity)) then
      fx%error_stack(ubound(fx%error_stack))%severity = ERR_ERROR
    else
      fx%error_stack(ubound(fx%error_stack))%severity = severity
    endif

  end subroutine add_parse_error

end module m_sax_tokenizer

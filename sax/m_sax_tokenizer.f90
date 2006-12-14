! XML tokenizer

module m_sax_tokenizer

  use m_common_array_str, only: vs_str, str_vs
  use m_common_charset, only: XML_WHITESPACE, &
    XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML1_0, XML1_1, operator(.in.)

  use m_sax_reader, only: file_buffer_t, &
    read_char, read_chars, get_characters, put_characters, &
    get_next_character_discarding_whitespace, &
    get_characters_until_all_of, &
    get_characters_until_one_of, &
    len_namebuffer, retrieve_namebuffer
  use m_sax_types ! everything, really

  implicit none
  private

  public :: sax_tokenizer
  public :: parse_xml_declaration

contains

  subroutine sax_tokenizer(fx, fb, iostat)
    type(sax_parser_t), intent(in) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat

    character :: c

    if (associated(fx%token)) deallocate(fx%token)

    if (fx%discard_whitespace) then
      c = get_next_character_discarding_whitespace(fb, iostat)
      if (iostat/=0) return

    elseif (fx%long_token) then
      select case(fx%state)
      case (ST_PI_CONTENTS)
        call get_characters_until_all_of(fb, '?>', iostat)
      case (ST_COMMENT_CONTENTS)
        call get_characters_until_all_of(fb, '--', iostat)
      case (ST_CDATA_CONTENTS)
        call get_characters_until_all_of(fb, ']]>', iostat)
      case (ST_CHAR_IN_CONTENT)
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

      elseif (c.in.str_vs(fx%initialNameChars)) then
        call get_characters_until_not_one_of(fb, str_vs(fx%nameChars), iostat)
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
          elseif (c.in.str_vs(fx%initialNameChars)) then
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

        case ('%')
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) then
            !make an error happen
            return
          endif
          if (c.in.XML_WHITESPACE) then
            allocate(fx%token(1))
            fx%token = '%'
          elseif (c.in.str_vs(fx%initialNameChars)) then
            call get_characters_until_not_one_of(fb, str_vs(fx%nameChars), iostat)
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
          elseif (c.in.str_vs(fx%initialNameChars)) then
            call get_characters_until_not_one_of(fb, str_vs(fx%nameChars), iostat)
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
            call get_namebuffer(fb, fx)
            
          elseif (fx%context==CTXT_IN_CONTENT) then !.an.d some other condition) then
            call get_characters_until_one_of(fb, '"', iostat)
            if (iostat/=0) return
            call get_namebuffer(fb, fx)
            ! expand entities
            ! normalize text

          else
            ! make an error
          endif

        case ("'")
          if (fx%context==CTXT_IN_DTD) then! .and. some other condition) then
            call get_characters_until_one_of(fb, "'", iostat)
            if (iostat/=0) return
            call get_namebuffer(fb, fx)
            
          elseif (fx%context==CTXT_IN_CONTENT) then! .and. some other condition) then
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


  subroutine parse_xml_declaration(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    ! Need to do this with read_* fns, not get_*
    ! FIXME below all read_chars should be error-checked.
    integer :: i
    character(len=*), parameter :: version="version", encoding="encoding", standalone="standalone"
    character :: c, quotation_mark
    character, allocatable :: ch(:)
    c = " "
    do while (c.in.XML_WHITESPACE)
      c = read_char(fb, iostat); if (iostat/=0) return
    enddo
    call put_characters(fb, 1)
    allocate(ch(7))
    ch = vs_str(read_chars(fb, 7, iostat)); if (iostat/=0) return
    if (str_vs(ch)/="version") then
      deallocate(ch)
      call parse_error("Expecting XML version"); return
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
        call parse_error("Expecting > to end XML declaration"); return
      endif
      return
    endif
    call put_characters(fb, 1)
    allocate(ch(8))
    ch = vs_str(read_chars(fb, 8, iostat)); if (iostat/=0) return
    if (str_vs(ch)/="encoding") then
      call put_characters(fb, 8)
      deallocate(ch)
      allocate(ch(10))
      ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="standalone") then
        deallocate(ch)
        call parse_error("Expecting XML encoding or standalone declaration"); return
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
          call parse_error("Expecting > to end XML declaration"); return
        endif
        return
      endif
      call put_characters(fb, 1)
      allocate(ch(10))
      ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="standalone") then
        deallocate(ch)
        call parse_error("Expecting XML encoding or standalone declaration"); return
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
          call parse_error("Expecting > to end XML declaration"); return
        endif
      endif
    endif

    if (str_vs(fx%encoding)/="UTF-8") then
      call FoX_warning("Unknown character encoding in XML declaration. "//&
        "Assuming you know what you are doing, going ahead anyway.")
    endif

  contains

    subroutine check_version
      character :: c, quotechar
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call parse_error("Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call parse_error("Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1") then
        call parse_error("Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=".") then
        call parse_error("Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1".and.c/="0") then
        call parse_error("Unknown XML version"); return
      endif
      if (c=="1") then
        fx%xml_version = XML1_1
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call parse_error("Expecting "//quotechar); return
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
        call parse_error("Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call parse_error("Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (.not.(c.in.XML_INITIALENCODINGCHARS)) then
        call parse_error("Illegal character at start of encoding declaration."); return
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
        i = i+1
        allocate(tempbuf(i))
        tempbuf(:i-1) = buf
        deallocate(buf)
        tempbuf(i) = c
        buf => tempbuf
        c = read_char(fb, iostat)
        if (iostat/=0) then
          deallocate(buf)
          return
        endif
      enddo
      if (c/=quotechar) then
        call parse_error("Illegal character in XML encoding declaration; expecting "//quotechar); return
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
        call parse_error("Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call parse_error("Expecting "" or '"); return
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
            call parse_error("standalone accepts only 'yes' or 'no'"); return
          endif
        else
          call parse_error("standalone accepts only 'yes' or 'no'"); return
        endif
      elseif (c=="n") then
        c = read_char(fb, iostat); if (iostat/=0) return
        if (c=="o") then
          fx%standalone = .false.
        else
          call parse_error("standalone accepts only 'yes' or 'no'"); return
        endif
      else
        call parse_error("standalone accepts only 'yes' or 'no'"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call parse_error("Expecting "" or '"); return
      endif
    end subroutine check_standalone

  end subroutine parse_xml_declaration

end module m_sax_tokenizer

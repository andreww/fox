! XML parser

module m_sax_parser

  use m_common_array_str, only: str_vs, vs_str
  use m_common_charset, only: XML1_0, XML1_1, XML1_0_INITIALNAMECHARS, &
    XML1_1_INITIALNAMECHARS, XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML_WHITESPACE, XML1_0_NAMECHARS, XML1_1_NAMECHARS, operator(.in.)
  use m_common_error, only: FoX_error
  use m_common_io, only: io_eof
  use m_common_namecheck, only: checkName

  use m_sax_reader, only: file_buffer_t, read_char, read_chars
  use m_sax_tokenizer, only: sax_tokenize, parse_xml_declaration, add_parse_error
  use m_sax_types ! everything, really

  implicit none
  private

  public :: sax_parser_init
  public :: sax_parser_destroy
  public :: sax_parse

contains

  subroutine sax_parser_init(fx)
    type(sax_parser_t), intent(out) :: fx
    
    allocate(fx%token(0))
    allocate(fx%error_stack(0:0))
    allocate(fx%error_stack(0)%msg(0))
    
  end subroutine sax_parser_init

  subroutine sax_parser_destroy(fx)
    type(sax_parser_t), intent(inout) :: fx
 
    integer :: i

    fx%context = CTXT_NULL
    fx%state = ST_NULL

    deallocate(fx%encoding)
    deallocate(fx%token)
    do i = 0, ubound(fx%error_stack,1)
      deallocate(fx%error_stack(i)%msg)
    enddo
    deallocate(fx%error_stack)

  end subroutine sax_parser_destroy

  recursive subroutine sax_parse(fx, fb, iostat, sax_error_callback)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    external sax_error_callback
    optional sax_error_callback

    iostat = 0
    
    if (fx%parse_stack==0) then
      call parse_xml_declaration(fx, fb, iostat)
      if (iostat/=0) return

      fx%context = CTXT_BEFORE_DTD
      fx%state = ST_MISC
      fx%discard_whitespace = .true.
    endif

    do

      call sax_tokenize(fx, fb, iostat)
      if (iostat/=0) goto 100

      select case (fx%context)

      case (ST_MISC)
        if (str_vs(fx%token) == '<?') then
          fx%state = ST_START_PI
        elseif (str_vs(fx%token) ==  '<!') then
          fx%state = ST_BANG_TAG
        elseif (str_vs(fx%token) == '<') then
          fx%context = CTXT_IN_CONTENT
          fx%state = ST_START_TAG
        else
          ! make error
        endif

      case (ST_BANG_TAG)
        if (str_vs(fx%token) == '--') then
          fx%state = ST_START_COMMENT
          fx%discard_whitespace = .false.
        elseif (str_vs(fx%token) == 'DOCTYPE') then
          ! go to DTD parser
        elseif (str_vs(fx%token) == '[') then
          if (fx%context == CTXT_IN_CONTENT) then
            fx%state = ST_START_CDATA_1
          else
            ! make an error
            continue
          endif
        else
          ! make an error
          continue
        endif

      case (ST_START_PI)
        !token should be an XML Name
        if (.true.) then
          fx%discard_whitespace = .false.
          fx%state = ST_PI_NAME
        else
          ! make error
          continue
        endif

      case (ST_PI_NAME)
        if (fx%token(1).in.XML_WHITESPACE) then
          fx%state = ST_PI_CONTENTS
        else
          ! make error
          continue
        endif

      case (ST_PI_CONTENTS)
        ! fx%token - longer than 1 char?
        if (size(fx%token)>1) then
          ! put it somewhere
          fx%state = ST_PI_END
        else
          ! make an error
          continue
        endif

      case (ST_PI_END)
        if (str_vs(fx%token)=='?>') then
          if (fx%context==CTXT_IN_CONTENT) then
            fx%state = ST_CHAR_IN_CONTENT
          else
            fx%state = ST_MISC
            fx%discard_whitespace = .true.
          endif
        else
          ! make an error
        endif

      case (ST_START_COMMENT)
        ! token should be comment contents.
        fx%state = ST_COMMENT_CONTENTS

      case (ST_COMMENT_CONTENTS)
        if (str_vs(fx%token)=='--') then
          fx%state = ST_COMMENT_END
        else
          ! make an error (internal error?)
          continue
        endif

      case (ST_COMMENT_END)
        if (str_vs(fx%token)=='>') then
          if (fx%context==CTXT_IN_CONTENT) then
            fx%state = ST_CHAR_IN_CONTENT
          else
            fx%state = ST_MISC
            fx%discard_whitespace = .true.
          endif
        else
          ! make an error
          continue
        endif

      case (ST_START_TAG)
        if (fx%context == CTXT_BEFORE_DTD &
          .or. fx%context == CTXT_BEFORE_CONTENT) then
          ! root element
          ! check token is name
          fx%discard_whitespace = .true.
          fx%state = ST_IN_TAG
        elseif (fx%context == CTXT_IN_CONTENT) then
          ! normal element
          ! check token is name
          fx%discard_whitespace = .true.
        elseif (fx%context == CTXT_AFTER_CONTENT) then
          ! make an error
          continue
        elseif (fx%context == CTXT_IN_DTD) then
          ! make an error
          continue
        endif

      case (ST_START_CDATA_1)
        if (str_vs(fx%token) == 'CDATA') then
          fx%state = ST_START_CDATA_2
        else
          ! make an error
          continue
        endif

      case (ST_START_CDATA_2)
        if (str_vs(fx%token) == '[') then
          fx%state = ST_CDATA_CONTENTS
        else
          ! make an error
          continue
        endif

      case (ST_IN_TAG)
        if (str_vs(fx%token)=='>') then
          ! push tag onto stack
          fx%state = ST_CHAR_IN_CONTENT
          fx%discard_whitespace = .false.
        elseif (str_vs(fx%token)=='/>') then
          ! open & close
          fx%state = ST_CHAR_IN_CONTENT
          fx%discard_whitespace = .false.
          !token should be xmlname
          fx%state = ST_ATT_NAME
        else
          !make an error
          continue
        endif

      case (ST_ATT_NAME)
        if (str_vs(fx%token)=='=') then
          fx%state = ST_ATT_EQUALS
        else
          ! make an error
          continue
        endif

      case (ST_ATT_EQUALS)
        if (fx%token(1)=='"'.or.fx%token(1)=="'") then
          ! token (2:end-1) is att value
          fx%state = ST_IN_TAG
        else
          ! make an error
          continue
        endif

      case (ST_CHAR_IN_CONTENT)
        if (str_vs(fx%token)=='<') then
          fx%state = ST_START_TAG
          fx%discard_whitespace = .true.
        elseif (str_vs(fx%token)=='<!') then
          fx%state = ST_BANG_TAG
        elseif (str_vs(fx%token)=='<?') then
          fx%state = ST_START_PI
        elseif (str_vs(fx%token)=='</') then
          fx%state = ST_CLOSING_TAG
        elseif (fx%token(1)=='&') then
          ! tell tokenizer to expand it
          call sax_parse(fx, fb, iostat)
          if (iostat/=0) goto 100
        else
          ! make an error
          continue
        endif
        ! entire token is character data

      case (ST_CLOSING_TAG)
        if (checkName(str_vs(fx%token))) then!fx%token, fx%xml_version)) then
          ! ok
          fx%discard_whitespace = .true.
          fx%state = ST_IN_TAG !is it FIXME
        else
          ! make an error
          continue
        endif

      end select

100   if (iostat==io_eof) then ! error is end of file then
        if (fx%parse_stack>0) then !we are parsing an entity
          if (fx%well_formed) then
            iostat = 0
            ! go back up stack
          else !badly formed entity
            call add_parse_error(fx, "Badly formed entity.")
            return
          endif
        else ! EOF of main file
          if (fx%well_formed) then
            continue
            ! finish
          else
            call add_parse_error(fx, "File is not well-formed")
            call sax_error(fx, sax_error_callback)
          endif
        endif
      else ! Hard error - stop immediately
        if (fx%parse_stack>0) then !we are parsing an entity
          call add_parse_error(fx, "Internal error: Error encountered processing entity.")
        else
          call sax_error(fx, sax_error_callback)
        endif
      endif

    end do

  end subroutine sax_parse

  subroutine sax_error(fx, sax_error_callback)
    type(sax_parser_t), intent(inout) :: fx
    external sax_error_callback
    optional sax_error_callback

    character, dimension(:), pointer :: errmsg

    integer :: i, m, n
    n = 0 
    
    do i = 0, fx%parse_stack
      n = n + size(fx%error_stack(i)%msg) ! + spaces + size of entityref
    enddo
    allocate(errmsg(n))
    n = 1
    do i = 0, fx%parse_stack
      m = size(fx%error_stack(i)%msg)
      errmsg(n:n+m-1) = fx%error_stack(i)%msg
      n = n + m 
    enddo
    if (present(sax_error_callback)) then
      call sax_error_callback(str_vs(errmsg))
    else
      call FoX_error(str_vs(errmsg))
    endif

  end subroutine sax_error
      
end module m_sax_parser


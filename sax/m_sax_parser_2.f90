! XML parser

module m_sax_parser

  use m_common_array_str, only: str_vs, vs_str
  use m_common_charset, only: XML1_0, XML1_1, XML1_0_INITIALNAMECHARS, &
    XML1_1_INITIALNAMECHARS, XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML_WHITESPACE, XML1_0_NAMECHARS, XML1_1_NAMECHARS, operator(.in.)
  use m_common_io, only: io_eof
  use m_common_namecheck, only: checkName

  use m_sax_reader, only: file_buffer_t
  use m_sax_types, only: sax_error_t, sax_parser_t

  implicit none
  private

  ! Context

  integer, parameter :: CTXT_NULL = -1
  integer, parameter :: CTXT_INIT = 0
  integer, parameter :: CTXT_BEFORE_DTD = 1
  integer, parameter :: CTXT_IN_DTD = 2
  integer, parameter :: CTXT_BEFORE_CONTENT = 3
  integer, parameter :: CTXT_IN_CONTENT = 4
  integer, parameter :: CTXT_AFTER_CONTENT = 5

  ! State

  integer, parameter :: ST_NULL = -1
  integer, parameter :: ST_MISC = 0
  integer, parameter :: ST_BANG_TAG = 1 
  integer, parameter :: ST_START_PI = 2
  integer, parameter :: ST_START_COMMENT = 3
  integer, parameter :: ST_START_TAG = 4 
  integer, parameter :: ST_START_CDATA_1 = 5
  integer, parameter :: ST_START_CDATA_2 = 6
  integer, parameter :: ST_IN_TAG = 7
  integer, parameter :: ST_ATT_NAME = 8
  integer, parameter :: ST_ATT_EQUALS = 9
  integer, parameter :: ST_CHAR_IN_CONTENT = 10
  integer, parameter :: ST_CLOSING_TAG = 11
  integer, parameter :: ST_PI_NAME = 12
  integer, parameter :: ST_PI_END = 13
  integer, parameter :: ST_COMMENT_CONTENTS = 14
  integer, parameter :: ST_COMMENT_END = 15
  integer, parameter :: ST_PI_CONTENTS = 16

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

    deallocate(fx%token)
    do i = 0, ubound(fx%error_stack,1)
      deallocate(fx%error_stack(i)%msg)
      enddo
    deallocate(fx%error_stack)

  end subroutine sax_parser_destroy

  recursive subroutine sax_parse(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat

    iostat = 0

    !read xml declaration

    fx%context = CTXT_BEFORE_DTD
    fx%state = ST_MISC
    fx%discard_whitespace = .true.

    do

      call sax_tokenizer(fx, fb, iostat)
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
          fx%state = ST_START_CDATA_2
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
            call make_an_error
            !        push an error onto stack
            return
          endif
        else ! EOF of main file
          if (fx%well_formed) then
            continue
            ! finish
          else
            call make_an_error
            !        push an error onto stack
          endif
        endif
      else ! Hard error - stop immediately
        if (fx%parse_stack>0) then !we are parsing an entity
          call make_an_error
          !        push an error onto stack
        else
          call sax_error_callback
        endif
      endif

    end do

  end subroutine sax_parse

  subroutine add_parse_error(fx, msg)
    type(sax_parser_t), intent(inout) :: fx
    character(len=*), intent(in) :: msg
    
    type(sax_error_t), dimension(:), pointer :: tempStack
    integer :: i, n
    
    n = size(fx%error_stack)
    allocate(tempStack(0:n+1))
    
    do i = 0, n
      tempStack(i)%msg => fx%error_stack(i)%msg
    enddo
    tempStack(n+1)%msg = vs_str(msg)
    deallocate(fx%error_stack)
    fx%error_stack => tempStack
  end subroutine add_parse_error
  

end module m_sax_parser


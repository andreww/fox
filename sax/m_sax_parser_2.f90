! XML parser

module m_sax_parser

! Context

integer, parameter :: CTXT_NULL = -1
integer, parameter :: CTXT_INIT = 0
integer, parameter :: CTXT_BEFORE_DTD = 1
integer, parameter :: CTXT_IN_DTD = 2
integer, parameter :: CTXT_BEFORE_CONTENT = 3
integer, parameter :: CTXT_IN_CONTENT = 4
integer, parameter :: CTXT_AFTER_CONTENT = 5

! State

integer, parameter :: ST_START_PI
integer, parameter :: ST_PI_NAME
integer, parameter :: ST_PI_CONTENT
integer, parameter :: ST_START_COMMENT
integer, parameter :: ST_COMMENT_CONTENTS
integer, parameter :: ST_START_TAG
integer, parameter :: ST_TAG_NAME
integer, parameter :: ST_A



  type sax_parser_t
    
  end type sax_parser_t

contains

  subroutine sax_parser_init()
  end subroutine sax_parser_init

  subroutine sax_parser_destroy()

  end subroutine sax_parser_destroy

  subroutine sax_parse()

    do

      call sax_tokenizer(fx, fb, signal)

      select case (fx%context)
        
      case (CTXT_INIT)
        read xml declaration

      case (CTXT_BEFORE_DTD)

        select case(fx%state)
        case(ST_MISC)
          if (str_vs(fx%token) == '<!DOCTYPE') then
            fx%context = CTXT_IN_DTD
            fx%state = 
          elseif (str_vs(fx%token) == '<?') then
            fx%state = ST_START_PI
          elseif (strvs(fx%token) ==  '<!--') then
            fx%state = ST_START_COMMENT
          elseif (str_vs(fx%token) == '<') then
            fx%context = CTXT_IN_CONTENT
            fx%state = ST_START_TAG
          else
            ! make error
          endif
        case (ST_START_PI)
          ! should be a Name
          ! put somewhere
          fx%state = ST_PI_CONTENT
        case (ST_PI_CONTENT)
          ! report PI
          fx%state = ST_MISC
        case (ST_START_COMMENT)

        end select

      case (ST_MISC)
        if (str_vs(fx%token) == '<!DOCTYPE') then
          if (fx%context == CTXT_BEFORE_DTD) then
            fx%context = CTXT_IN_DTD
            fx%state = 
          else
            ! make an error
          endif
        elseif (str_vs(fx%token) == '<?') then
          fx%state = ST_START_PI
        elseif (strvs(fx%token) ==  '<!--') then
          fx%state = ST_START_COMMENT
        elseif (str_vs(fx%token) == '<') then
          fx%context = CTXT_IN_CONTENT
          fx%state = ST_START_TAG
        else
          ! make error
        endif
        
      case (ST_START_PI)
        !token should be an XML Name
        else
          ! make error
        endif

      case (ST_START_COMMENT)
        ! token should be comment contents.
        if (fx%context == CTXT_IN_CONTENT) then
          fx%state = ST_CONTENT
        else
          fx%state = ST_MISC
        endif

      case (ST_START_TAG)
        if (fx%context == CTXT_BEFORE_DTD &
          .or. fx%context == CTXT_BEFORE_CONTENT) then
          ! root element
          ! get next token.
        elseif (fx%context == CTXT_IN_CONTENT)
          ! normal element
        elseif (fx%context == CTXT_AFTER_CONTENT)
          ! make an error
        elseif (fx%context == CTXT_IN_DTD)
          ! make an error
        endif

      case (
      end select

  end subroutine sax_parse

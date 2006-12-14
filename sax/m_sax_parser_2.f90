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

    !read xml declaration

    fx%context = CTXT_BEFORE_DTD
    fx%state = ST_MISC
    fx%discard_whitespace = .true.
    
    do

      call sax_tokenizer(fx, fb, signal)

      select case (fx%context)
        
      case (ST_MISC)
        if (str_vs(fx%token) == '<?') then
          fx%state = ST_START_PI
        elseif (strvs(fx%token) ==  '<!') then
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
        elseif (str_vs(fx%token) == 'DOCTYPE') then
          ! go to DTD parser
        elseif (str_vs(fx%token) == '[') then
          if (fx%context == CTXT_CONTENT) then
            fx%state = ST_START_CDATA_1
          elseif
            ! make an error
          endif
        else
          ! make an error
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
          fx%discard_whitespace = .true.
          fx%state = ST_IN_TAG
        elseif (fx%context == CTXT_IN_CONTENT)
          ! normal element
          fx%discard_whitespace = .true.
        elseif (fx%context == CTXT_AFTER_CONTENT)
          ! make an error
        elseif (fx%context == CTXT_IN_DTD)
          ! make an error
        endif

      case (ST_START_CDATA_1)
        if (str_vs(fx%token) == 'CDATA') then
          fx%state = ST_START_CDATA_2
        else
          ! make an error
        endif

      case (ST_START_CDATA_2)
        if (str_vs(fx%token) == '[') then
          fx%state = ST_START_CDATA_2
        else
          ! make an error
        endif

      case (ST_IN_TAG)
        if (token=='>') then
          ! push tag onto stack
          fx%state = ST_CHAR_IN_CONTENT
        elseif (token=='/>') then
          ! open & close
          fx%state = ST_CHAR_IN_CONTENT
        !token should be xmlname
          fx%state = ST_ATT_NAME
        else
          !make an error
        endif

      case (ST_ATT_NAME)
        if (fx%token=='=') then
          fx%state = ST_ATT_EQUALS
        else
          ! make an error
        endif

      case (ST_ATT_EQUALS)
        if (fx%token(1)=='"'.or.fx%token(1)=="'") then
          ! token (2:end-1) is att value
          fx%state = ST_IN_TAG
        else
          ! make an error
        endif
        
        case (ST_CHAR_IN_CONTENT)
          if (fx%token=='<') then
            fx%state = ST_STARTING_TAG
            fx%discard_whitespace = .true.
          elseif (fx%token=='<!') then
            fx%state = ST_BANG_TAG
          elseif (fx%token=='<?') then
            fx%state = 
          ! entire token is character data
          
        
      end select

  end subroutine sax_parse

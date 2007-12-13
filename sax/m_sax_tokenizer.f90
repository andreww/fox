module m_sax_tokenizer

  use m_common_array_str, only: vs_str, str_vs, vs_str_alloc
  use m_common_charset, only: XML_WHITESPACE, &
    upperCase, digits, hexdigits, isInitialNameChar, &
    isXML1_0_NameChar, isXML1_1_NameChar, XML1_0, XML1_1
  use m_common_error, only: add_error, in_error
  use m_common_entities, only: existing_entity, &
    is_unparsed_entity, is_external_entity, expand_entity_text, &
    expand_char_entity, add_internal_entity, pop_entity_list
  use m_common_io, only: io_eof
  use m_common_namecheck, only: checkName, checkCharacterEntityReference

  use m_sax_reader, only: file_buffer_t, &
    push_chars, get_characters, &
    get_characters_until_all_of, &
    get_characters_until_one_of, &
    get_characters_until_not_one_of, &
    get_characters_until_not_namechar
  use m_sax_types ! everything, really

  implicit none
  private

  integer, parameter :: TOK_NULL = 0
  integer, parameter :: TOK_PI_TAG = 1 ! <?
  integer, parameter :: TOK_BANG_TAG = 2 ! <!
  integer, parameter :: TOK_OPEN_TAG = 3 ! <
  integer, parameter :: TOK_OPEN_SB = 4 ! [
  integer, parameter :: TOK_CLOSE_SB = 5 ! [
  integer, parameter :: TOK_OPEN_COMMENT = 6 ! --
  integer, parameter :: TOK_NAME = 7 ! name (+token)
  integer, parameter :: TOK_CHAR = 8 ! character data (+token)
  integer, parameter :: TOK_PI_END = 9 ! ?>
  integer, parameter :: TOK_COMMENT_END = 10 ! -->
  integer, parameter :: TOK_START_CDATA = 11 ! CDATA[
  integer, parameter :: TOK_CDATA_END = 12 ! ]]>
  integer, parameter :: TOK_END_TAG = 13 ! >
  integer, parameter :: TOK_END_TAG_CLOSE = 14 ! />
  integer, parameter :: TOK_CLOSE_TAG = 15 ! </
  integer, parameter :: TOK_ENTITY = 16 ! % or &
  integer, parameter :: TOK_EQUALS = 17 ! =
  integer, parameter :: TOK_DTD_CONTENTS = 18 ! for element and attlist

  public :: sax_tokenize
  public :: normalize_text

contains

  subroutine sax_tokenize2(fx, fb, eof)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    logical, intent(out) :: eof

    character :: c, q
    integer :: xv, phrase, iostat
    logical :: firstChar, ws_discard

    xv = fx%xds%xml_version

    eof = .false.
    fx%tokentype = TOK_NULL
    if (associated(fx%token)) deallocate(fx%token)
    fx%token => vs_str_alloc("")
    !FIXME next token
    if (associated(fx%next_token)) then
      fx%token => fx%next_token
      nullify(fx%next_token)
      return
    endif

    ! This would all be SO much easier if there were a regular-expression
    ! library available. As it is, we essentially do hand-written regex
    ! equivalents for each state ...

    q = " "
    phrase = 0
    firstChar = .true.
    do
      c = get_characters(fb, 1, iostat, fx%error_stack)
      if (iostat==io_eof) then
        eof = .true.
        return
      elseif (in_error(fx%error_stack)) then
        return
      endif

      select case (fx%state)
      case (ST_MISC)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c, XML_WHITESPACE)>0) then
            if (c=="<") then
              ws_discard = .false.
            else
              call add_error(fx%error_stack, "Unexpected character found outside content")
            endif
          endif
        else
          if (c=="?") then
            fx%tokenType = TOK_PI_TAG
          elseif (c=="!") then
            fx%tokenType = TOK_BANG_TAG
          elseif (isInitialNameChar(c, xv)) then
            call push_chars(fb, c)
            fx%tokenType = TOK_OPEN_TAG
          else
            call add_error(fx%error_stack, "Unexpected character after <")
          endif
        endif

      case (ST_BANG_TAG)
        if (firstChar) then
          if (c=="-") then
            phrase = 1
          elseif (c=="[") then
            fx%tokenType = TOK_OPEN_SB
          elseif (verify(c,upperCase)==0) then
            fx%token => vs_str_alloc(c)
          else
            call add_error(fx%error_stack, "Unexpected character after <!")
          endif
        elseif (phrase==1) then
          if (c=="-") then
            fx%tokenType = TOK_OPEN_COMMENT
          else
            call add_error(fx%error_stack, "Unexpected character after <!-")
          endif
        elseif (verify(c,XML_WHITESPACE)>0) then
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        else
          fx%tokenType = TOK_NAME
        endif

      case (ST_START_PI)
        ! grab until whitespace or ?
        if (verify(c, XML_WHITESPACE//"?")>0) then
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        else
          fx%tokenType = TOK_NAME
          if (c=="?") call push_chars(fb, c)
        endif

      case (ST_PI_CONTENTS)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c, XML_WHITESPACE)>0) then
            fx%token => vs_str_alloc(c)
            ws_discard = .false.
          endif
        elseif (phrase==1) then
          if (c==">") then
            phrase = 2
            fx%tokenType = TOK_CHAR
            fx%nextTokenType = TOK_PI_END
          elseif (c=="?") then
            fx%token => vs_str_alloc(str_vs(fx%token)//"?")
          else
            phrase = 0
            fx%token => vs_str_alloc(str_vs(fx%token)//"?"//c)
          endif
        elseif (c=="?") then
          phrase = 1
        else
          fx%token => vs_str_alloc(str_vs(fx%token)//"?"//c)
        endif

      case (ST_START_COMMENT)
        select case(phrase)
        case (0)
          if (c=="-") then
            phrase = 1
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
          endif
        case (1)
          if (c=="-") then
            phrase = 2
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//"-"//c)
            phrase = 0
          endif
        case (2)
          if (c==">") then
            fx%tokenType = TOK_CHAR
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
            fx%nextTokenType = TOK_COMMENT_END
            exit
          else
            call add_error(fx%error_stack, &
              "Expecting > after -- inside a comment.")
          endif
        end select

      case (ST_START_TAG)
        ! grab until whitespace or /, >
        if (verify(c, XML_WHITESPACE//"/>")>0) then
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        else
          fx%tokenType = TOK_NAME
          if (verify(c, "/>")>0) call push_chars(fb, c)
        endif

      case (ST_START_CDATA_1)
        ! grab until whitespace
        if (firstChar) fx%token => vs_str_alloc("")
        if (verify(c, XML_WHITESPACE//"[")>0) then
          if (size(fx%token)>5) then
            call add_error(fx%error_stack, &
              "Expecting CDATA[ after <![")
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
          elseif (str_vs(fx%token)=="CDATA") then
            if (c=="[") then
              fx%tokenType = TOK_START_CDATA
              deallocate(fx%token)
            else
              call add_error(fx%error_stack, &
                "Expecting [ after CDATA")
            endif
          else
            call add_error(fx%error_stack, &
              "Expecting CDATA[ after <![")
          endif
        endif

      case (ST_CDATA_CONTENTS)
        select case(phrase)
        case (0)
          if (c=="]") then
            phrase = 1
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
          endif
        case (1)
          if (c=="]") then
            phrase = 2
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
            phrase = 0
          endif
        case (2)
          if (c==">") then
            fx%tokenType = TOK_CHAR
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
            fx%nextTokenType = TOK_CDATA_END
            exit
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
            phrase = 0
          endif
        end select

      case (ST_IN_TAG)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c,XML_WHITESPACE)>0) then
            if (c==">") then
              fx%tokenType = TOK_END_TAG
            elseif (c=="/") then
              phrase = 1
              ws_discard = .false.
            elseif ((xv==XML1_0.and.isXML1_0_NameChar(c)) &
              .or.(xv==XML1_1.and.isXML1_1_NameChar(c))) then
              fx%token => vs_str_alloc(c)
              ws_discard = .false.
            else
              call add_error(fx%error_stack, &
                "Unexpected character in element tag")
            endif
          else
            if (phrase==1) then
              if (c==">") then
                fx%tokenType = TOK_CLOSE_TAG
              else
                call add_error(fx%error_stack, &
                  "Unexpected character after / in element tag")
                exit
              endif
            else
              if ((xv==XML1_0.and.isXML1_0_NameChar(c)) &
                .or.(xv==XML1_1.and.isXML1_1_NameChar(c))) then
                fx%token => vs_str_alloc(str_vs(fx%token)//c)
              elseif (verify(c,XML_WHITESPACE)==0) then
                fx%tokenType = TOK_NAME
              elseif (c=="=") then
                fx%tokenType = TOK_NAME
                fx%nextTokenType = TOK_EQUALS
              else
                call add_error(fx%error_stack, &
                  "Unexpected character in element tag")
                exit
              endif
            endif
          endif
        endif

      case (ST_ATT_NAME)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c,XML_WHITESPACE)>0) then
            if (c=="=") then
              fx%tokenType = TOK_EQUALS
            else
              call add_error(fx%error_stack, &
                "Unexpected character in element tag, expected =")
            endif
          endif
        endif

      case (ST_ATT_EQUALS)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c,XML_WHITESPACE)>0) then
            if (verify(c,"'""")>0) then
              call add_error(fx%error_stack, "Expecting "" or '")
            endif
          else
            q = c
            ws_discard = .false.
          endif
        else
          if (c==q) then
            fx%tokenType = TOK_CHAR
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
          endif
        endif

      case (ST_CHAR_IN_CONTENT)
        if (verify(c, "<&")>0) then
          if (phrase==1) then
            fx%token => vs_str_alloc(str_vs(fx%token)//"]")
          elseif (phrase==2) then
            fx%token => vs_str_alloc(str_vs(fx%token)//"]]")
          endif
          call push_chars(fb, c)
          fx%tokenType = TOK_CHAR
        elseif (c=="]") then
          if (phrase==0) then
            phrase = 1
          elseif (phrase==1) then
            phrase = 2
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//"]]]")
          endif
        elseif (c==">") then
          if (phrase==2) then
            call add_error(fx%error_stack, "]]> forbidden in character context")
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//">")
          endif
        else
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        endif

      case (ST_TAG_IN_CONTENT)
        if (phrase==0) then
          if (c=="<") then
            phrase = 1
            ws_discard = .false.
          elseif (c=="&") then
            fx%tokenType = TOK_ENTITY
          else
            call add_error(fx%error_stack, "Unexpected character found outside content")
          endif
        elseif (phrase==1) then
          if (c=="?") then
            fx%tokenType = TOK_PI_TAG
          elseif (c=="!") then
            fx%tokenType = TOK_BANG_TAG
          elseif (c=="/") then
            fx%tokenType = TOK_CLOSE_TAG
          elseif (isInitialNameChar(c, xv)) then
            call push_chars(fb, c)
            fx%tokenType = TOK_OPEN_TAG
          else
            call add_error(fx%error_stack, "Unexpected character after <")
          endif
        endif

      case (ST_START_ENTITY, ST_START_PE)
        if (verify(c,XML_WHITESPACE//";")>0) then
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        elseif (c==";") then
          fx%tokenType = TOK_NAME
        else
          call add_error(fx%error_stack, "Entity reference must be terminated with a ;")
        endif

      case (ST_CLOSING_TAG)
        if (verify(c,XML_WHITESPACE//">")>0) then
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        else
          fx%tokenType = TOK_NAME
          if (c==">") fx%nextTokenType = TOK_END_TAG
        endif

      case (ST_IN_CLOSING_TAG)
        if (verify(c, XML_WHITESPACE)>0) then
          if (c==">") then
            fx%nextTokenType = TOK_END_TAG
          else
            call add_error(fx%error_stack, "Unexpected character - expecting >")
          endif
        endif

      case (ST_IN_DTD)
        if (verify(c,XML_WHITESPACE//"[>")>0) then
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        else
          fx%tokenType = TOK_NAME
          if (c=="[") then
            fx%nextTokenType = TOK_OPEN_SB
          elseif (c==">") then
            fx%nextTokenType = TOK_END_TAG
          endif
        endif

      case (ST_DTD_NAME, ST_DTD_DECL, ST_INT_SUBSET)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c, XML_WHITESPACE)>0) then
            if (c=="[") then
              fx%tokenType = TOK_OPEN_SB
            elseif (c=="]") then
              fx%tokenType = TOK_CLOSE_SB
            elseif (c==">") then
              fx%tokenType = TOK_END_TAG
            elseif (c=="%") then
              fx%tokenType = TOK_ENTITY
            elseif (c=="<") then
              phrase = 1
              ws_discard = .false.
            else
              fx%token => vs_str_alloc(c)
              ws_discard = .false.
            endif
          elseif (phrase==1) then
            if (c=="?") then
              fx%tokenType = TOK_PI_TAG
            elseif (c=="!") then
              fx%tokenType = TOK_BANG_TAG
            else
              call add_error(fx%error_stack, "Unexpected character, expecting ! or ?")
            endif
          else
            if (verify(c, XML_WHITESPACE)>0) then
              fx%token => vs_str_alloc(str_vs(fx%token)//c)
            elseif (str_vs(fx%token)=="SYSTEM") then
              fx%tokenType = TOK_NAME
            elseif (str_vs(fx%token)=="PUBLIC") then
              fx%tokenType = TOK_NAME
            else
              call add_error(fx%error_stack, "Unexpected token - expecting SYSTEM or PUBLIC")
            endif
          endif
        endif

      case (ST_DTD_PUBLIC, ST_DTD_SYSTEM)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c, XML_WHITESPACE)>0) then
            if (verify(c, "'""")==0) then
              q = c
              fx%token => vs_str_alloc("")
              ws_discard = .false.
            endif
          else
            call add_error(fx%error_stack, "Unexpected character - expecting ' or """)
          endif
        else
          if (c==q) then
            fx%tokenType = TOK_CHAR
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
          endif
        endif
        
      case (ST_DTD_ATTLIST, ST_DTD_ELEMENT, ST_DTD_ENTITY, &
        ST_DTD_ENTITY_PE, ST_DTD_NOTATION)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c,XML_WHITESPACE)>0) then
            fx%token => vs_str_alloc(c)
            ws_discard = .false.
          endif
        elseif (verify(c,XML_WHITESPACE)>0) then
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        else
          if (str_vs(fx%token)=="%") then
            fx%tokenType = TOK_ENTITY
          else
            fx%tokenType = TOK_NAME
          endif
        endif
        
      case (ST_DTD_ATTLIST_CONTENTS, ST_DTD_ELEMENT_CONTENTS)
        if (c==">") then
          fx%tokenType = TOK_DTD_CONTENTS
          fx%nextTokenType = TOK_END_TAG
        else
          fx%token => vs_str_alloc(str_vs(fx%token)//c)
        endif
        
      case (ST_DTD_ENTITY_ID, ST_DTD_ENTITY_PUBLIC, ST_DTD_ENTITY_SYSTEM, &
        ST_DTD_ENTITY_NDATA, ST_DTD_NOTATION_ID, ST_DTD_NOTATION_SYSTEM, &
        ST_DTD_NOTATION_PUBLIC, ST_DTD_NOTATION_PUBLIC_2)
        if (firstChar) ws_discard = .true.
        if (ws_discard) then
          if (verify(c, XML_WHITESPACE)>0) then
            if (verify(c, "'""")==0) then
              q = c
              fx%token => vs_str_alloc("")
              ws_discard = .false.
            elseif (c==">") then
              fx%tokenType = TOK_END_TAG
            else
              fx%token => vs_str_alloc(c)
            endif
          endif
        else
          if (q/=" ".and.c==q) then
            fx%tokenType = TOK_CHAR
          elseif (verify(c, XML_WHITESPACE)==0) then
            fx%tokenType = TOK_NAME
          else
            fx%token => vs_str_alloc(str_vs(fx%token)//c)
          endif
        endif

      end select
      
      firstChar = .false.
      if (fx%tokenType/=TOK_NULL) exit
    enddo

  end subroutine sax_tokenize2

  subroutine sax_tokenize(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat

    character :: c, c2


    ! FIXME when we return because iostat/=0, do we leak memory?

    if (associated(fx%token)) deallocate(fx%token)
    nullify(fx%token)
    if (associated(fx%next_token)) then
      iostat = 0
      fx%token => fx%next_token
      nullify(fx%next_token)
      return
    endif

    if (fx%state==ST_START_PI) then
      c = get_characters(fb, 1, iostat, fx%error_stack)
      if (iostat/=0) return
      if (.not.isInitialNameChar(c, fx%xds%xml_version)) then
        call add_error(fx%error_stack, &
          'Invalid PI Name')
        return
      endif
      call get_characters_until_not_namechar(fb, fx%xds%xml_version, iostat, fx%error_stack)
      if (iostat/=0) return
      fx%token => vs_str_alloc(c//str_vs(fb%namebuffer))
      deallocate(fb%namebuffer)

    elseif (fx%state==ST_PI_CONTENTS) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat, fx%error_stack)
      if (iostat/=0) return
      if (size(fb%namebuffer)==0) then
        !token had better be '?>'
        deallocate(fb%namebuffer)
        fx%token => vs_str_alloc(get_characters(fb, 2, iostat, fx%error_stack))
        if (iostat/=0) return
        if (str_vs(fx%token)/='?>') then
          call add_error(fx%error_stack, "Unexpected token in PI")
          return
        endif
      else
        ! Otherwise token is all chars after whitespace until '?>'
        deallocate(fb%namebuffer)
        call get_characters_until_all_of(fb, '?>', iostat, fx%error_stack)
        if (iostat/=0) return
        fx%next_token => vs_str_alloc(get_characters(fb, 2, iostat, fx%error_stack))
        if (iostat/=0) return
        fx%token => fb%namebuffer
        nullify(fb%namebuffer)
      endif

    elseif (fx%state==ST_BANG_TAG) then
      c = get_characters(fb, 1, iostat, fx%error_stack)
      if (iostat/=0) return
      if (c=='-') then
        c = get_characters(fb, 1, iostat, fx%error_stack)
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
      elseif (verify(c,upperCase)==0) then
        call get_characters_until_not_one_of(fb, upperCase, iostat, fx%error_stack)
        if (iostat/=0) return
        fx%token => vs_str_alloc(c//str_vs(fb%namebuffer))
        deallocate(fb%namebuffer)
      else
        call add_error(fx%error_stack, &
          'Unexpected token after <!')
        return
      endif

    elseif (fx%state==ST_START_COMMENT) then
      call get_characters_until_all_of(fb, '--', iostat, fx%error_stack)
      if (iostat/=0) return
      fx%next_token => vs_str_alloc(get_characters(fb, 2, iostat, fx%error_stack))
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)

    elseif (fx%state==ST_COMMENT_END_2) then
      fx%token => vs_str_alloc(get_characters(fb, 1, iostat, fx%error_stack))
      if (iostat/=0) return

    elseif (fx%state==ST_CDATA_CONTENTS) then
      call get_characters_until_all_of(fb, ']]>', iostat, fx%error_stack)
      if (iostat/=0) return
      fx%next_token => vs_str_alloc(get_characters(fb, 3, iostat, fx%error_stack))
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)

    elseif (fx%state==ST_CHAR_IN_CONTENT) then
      call get_characters_until_one_of(fb, '<&', iostat, fx%error_stack)
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)
      if (iostat/=0) return

    elseif (fx%state==ST_DTD_ELEMENT_CONTENTS) then
      c = get_characters(fb, 1, iostat, fx%error_stack)
      if (iostat/=0) return
      if (verify(c,XML_WHITESPACE)==0) then
        call get_characters_until_all_of(fb, '>', iostat, fx%error_stack)
        if (iostat/=0) return
        fx%next_token => vs_str_alloc(get_characters(fb, 1, iostat, fx%error_stack))
        if (iostat/=0) return
        fx%token => fb%namebuffer
        nullify(fb%namebuffer)
      else
        call add_error(fx%error_stack, &
          'Missing whitespace in element declaration')
      endif

    elseif (fx%state==ST_DTD_ATTLIST_CONTENTS) then
      c = get_characters(fb, 1, iostat, fx%error_stack)
      if (iostat/=0) return
      if (verify(c,XML_WHITESPACE)==0) then
        call get_characters_until_all_of(fb, '>', iostat, fx%error_stack)
        if (iostat/=0) return
        fx%next_token => vs_str_alloc(get_characters(fb, 1, iostat, fx%error_stack))
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
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat, fx%error_stack)
      if (iostat/=0) return
      if (size(fb%namebuffer)==0) then
        deallocate(fb%namebuffer) ! which only contains whitespace now.
        ! This had better be the end of the tag.
        c = get_characters(fb, 1, iostat, fx%error_stack)
        if (iostat/=0) return
        if (c=='>') then
          fx%token => vs_str_alloc('>')
        elseif (c=='/') then
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          if (c=='>') then
            fx%token => vs_str_alloc('/>')
          endif
        else
          call add_error(fx%error_stack, "Unexpected character in tag.")
        endif
      else
        deallocate(fb%namebuffer) ! which only contains whitespace now.
        c = get_characters(fb, 1, iostat, fx%error_stack)
        if (iostat/=0) return
        if (c=='>') then
          fx%token => vs_str_alloc('>')
        elseif (c=='/') then
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          if (c=='>') then
            fx%token => vs_str_alloc('/>')
          endif
        elseif (isInitialNameChar(c, fx%xds%xml_version)) then 
          call get_characters_until_not_namechar(fb, fx%xds%xml_version, iostat, fx%error_stack)
          if (iostat/=0) return
          fx%token => vs_str_alloc(c//str_vs(fb%namebuffer))
          deallocate(fb%namebuffer)
        else
          call add_error(fx%error_stack, "Unexpected character in tag.")
        endif
      endif

    elseif (fx%state==ST_ATT_EQUALS) then
      call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat, fx%error_stack)
      if (iostat/=0) return
      c = get_characters(fb, 1, iostat, fx%error_stack)
      if (iostat/=0) return
      if (c/='"'.and.c/="'") then
        call add_error(fx%error_stack, "Expecting "" or '")
        return
      endif
      call get_characters_until_all_of(fb, c, iostat, fx%error_stack)
      if (iostat/=0) return
      fx%token => fb%namebuffer
      nullify(fb%namebuffer)
      ! Next character is either quotechar or eof.
      c = get_characters(fb, 1, iostat, fx%error_stack)
      ! Either way, we return

    elseif (fx%context==CTXT_IN_DTD) then
      if (fx%whitespace==WS_FORBIDDEN) then
        ! This MUST be the end of a comment
        c = get_characters(fb, 1, iostat, fx%error_stack)
        if (iostat/=0) return
        fx%token => vs_str_alloc(c)
        ! it will either be a <!DIRECTIVE or a <?PINAME
        !call get_characters_until_one_of(fb, XML_WHITESPACE, iostat)
        !if (iostat/=0) return
        !fx%token => fb%namebuffer
        !nullify(fb%namebuffer)

      elseif (fx%whitespace==WS_MANDATORY) then
        !We are still allowed a '>' without space, ... check first.
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat, fx%error_stack)
        if (iostat/=0) return
        if (size(fb%namebuffer)==0) then
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          if (c=='>') then
            fx%token => vs_str_alloc(c)
          else
            call add_error(fx%error_stack, "Required whitespace omitted in DTD")
          endif
          return
        endif
        c = get_characters(fb, 1, iostat, fx%error_stack)
        if (iostat/=0) return
        ! do some stuff
        if (verify(c,"%>[]")==0) then
          fx%token => vs_str_alloc(c)
        elseif (c=='<') then
          !it's a comment or a PI ... or a DTD keyword.
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          if (c=='?') then
            fx%token => vs_str_alloc('<?')
          elseif (c=='!') then
            fx%token => vs_str_alloc('<!')
          endif
        elseif (c=='"'.or.c=="'") then ! grab until next quote
          call get_characters_until_all_of(fb, c, iostat, fx%error_stack)
          if (iostat/=0) return
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          fx%token => vs_str_alloc(c//str_vs(fb%namebuffer)//c)
          deallocate(fb%namebuffer)
        else !it must be a NAME for some reason
          call push_chars(fb, c)
          call get_characters_until_not_namechar(fb, fx%xds%xml_version, iostat, fx%error_stack)
          if (iostat/=0) return
          fx%token => fb%namebuffer
          nullify(fb%namebuffer)
        endif

      elseif (fx%whitespace==WS_DISCARD) then
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat, fx%error_stack)
        if (iostat/=0) return
        c = get_characters(fb, 1, iostat, fx%error_stack)
        if (iostat/=0) return
        if (verify(c,">[]")==0) then
          fx%token => vs_str_alloc(c)
        elseif (c=='<') then
          !it's a comment or a PI ... or a DTD keyword.
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          if (c=='?') then
            fx%token => vs_str_alloc('<?')
          elseif (c=='!') then
            fx%token => vs_str_alloc('<!')
          else
            call add_error(fx%error_stack, &
              "Invalid token found in DTD")
            return
          endif
        elseif (c=='%') then
          !It ought to be a parameter entity reference
          call get_characters_until_not_namechar(fb, fx%xds%xml_version, iostat, fx%error_stack)
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          if (c/=';') then
            call add_error(fx%error_stack, "Illegal parameter entity reference.")
            return
          endif
          fx%token => vs_str_alloc('%'//str_vs(fb%namebuffer)//';')
        else
          call add_error(fx%error_stack, &
            "Invalid token found in DTD")
          return
        endif
      endif
      return


    else

      if (fx%whitespace==WS_DISCARD) then
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat, fx%error_stack)
        if (iostat/=0) return

      elseif (fx%whitespace==WS_MANDATORY) then
        call get_characters_until_not_one_of(fb, XML_WHITESPACE, iostat, fx%error_stack)
        if (size(fb%namebuffer)==0) then
          call add_error(fx%error_stack, 'Tokenizer expected whitespace')
          return
        endif
      endif

      c = get_characters(fb, 1, iostat, fx%error_stack)
      if (iostat/=0) return

      if (verify(c,'>=[')==0) then
        !No further investigation needed, that's the token
        fx%token => vs_str_alloc(c)

      elseif (isInitialNameChar(c, fx%xds%xml_version)) then
        call get_characters_until_not_namechar(fb, fx%xds%xml_version, iostat, fx%error_stack)
        if (iostat/=0) return
        fx%token => vs_str_alloc(c//str_vs(fb%namebuffer))
        deallocate(fb%namebuffer)

      else
        select case(c)

        case ('<')
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          if (c=='?') then
            fx%token => vs_str_alloc('<?')
          elseif (c=='!') then
            fx%token => vs_str_alloc('<!')
          elseif (c=='/') then
            fx%token => vs_str_alloc('</')
          elseif (isInitialNameChar(c, fx%xds%xml_version)) then
            call push_chars(fb, c)
            fx%token => vs_str_alloc('<')
          else
            call add_error(fx%error_stack,"Unexpected character found.")
          endif

        case ('/')
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) return
          if (c=='>') then
            fx%token => vs_str_alloc('/>')
          else
            call add_error(fx%error_stack, "Unexpected character after /")
          endif

        case ('&')
          c = get_characters(fb, 1, iostat, fx%error_stack)
          if (iostat/=0) then
            call add_error(fx%error_stack, 'Bare ampersand found.')
            return
          elseif (c=='#') then
            c = get_characters(fb, 1, iostat, fx%error_stack)
            if (iostat/=0) return
            if (c=='x') then
              call get_characters_until_not_one_of(fb, hexdigits, iostat, fx%error_stack)
              if (size(fb%namebuffer)==0) then
                call add_error(fx%error_stack, 'Illegal character entity reference')
                return
              endif
              if (iostat/=0) return
              c2 = get_characters(fb, 1, iostat, fx%error_stack)
              if (iostat/=0) return
              if (c2/=';') then
                call add_error(fx%error_stack, &
                  'Illegal character entity reference')
                return
              endif
              fx%token => vs_str_alloc('&#x'//str_vs(fb%namebuffer)//';')
              deallocate(fb%namebuffer)
            elseif (verify(c,digits)==0) then
              call get_characters_until_not_one_of(fb, digits, iostat, fx%error_stack)
              if (iostat/=0) return
              c2 = get_characters(fb, 1, iostat, fx%error_stack)
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
          elseif (isInitialNameChar(c, fx%xds%xml_version)) then
            call get_characters_until_not_namechar(fb, fx%xds%xml_version, iostat, fx%error_stack)
            if (iostat/=0) return
            c2 = get_characters(fb, 1, iostat, fx%error_stack)
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
            call get_characters_until_one_of(fb, '"', iostat, fx%error_stack)
            if (iostat/=0) return
            deallocate(fb%namebuffer)
            fx%token => vs_str_alloc('"'//str_vs(fb%namebuffer)//'"')
          else
            ! make an error
          endif

        case ("'")
          if (fx%context==CTXT_IN_CONTENT) then! .and. some other condition) then
            call get_characters_until_one_of(fb, "'", iostat, fx%error_stack)
            if (iostat/=0) return
            deallocate(fb%namebuffer)
            fx%token = vs_str_alloc("'"//str_vs(fb%namebuffer)//"'")

          else

            call add_error(fx%error_stack, "Unrecognized token.")
            return
          endif

        case default 
          ! return c as token anyway - probably meaningless though
          fx%token => vs_str_alloc(c)

        end select

      end if ! more than one-char token
    endif

  end subroutine sax_tokenize


  recursive function normalize_text(fx, s_in) result(s_out)
    type(sax_parser_t), intent(inout) :: fx
    character, dimension(:), intent(in) :: s_in
    character, dimension(:), pointer :: s_out

    character, dimension(:), pointer :: s_temp, s_temp2, s_ent, tempString
    character :: dummy
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
      if (verify(s_in(i),XML_WHITESPACE)==0) then
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
        elseif (checkCharacterEntityReference(str_vs(tempString), fx%xds%xml_version)) then
          ! Expand all character entities
          s_temp(i2) = expand_char_entity(str_vs(tempString)) ! FIXME ascii
          i = i + j  + 1
          i2 = i2 + 1 ! fixme
        elseif (checkName(str_vs(tempString), fx%xds)) then
          if (existing_entity(fx%forbidden_ge_list, str_vs(tempString))) then
            call add_error(fx%error_stack, 'Recursive entity expansion')
            goto 100
          elseif (existing_entity(fx%xds%entityList, str_vs(tempString))) then
            !is it the right sort of entity?
            if (is_unparsed_entity(fx%xds%entityList, str_vs(tempString))) then
              call add_error(fx%error_stack, "Unparsed entity forbidden in attribute")
              goto 100
            elseif (is_external_entity(fx%xds%entityList, str_vs(tempString))) then
              call add_error(fx%error_stack, "External entity forbidden in attribute")
              goto 100
            endif
            call add_internal_entity(fx%forbidden_ge_list, str_vs(tempString), "")
            ! Recursively expand entity, checking for errors.
            s_ent => normalize_text(fx, vs_str(expand_entity_text(fx%xds%entityList, str_vs(tempString))))
            dummy = pop_entity_list(fx%forbidden_ge_list)
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
            if (.not.fx%skippedExternal.or.fx%xds%standalone) then
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

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

  recursive subroutine sax_parse(fx, fb,  &
    begin_element_handler,    &
    end_element_handler,             &
    start_prefix_handler,            &
    end_prefix_handler,              &
    character_handler,            &
    comment_handler,                 &
    processing_instruction_handler,  &
    error_handler,                   &
    start_document_handler,          & 
    end_document_handler)
    
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    optional                            :: begin_element_handler
    optional                            :: end_element_handler
    optional                            :: start_prefix_handler
    optional                            :: end_prefix_handler
    optional                            :: character_handler
    optional                            :: comment_handler
    optional                            :: processing_instruction_handler
    optional                            :: error_handler
    optional                            :: start_document_handler
    optional                            :: end_document_handler

    interface
      subroutine begin_element_handler(namespaceURI, localName, name, attributes)
        use FoX_common
        character(len=*), intent(in)     :: namespaceUri
        character(len=*), intent(in)     :: localName
        character(len=*), intent(in)     :: name
        type(dictionary_t), intent(in)   :: attributes
      end subroutine begin_element_handler

      subroutine end_element_handler(namespaceURI, localName, name)
        character(len=*), intent(in)     :: namespaceURI
        character(len=*), intent(in)     :: localName
        character(len=*), intent(in)     :: name
      end subroutine end_element_handler

      subroutine start_prefix_handler(namespaceURI, prefix)
        character(len=*), intent(in) :: namespaceURI
        character(len=*), intent(in) :: prefix
      end subroutine start_prefix_handler

      subroutine end_prefix_handler(prefix)
        character(len=*), intent(in) :: prefix
      end subroutine end_prefix_handler

      subroutine character_handler(chunk)
        character(len=*), intent(in) :: chunk
      end subroutine character_handler

      subroutine comment_handler(comment)
        character(len=*), intent(in) :: comment
      end subroutine comment_handler

      subroutine processing_instruction_handler(name, content)
        use FoX_common
        character(len=*), intent(in)     :: name
        character(len=*), intent(in)     :: content
      end subroutine processing_instruction_handler

      subroutine error_handler(msg)
        character(len=*), intent(in)     :: msg
      end subroutine error_handler

      subroutine start_document_handler()   
      end subroutine start_document_handler

      subroutine end_document_handler()     
      end subroutine end_document_handler
    end interface

    integer :: iostat

    iostat = 0
    
    if (fx%parse_stack==0) then
      call parse_xml_declaration(fx, fb, iostat)
      if (iostat/=0) goto 100

      fx%context = CTXT_BEFORE_DTD
      fx%state = ST_MISC
      fx%discard_whitespace = .true.
    endif
    print*,'XML declaration parsed.', fb%input_pos

    do
      print*,'executing parse loop'
      print*,fb%buffer(1:1)

      call sax_tokenize(fx, fb, iostat)
      if (iostat/=0) goto 100
      print*,'token',str_vs(fx%token)

      select case (fx%context)

      case (ST_MISC)
        print*,'ST_MISC'
        if (str_vs(fx%token) == '<?') then
          fx%state = ST_START_PI
        elseif (str_vs(fx%token) ==  '<!') then
          fx%state = ST_BANG_TAG
        elseif (str_vs(fx%token) == '<') then
          print*,'starting tag'
          stop
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
          if (fx%context /= CTXT_IN_CONTENT) then
            if(present(start_document_handler)) call start_document_handler()
            fx%context = CTXT_IN_CONTENT
          endif
          fx%state = ST_CHAR_IN_CONTENT
          fx%discard_whitespace = .false.
        elseif (str_vs(fx%token)=='/>') then
          !token should be xmlname
          if (fx%context==CTXT_IN_CONTENT) then
            fx%state = ST_CHAR_IN_CONTENT
          else
            ! only a single element in this doc
            if(present(start_document_handler)) call start_document_handler()
          endif
          ! begin and end
          if (fx%context==CTXT_IN_CONTENT) then
            fx%discard_whitespace = .false.
          else
            if(present(end_document_handler)) call end_document_handler()
            fx%context = CTXT_AFTER_CONTENT
            fx%state = ST_MISC
            fx%discard_whitespace = .true.
          endif
          ! open & close
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
          call sax_parse(fx, fb, &
            begin_element_handler,    &
            end_element_handler,             &
            start_prefix_handler,            &
            end_prefix_handler,              &
            character_handler,            &
            comment_handler,                 &
            processing_instruction_handler,  &
            error_handler,                   &
            start_document_handler,          & 
            end_document_handler)
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

    end do

100 if (iostat==io_eof) then ! error is end of file then
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
          call sax_error(fx, error_handler)
        endif
      endif
    else ! Hard error - stop immediately
      if (fx%parse_stack>0) then !we are parsing an entity
        call add_parse_error(fx, "Internal error: Error encountered processing entity.")
      else
        call sax_error(fx, error_handler)
      endif
    endif

  end subroutine sax_parse

  subroutine sax_error(fx, error_handler)
    type(sax_parser_t), intent(inout) :: fx
    optional :: error_handler
    interface
      subroutine error_handler(msg)
        character(len=*), intent(in)     :: msg
      end subroutine error_handler
    end interface

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
    if (present(error_handler)) then
      call error_handler(str_vs(errmsg))
    else
      call FoX_error(str_vs(errmsg))
    endif

  end subroutine sax_error
      
end module m_sax_parser

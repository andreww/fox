! XML parser

module m_sax_parser

  use m_common_array_str, only: str_vs, vs_str
  use m_common_attrs, only: init_dict, destroy_dict, add_item_to_dict
  use m_common_charset, only: XML1_0, XML1_1, XML1_0_INITIALNAMECHARS, &
    XML1_1_INITIALNAMECHARS, XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML_WHITESPACE, XML1_0_NAMECHARS, XML1_1_NAMECHARS, operator(.in.)
  use m_common_elstack, only: elstack_t, push_elstack, pop_elstack, &
    init_elstack, destroy_elstack, is_empty, get_top_elstack, len
  use m_common_error, only: FoX_error
  use m_common_io, only: io_eof, io_err
  use m_common_namecheck, only: checkName
  use m_common_namespaces, only: getnamespaceURI, invalidNS, &
    checkNamespaces, checkEndNamespaces, &
    initNamespaceDictionary, destroyNamespaceDictionary

  use m_sax_reader, only: file_buffer_t
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
    
    call init_elstack(fx%elstack)
    call initNamespaceDictionary(fx%nsdict)
    
  end subroutine sax_parser_init

  subroutine sax_parser_destroy(fx)
    type(sax_parser_t), intent(inout) :: fx
 
    integer :: i

    fx%context = CTXT_NULL
    fx%state = ST_NULL

    deallocate(fx%encoding)
    if (associated(fx%token)) deallocate(fx%token)
    if (associated(fx%root_element)) deallocate(fx%root_element)
    if (associated(fx%error_stack(0)%msg)) deallocate(fx%error_stack(0)%msg)
    do i = 1, ubound(fx%error_stack,1)
      deallocate(fx%error_stack(i)%msg)
    enddo
    deallocate(fx%error_stack)

    stop
    call destroy_elstack(fx%elstack)
    call destroy_dict(fx%attributes)
    call destroyNamespaceDictionary(fx%nsdict)

  end subroutine sax_parser_destroy

  recursive subroutine sax_parse(fx, fb,  &
    begin_element_handler,                &
    end_element_handler,                  &
    start_prefix_handler,                 &
    end_prefix_handler,                   &
    characters_handler,                   &
    comment_handler,                      &
    processing_instruction_handler,       &
    error_handler,                        &
    start_document_handler,               & 
    end_document_handler)
    
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    optional                            :: begin_element_handler
    optional                            :: end_element_handler
    optional                            :: start_prefix_handler
    optional                            :: end_prefix_handler
    optional                            :: characters_handler
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

      subroutine characters_handler(chunk)
        character(len=*), intent(in) :: chunk
      end subroutine characters_handler

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
      fx%whitespace = WS_DISCARD
      print*,'XML declaration parsed.', fb%input_pos
      if(present(start_document_handler)) &
        call start_document_handler()
    endif

    do
      print*,'executing parse loop'

      call sax_tokenize(fx, fb, iostat)
      if (fx%error) iostat = io_err
      if (iostat/=0) goto 100
      print*,'token: ',str_vs(fx%token)

      select case (fx%state)

      case (ST_MISC)
        print*,'ST_MISC'
        if (str_vs(fx%token) == '<?') then
          fx%state = ST_START_PI
          fx%whitespace = WS_FORBIDDEN
        elseif (str_vs(fx%token) ==  '<!') then
          fx%state = ST_BANG_TAG
          fx%whitespace = WS_FORBIDDEN
        elseif (str_vs(fx%token) == '<') then
          fx%state = ST_START_TAG
          fx%whitespace = WS_FORBIDDEN
        else
          call add_parse_error(fx, "Unexpected token found outside content")
          return
        endif

      case (ST_BANG_TAG)
        if (str_vs(fx%token) == '--') then
          fx%state = ST_START_COMMENT
        elseif (str_vs(fx%token) == 'DOCTYPE') then
          fx%whitespace = WS_MANDATORY
          ! go to DTD parser FIXME
        elseif (str_vs(fx%token) == '[') then
          if (fx%context == CTXT_IN_CONTENT) then
            fx%state = ST_START_CDATA_1
          else
            call add_parse_error(fx, "Unexpected token after !")
            return
          endif
        else
          call add_parse_error(fx, "Unexpected token after !")
          return
        endif

      case (ST_START_PI)
        print*,'ST_START_PI'
        !token should be an XML Name FIXME
        if (checkName(str_vs(fx%token))) then
          fx%whitespace = WS_MANDATORY
          fx%state = ST_PI_CONTENTS
          fx%name => fx%token
          nullify(fx%token)
        else
          call add_parse_error(fx, "Unexpected token found for PI target; expecting Name")
          return
        endif

      case (ST_PI_CONTENTS)
        print*,'ST_PI_CONTENTS'
        if (str_vs(fx%token)=='?>') then
          ! No data for this PI
          if (present(processing_instruction_handler)) &
            call processing_instruction_handler(str_vs(fx%name), '')
          deallocate(fx%name)
          if (fx%context==CTXT_IN_CONTENT) then
            fx%state = ST_CHAR_IN_CONTENT
          else
            fx%state = ST_MISC
          endif
        else
          if (present(processing_instruction_handler)) &
            call processing_instruction_handler(str_vs(fx%name), str_vs(fx%token))
          deallocate(fx%name)
          fx%state = ST_PI_END
        endif

      case (ST_PI_END)
        print*,'ST_PI_END'
        if (str_vs(fx%token)=='?>') then
          if (fx%context==CTXT_IN_CONTENT) then
            fx%state = ST_CHAR_IN_CONTENT
            fx%whitespace = WS_PRESERVE
          else
            fx%state = ST_MISC
            fx%whitespace = WS_DISCARD
          endif
        else
          call add_parse_error(fx, "Internal error: unexpected token at end of PI, expecting ?>")
          return
        endif

      case (ST_START_COMMENT)
        print*,'ST_START_COMMENT'
        fx%name => fx%token
        nullify(fx%token)
        fx%state = ST_COMMENT_END_1

      case (ST_COMMENT_END_1)
        print*,'ST_COMMENT_END_1'
        if (str_vs(fx%token)=='--') then
          fx%state = ST_COMMENT_END_2
        else
          call add_parse_error(fx, "Internal error: expecting --")
          return
        endif

      case (ST_COMMENT_END_2)
        print*,'ST_COMMENT_END_2'
        if (str_vs(fx%token)=='>') then
          if (present(comment_handler)) &
            call comment_handler(str_vs(fx%name))
          deallocate(fx%name)
          if (fx%context==CTXT_IN_CONTENT) then
            fx%state = ST_CHAR_IN_CONTENT
          else
            fx%state = ST_MISC
            fx%whitespace = WS_DISCARD
          endif
        else
          call add_parse_error(fx, "Expecting > after -- in comment")
          return
        endif

      case (ST_START_TAG)
        print*,'ST_START_TAG', fx%context
        if (fx%context==CTXT_BEFORE_DTD &
          .or. fx%context==CTXT_BEFORE_CONTENT &
          .or. fx%context==CTXT_IN_CONTENT) then
          fx%name => fx%token
          nullify(fx%token)
          ! FIXME check name is name? ought to be.
          fx%whitespace = WS_MANDATORY
          fx%state = ST_IN_TAG
          call init_dict(fx%attributes)
        elseif (fx%context == CTXT_AFTER_CONTENT) then
          call add_parse_error(fx, "Cannot open second root element")
        elseif (fx%context == CTXT_IN_DTD) then
          call add_parse_error(fx, "Cannot open root element before DTD is finished")
        endif

      case (ST_START_CDATA_1)
        print*,'ST_START_CDATA_1'
        if (str_vs(fx%token) == 'CDATA') then
          fx%state = ST_START_CDATA_2
        else
          call add_parse_error(fx, "Unexpected token found - expecting CDATA afte <![")
        endif

      case (ST_START_CDATA_2)
        print*,'ST_START_CDATA_2'
        if (str_vs(fx%token) == '[') then
          fx%state = ST_CDATA_CONTENTS
        else
          call add_parse_error(fx, "Unexpected token found - expecting [ after CDATA")
        endif

      case (ST_CDATA_CONTENTS)
        print*,'ST_CDATA_CONTENTS'
        fx%name => fx%token
        nullify(fx%token)
        fx%state = ST_CDATA_END

      case (ST_CDATA_END)
        print*,'ST_CDATA_END'
        if (str_vs(fx%token) == ']]>') then
          if (size(fx%name)>0) then
            if (present(characters_handler)) &
              call characters_handler(str_vs(fx%name))
          endif
          deallocate(fx%name)
          fx%state = ST_CHAR_IN_CONTENT
        else
          call add_parse_error(fx, "Internal error, unexpected token in CDATA")
        endif

      case (ST_IN_TAG)
        print*,'ST_IN_TAG'
        if (str_vs(fx%token)=='>') then
          if (fx%context /= CTXT_IN_CONTENT) then
            if (associated(fx%root_element)) then
              if (str_vs(fx%name)/=str_vs(fx%root_element)) then
                call add_parse_error(fx, "Root element name does not match document name")
                return
              endif
            else
              fx%root_element => fx%name
            endif
            call open_tag
            if (fx%error) goto 100
            nullify(fx%name)
            fx%context = CTXT_IN_CONTENT
          else
            call open_tag
            if (fx%error) goto 100
            deallocate(fx%name)
          endif
          call destroy_dict(fx%attributes)
          fx%state = ST_CHAR_IN_CONTENT

        elseif (str_vs(fx%token)=='/>') then
          if (fx%context==CTXT_IN_CONTENT) then
            fx%state = ST_CHAR_IN_CONTENT
          else
            ! only a single element in this doc
            if (associated(fx%root_element)) then
              if (str_vs(fx%name)/=str_vs(fx%root_element)) then
                call add_parse_error(fx, "Root element name does not match document name")
                return
              endif
            else
              fx%root_element => fx%name
              nullify(fx%name)
            endif
          endif
          call open_tag
          if (fx%error) goto 100
          call close_tag
          if (fx%error) goto 100
          if (fx%context==CTXT_IN_CONTENT) then
            deallocate(fx%name)
            fx%whitespace = WS_PRESERVE
          else
            fx%well_formed = .true.
            nullify(fx%name)
            fx%context = CTXT_AFTER_CONTENT
            fx%state = ST_MISC
          endif
        else
          ! FIXME It should be an XML name for the attribute
          ! pick up that it is a name:
          fx%attname => fx%token
          nullify(fx%token)
          fx%state = ST_ATT_NAME
          fx%whitespace = WS_DISCARD
        endif

      case (ST_ATT_NAME)
        print*,'ST_ATT_NAME'
        if (str_vs(fx%token)=='=') then
          fx%state = ST_ATT_EQUALS
        else
          call add_parse_error(fx, "Unexpected token in tag - expected =")
        endif

      case (ST_ATT_EQUALS)
        print*,'ST_ATT_EQUALS'
        ! token is pre-processed attribute value.
        ! fx%name still contains attribute name
        ! Is it an xmlns:?
        ! Is it an xml:?
        call add_item_to_dict(fx%attributes, str_vs(fx%attname), &
          str_vs(fx%token))
        deallocate(fx%attname)
        fx%state = ST_IN_TAG

      case (ST_CHAR_IN_CONTENT)
        print*,'ST_CHAR_IN_CONTENT'
        if (size(fx%token)>0) then
          if (present(characters_handler)) call characters_handler(str_vs(fx%token))
        endif
        fx%state = ST_TAG_IN_CONTENT

      case (ST_TAG_IN_CONTENT)
        print*,'ST_TAG_IN_CONTENT'
        if (str_vs(fx%token)=='<') then
          fx%state = ST_START_TAG
        elseif (str_vs(fx%token)=='<!') then
          fx%state = ST_BANG_TAG
        elseif (str_vs(fx%token)=='<?') then
          fx%state = ST_START_PI
        elseif (str_vs(fx%token)=='</') then
          fx%state = ST_CLOSING_TAG
        elseif (fx%token(1)=='&') then
          ! tell tokenizer to expand it
          call sax_parse(fx, fb,             &
            begin_element_handler,           &
            end_element_handler,             &
            start_prefix_handler,            &
            end_prefix_handler,              &
            characters_handler,              &
            comment_handler,                 &
            processing_instruction_handler,  &
            error_handler,                   &
            start_document_handler,          & 
            end_document_handler)
          if (iostat/=0) goto 100
        else
          call add_parse_error(fx, "Unexpected token found in character context")
        endif

      case (ST_CLOSING_TAG)
        print*,'ST_CLOSING_TAG'
        if (checkName(str_vs(fx%token))) then!fx%token, fx%xml_version)) then
          fx%name => fx%token
          nullify(fx%token)
          fx%whitespace = WS_DISCARD
          fx%state = ST_IN_CLOSING_TAG
        else
          call add_parse_error(fx, "Unexpected token found in closing tag: expecting a Name")
        endif

      case (ST_IN_CLOSING_TAG)
        print*,'ST_IN_CLOSING_TAG'
        if (str_vs(fx%token)=='>') then
          if (str_vs(fx%name)/=pop_elstack(fx%elstack)) then
            call add_parse_error(fx, "Mismatching close tag - expecting "//str_vs(fx%name))
            return
          endif
          !if (present(end_element_handler)) call end_element_handler()
          deallocate(fx%name)
          if (is_empty(fx%elstack)) then
            !we're done
            fx%well_formed = .true.
            fx%state = ST_MISC
            fx%context = CTXT_AFTER_CONTENT
            fx%whitespace = WS_DISCARD
          else
            fx%whitespace = WS_PRESERVE
            fx%state = ST_CHAR_IN_CONTENT
          endif
        else
          call add_parse_error(fx, "Unexpected token in closing tag - expecting Name")
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
        if (fx%well_formed.and.fx%state==ST_MISC) then
          if (present(end_document_handler)) &
            call end_document_handler()
        else
          call add_parse_error(fx, "File is not well-formed")
          call sax_error(fx, error_handler)
        endif
      endif
    elseif (iostat==io_err) then ! we generated the error
      if (fx%parse_stack>0) then
        ! append to error all the way up the stack
        return ! go back up stack
      else
        call sax_error(fx, error_handler)
      endif
    else ! Hard error - stop immediately
      if (fx%parse_stack>0) then !we are parsing an entity
        call add_parse_error(fx, "Internal error: Error encountered processing entity.")
      else
        call sax_error(fx, error_handler)
      endif
    endif

    contains

      subroutine open_tag
        call checkNamespaces(fx%attributes, fx%nsDict, &
          len(fx%elstack), start_prefix_handler)
        if (getURIofQName(fx,str_vs(fx%name))==invalidNS) then
          ! no namespace was found for the current element
          call add_parse_error(fx, "No namespace found for current element")
          return
        endif
        call push_elstack(str_vs(fx%name), fx%elstack)
        ! No point in pushing & pulling onto elstack.
        if (present(begin_element_handler)) &
          call begin_element_handler(getURIofQName(fx, str_vs(fx%name)), &
          getlocalNameofQName(str_vs(fx%name)), &
          str_vs(fx%name), fx%attributes)
        call destroy_dict(fx%attributes)
      end subroutine open_tag

      subroutine close_tag
        if (str_vs(fx%name)/=pop_elstack(fx%elstack)) then
          call add_parse_error(fx, "Mismatching close tag - expecting "//str_vs(fx%name))
          return
        endif
        if (present(end_element_handler)) &
          call end_element_handler(getURIofQName(fx, str_vs(fx%name)), &
          getlocalnameofQName(str_vs(fx%name)), &
          str_vs(fx%name))
        call checkEndNamespaces(fx%nsDict, len(fx%elstack)+1, &
          end_prefix_handler)
      end subroutine close_tag

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

  pure function getURIofQName(fx, qname) result(URI)
    type(sax_parser_t), intent(in) :: fx
    character(len=*), intent(in) :: qName
    character(len=URIlength(fx, qname)) :: URI
    
    integer :: n
    n = index(QName, ':')
    if (n > 0) then
       URI = getnamespaceURI(fx%nsDict, QName(1:n-1))
    else
       URI = getnamespaceURI(fx%nsDict)
    endif

  end function getURIofQName
  
  pure function URIlength(fx, qname) result(l_u)
    type(sax_parser_t), intent(in) :: fx
    character(len=*), intent(in) :: qName
    integer :: l_u
    integer :: n
    n = index(QName, ':')
    if (n > 0) then
       l_u = len(getnamespaceURI(fx%nsDict, QName(1:n-1)))
    else
       l_u = len(getnamespaceURI(fx%nsDict))
    endif
  end function URIlength

  pure function getLocalNameofQName(qname) result(localName)
    character(len=*), intent(in) :: qName
    character(len=len(QName)-index(QName,':')) :: localName
    
    localName = QName(index(QName,':')+1:)
  end function getLocalNameofQName
      
end module m_sax_parser

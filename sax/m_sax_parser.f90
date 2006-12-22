! XML parser

module m_sax_parser

  use m_common_array_str, only: str_vs, vs_str
  use m_common_attrs, only: init_dict, destroy_dict, add_item_to_dict
  use m_common_charset, only: XML1_0, XML1_1, XML1_0_INITIALNAMECHARS, &
    XML1_1_INITIALNAMECHARS, XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML_WHITESPACE, XML1_0_NAMECHARS, XML1_1_NAMECHARS, operator(.in.)
  use m_common_elstack, only: elstack_t, push_elstack, pop_elstack, &
    init_elstack, destroy_elstack, is_empty, get_top_elstack, len
  use m_common_entities, only: existing_entity, &
    init_entity_list, destroy_entity_list, &
    add_internal_entity, add_external_entity
  use m_common_error, only: FoX_error
  use m_common_io, only: io_eof, io_err
  use m_common_namecheck, only: checkName, checkSystemId, checkPubId
  use m_common_namespaces, only: getnamespaceURI, invalidNS, &
    checkNamespaces, checkEndNamespaces, &
    initNamespaceDictionary, destroyNamespaceDictionary
  use m_common_notations, only: init_notation_list, destroy_notation_list, &
    add_notation, notation_exists

  use m_sax_reader, only: file_buffer_t
  use m_sax_tokenizer, only: sax_tokenize, parse_xml_declaration, &
    add_parse_error
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
    call init_notation_list(fx%nlist)
    call init_entity_list(fx%ge_list)
    call init_entity_list(fx%pe_list)
    
  end subroutine sax_parser_init

  subroutine sax_parser_destroy(fx)
    type(sax_parser_t), intent(inout) :: fx
 
    integer :: i

    fx%context = CTXT_NULL
    fx%state = ST_NULL

    if (associated(fx%encoding)) deallocate(fx%encoding)
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
    call destroy_notation_list(fx%nlist)
    call destroy_entity_list(fx%ge_list)
    call destroy_entity_list(fx%pe_list)

    if (associated(fx%token)) deallocate(fx%token)
    if (associated(fx%next_token)) deallocate(fx%next_token)
    if (associated(fx%name)) deallocate(fx%name)
    if (associated(fx%attname)) deallocate(fx%attname)
    if (associated(fx%publicId)) deallocate(fx%publicId)
    if (associated(fx%systemId)) deallocate(fx%systemId)
    if (associated(fx%entityContent)) deallocate(fx%entityContent)
    if (associated(fx%Ndata)) deallocate(fx%Ndata)

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
    end_document_handler,                 &
    startDTD_handler,                     &
    endDTD_handler,                       &
    startCdata_handler,                   &
    endCdata_handler,                     &
    unparsedEntityDecl_handler,           &
    notationDecl_handler)
    
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
    !optional :: ignorableWhitespace
    optional :: startDTD_handler
    optional :: endDTD_handler
    optional :: startCdata_handler
    optional :: endCdata_handler
    optional :: notationDecl_handler
    optional :: unparsedEntityDecl_handler

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

      subroutine unparsedEntityDecl_handler(name, publicId, systemId, notation)
        character(len=*), intent(in) :: name
        character(len=*), optional, intent(in) :: publicId
        character(len=*), intent(in) :: systemId
        character(len=*), intent(in) :: notation
      end subroutine unparsedEntityDecl_handler

      subroutine notationDecl_handler(name, publicId, systemId)
        character(len=*), intent(in) :: name
        character(len=*), optional, intent(in) :: publicId
        character(len=*), optional, intent(in) :: systemId
      end subroutine notationDecl_handler

      subroutine startDTD_handler(name, publicId, systemId)
        character(len=*), intent(in) :: name
        character(len=*), optional, intent(in) :: publicId
        character(len=*), intent(in) :: systemId
      end subroutine startDTD_handler

      subroutine endDTD_handler()
      end subroutine endDTD_handler

      subroutine startCdata_handler()
      end subroutine startCdata_handler

      subroutine endCdata_handler()
      end subroutine endCdata_handler
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
      if (iostat/=0) then
        !fx%state = ST_NULL
        goto 100
      endif
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
          exit
        endif

      case (ST_BANG_TAG)
        if (str_vs(fx%token)=='--') then
          fx%state = ST_START_COMMENT
        elseif (fx%context==CTXT_BEFORE_DTD) then
          if (str_vs(fx%token)=='DOCTYPE') then
            fx%whitespace = WS_MANDATORY
            fx%context = CTXT_IN_DTD
            fx%state = ST_IN_DTD
          endif
        elseif (fx%context==CTXT_IN_CONTENT) then
          if (str_vs(fx%token)=='[') then
            fx%state = ST_START_CDATA_1
          endif
        elseif (fx%context==CTXT_IN_DTD) then  
          if (str_vs(fx%token)=='ATTLIST') then
            fx%state = ST_DTD_ATTLIST
          elseif (str_vs(fx%token)=='ELEMENT') then
            fx%state = ST_DTD_ELEMENT
          elseif (str_vs(fx%token)=='ENTITY') then
            fx%state = ST_DTD_ENTITY
          elseif (str_vs(fx%token)=='NOTATION') then
            fx%state = ST_DTD_NOTATION
          endif
          fx%whitespace = WS_MANDATORY
        else
          call add_parse_error(fx, "Unexpected token after !")
          exit
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
          exit
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
          elseif (fx%context==CTXT_IN_DTD) then
            fx%state = ST_INT_SUBSET
          else
            fx%state = ST_MISC
            fx%whitespace = WS_DISCARD
          endif
        else
          call add_parse_error(fx, "Internal error: unexpected token at end of PI, expecting ?>")
          exit
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
          exit
        endif

      case (ST_COMMENT_END_2)
        print*,'ST_COMMENT_END_2'
        if (str_vs(fx%token)=='>') then
          if (present(comment_handler)) &
            call comment_handler(str_vs(fx%name))
          deallocate(fx%name)
          if (fx%context==CTXT_IN_CONTENT) then
            fx%state = ST_CHAR_IN_CONTENT
          elseif (fx%context==CTXT_IN_DTD) then
            fx%state = ST_INT_SUBSET
          else
            fx%state = ST_MISC
            fx%whitespace = WS_DISCARD
          endif
        else
          call add_parse_error(fx, "Expecting > after -- in comment")
          exit
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
          if (present(startCdata_handler)) &
            call startCdata_handler
          if (size(fx%name)>0) then
            if (present(characters_handler)) &
              call characters_handler(str_vs(fx%name))
          endif
          if (present(endCdata_handler)) &
            call endCdata_handler
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
                exit
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
                exit
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
            fx%whitespace = WS_DISCARD
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
            end_document_handler,            &
            startDTD_handler,                &
            endDTD_handler,                  &
            startCdata_handler,              &
            endCdata_handler,                &
            unparsedEntityDecl_handler,      &
            notationDecl_handler)
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
        if (str_vs(fx%token) == '>') then
          call close_tag
          if (fx%error) goto 100
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

      case (ST_IN_DTD)
        print*,'ST_IN_DTD'
        ! check token is name
        fx%root_element => fx%token
        nullify(fx%token)
        fx%whitespace = WS_MANDATORY
        fx%state = ST_DTD_NAME
        
      case (ST_DTD_NAME)
        print*, 'ST_DTD_NAME'
        if (str_vs(fx%token)=='SYSTEM') then
          fx%state = ST_DTD_SYSTEM
        elseif (str_vs(fx%token)=='PUBLIC') then
          fx%state = ST_DTD_PUBLIC
        elseif (str_vs(fx%token)=='[') then
          fx%whitespace = WS_DISCARD
          fx%state = ST_INT_SUBSET
        elseif (str_vs(fx%token)=='>') then
          fx%context = CTXT_BEFORE_CONTENT
          fx%state = ST_MISC
        else
          call add_parse_error(fx, "Internal error: unexpected token")
        endif
         
      case (ST_DTD_PUBLIC)
        print*, 'ST_DTD_PUBLIC'
        if (checkPubId(str_vs(fx%token))) then
          fx%systemId => fx%token
          nullify(fx%token)
          fx%state = ST_DTD_SYSTEM
        else
          call add_parse_error(fx, "Invalid document system id")
        endif       

      case (ST_DTD_SYSTEM)
        print*, 'ST_DTD_SYSTEM'
        if (checkSystemId(str_vs(fx%token))) then
          fx%systemId => fx%token
          nullify(fx%token)
          fx%state = ST_DTD_DECL
        else
          call add_parse_error(fx, "Invalid document system id")
        endif

      case (ST_DTD_DECL)
        print*, 'ST_DTD_DECL'
        if (str_vs(fx%token)=='[') then
          if (present(startDTD_handler)) then
            if (associated(fx%publicId)) then
              call startDTD_handler(str_vs(fx%name), publicId=str_vs(fx%publicId), systemId=str_vs(fx%systemId))
            else
              call startDTD_handler(str_vs(fx%name), systemId=str_vs(fx%systemId))
            endif
          endif
          fx%whitespace = WS_DISCARD
          fx%state = ST_INT_SUBSET
        elseif (str_vs(fx%token)=='>') then
          if (present(startDTD_handler)) then
            if (associated(fx%publicId)) then
              call startDTD_handler(str_vs(fx%name), publicId=str_vs(fx%publicId), systemId=str_vs(fx%systemId))
            else
              call startDTD_handler(str_vs(fx%name), systemId=str_vs(fx%systemId))
            endif
          endif
          if (present(endDTD_handler)) &
            call endDTD_handler
          fx%context = CTXT_BEFORE_CONTENT
          fx%state = ST_MISC
        else
          call add_parse_error(fx, "Internal error: unexpected token")
        endif

      case (ST_INT_SUBSET)
        print*, 'ST_INT_SUBSET'
        if (str_vs(fx%token)==']') then
          fx%state = ST_CLOSE_DTD
        elseif (str_vs(fx%token)=='%') then
          call FoX_Error("PE reference unimplemented")
        elseif (str_vs(fx%token)=='<?') then
          fx%state = ST_START_PI
        elseif (str_vs(fx%token)=='<!') then
          fx%state = ST_BANG_TAG
          fx%whitespace = WS_FORBIDDEN
        else
          call add_parse_error(fx, "Unexpected token in internal subset")
        endif

      case (ST_DTD_ATTLIST)
        print*, 'ST_DTD_ATTLIST'
        ! check is name
        fx%name => fx%token
        nullify(fx%token)
        fx%state = ST_DTD_ATTLIST_CONTENTS

      case (ST_DTD_ATTLIST_CONTENTS)
        !token is everything up to >
        call parse_attlist
        if (fx%error) goto 100
        fx%state = ST_DTD_ATTLIST_END

      case (ST_DTD_ATTLIST_END)
        if (str_vs(fx%token)=='>') then
          ! register contents ...
          deallocate(fx%name)
          fx%state = ST_INT_SUBSET
          fx%whitespace = WS_DISCARD
        else
          call add_parse_error(fx, "Unexpected token in ATTLIST")
        endif

      case (ST_DTD_ELEMENT)
        print*, 'ST_DTD_ELEMENT'
        ! check is name
        fx%name => fx%token
        nullify(fx%token)
        fx%state = ST_DTD_ELEMENT_CONTENTS
        call FoX_Error("element unimplemented")

      case (ST_DTD_ELEMENT_CONTENTS)
        !token is everything up to >
        call parse_element
        if (fx%error) goto 100
        fx%state = ST_DTD_ELEMENT_END
        
      case (ST_DTD_ELEMENT_END)
        if (str_vs(fx%token)=='>') then
          ! register contents ...
          deallocate(fx%name)
          fx%state = ST_INT_SUBSET
          fx%whitespace = WS_DISCARD
        else
          call add_parse_error(fx, "Unexpected token in ELEMENT")
        endif
        
      case (ST_DTD_ENTITY)
        print*, 'ST_DTD_ENTITY'
        if (str_vs(fx%token) == '%') then
          fx%pe = .true.
          ! this will be a PE
          fx%state = ST_DTD_ENTITY_PE
          fx%name => fx%token
          nullify(fx%token)
        else
          fx%pe = .false.
          fx%name => fx%token
          nullify(fx%token)
          ! FIXME check it's a name
          fx%state = ST_DTD_ENTITY_ID
        endif
        
      case (ST_DTD_ENTITY_PE)
        print*, 'ST_DTD_ENTITY_PE'
        !check name is name FIXME
        fx%name => fx%token
        nullify(fx%token)
        fx%state = ST_DTD_ENTITY_ID

      case (ST_DTD_ENTITY_ID)
        print*, 'ST_DTD_ENTITY_ID'
        if (str_vs(fx%token) == 'PUBLIC') then
          fx%state = ST_DTD_ENTITY_PUBLIC
        elseif (str_vs(fx%token) == 'SYSTEM') then
          fx%state = ST_DTD_ENTITY_SYSTEM
        elseif (fx%token(1)=="'".or.fx%token(1)=='"') then
          fx%attname => fx%token
          nullify(fx%token)
          fx%state = ST_DTD_ENTITY_END
          fx%whitespace = WS_DISCARD
        else
          call add_parse_error(fx, "Unexpected token in ENTITY")
        endif
          
      case (ST_DTD_ENTITY_PUBLIC)
        print*, 'ST_DTD_ENTITY_PUBLIC'
        if (checkPubId(str_vs(fx%token))) then
          fx%publicId => fx%token
          nullify(fx%token)
          fx%state = ST_DTD_ENTITY_SYSTEM
        else
          call add_parse_error(fx, "Invalid PUBLIC id in ENTITY")
        endif
        
      case (ST_DTD_ENTITY_SYSTEM)
        print*, 'ST_DTD_ENTITY_SYSTEM'
        if (checkSystemId(str_vs(fx%token))) then
          fx%systemId => fx%token
          nullify(fx%token)
          fx%state = ST_DTD_ENTITY_NDATA
        else
          call add_parse_error(fx, "Invalid SYSTEM id in ENTITY")
        endif

      case (ST_DTD_ENTITY_NDATA)
        print*, 'ST_DTD_ENTITY_NDATA'
        if (str_vs(fx%token)=='>') then
          call add_entity
          fx%state = ST_INT_SUBSET
          fx%whitespace = WS_DISCARD
        elseif (str_vs(fx%token)=='NDATA') then
          if (fx%pe) then
            call add_parse_error(fx, "Parameter entity cannot have NDATA declaration"); goto 100
          endif
          fx%state = ST_DTD_ENTITY_NDATA_VALUE
        else
          call add_parse_error(fx, "Unexpected token in ENTITY")
        endif

      case (ST_DTD_ENTITY_NDATA_VALUE)
        print*, 'ST_DTD_ENTITY_NDATA_VALUE'
        !check is a name and exists in notationlist
        if(notation_exists(fx%nlist, str_vs(fx%token))) then
          fx%Ndata => fx%token
          nullify(fx%token)
          fx%state = ST_DTD_ENTITY_END
          fx%whitespace = WS_DISCARD
          ! add entity
        else
          call add_parse_error(fx, "Attempt to use undeclared notation")
        endif

      case (ST_DTD_ENTITY_END)
        print*, 'ST_DTD_ENTITY_END'
        if (str_vs(fx%token)=='>') then
          call add_entity
          fx%state = ST_INT_SUBSET
        else
          call add_parse_error(fx, "Unexpected token at end of ENTITY")
        endif

      case (ST_DTD_NOTATION)
        print*, 'ST_DTD_NOTATION'
        ! check name is name FIXMe
        fx%name => fx%token
        nullify(fx%token)
        fx%state = ST_DTD_NOTATION_ID

      case (ST_DTD_NOTATION_ID)
        print*,'ST_DTD_NOTATION_ID'
        if (str_vs(fx%token)=='SYSTEM') then
          fx%state = ST_DTD_NOTATION_SYSTEM
        elseif (str_vs(fx%token)=='PUBLIC') then
          fx%state = ST_DTD_NOTATION_PUBLIC
        else
          call add_parse_error(fx, "Unexpected token after NOTATION")
        endif

      case (ST_DTD_NOTATION_SYSTEM)
        print*,'ST_DTD_NOTATION_SYSTEM'
        if (checkSystemId(str_vs(fx%token))) then
          fx%systemId => fx%token
          nullify(fx%token)
          fx%state = ST_DTD_NOTATION_END
        else
          call add_parse_error(fx, "Invalid SYSTEM id in NOTATION")
        endif
        
      case (ST_DTD_NOTATION_PUBLIC)
        print*,'ST_DTD_NOTATION_PUBLIC'
        if (checkPubId(str_vs(fx%token))) then
          fx%publicId => fx%token
          nullify(fx%token)
          fx%state = ST_DTD_NOTATION_PUBLIC_2
        else
          call add_parse_error(fx, "Invalid PUBLIC id in NOTATION")
        endif
        
      case (ST_DTD_NOTATION_PUBLIC_2)
        print*,'ST_DTD_NOTATION_PUBLIC_2'
        if (str_vs(fx%token)=='>') then
          if (notation_exists(fx%nlist, str_vs(fx%name))) then
            call add_parse_error(fx, "Two notations share the same Name")
            exit
          endif
          print*,'ADDED NOTATION: ', str_vs(fx%name)
          call add_notation(fx%nlist, str_vs(fx%name), &
            publicId=str_vs(fx%publicId))
          print*,'NOTATION EXISTS: ', notation_exists(fx%nlist,str_vs(fx%name))
          if (present(notationDecl_handler)) &
            call notationDecl_handler(str_vs(fx%name), publicId=str_vs(fx%publicId)) 
          deallocate(fx%name)
          deallocate(fx%publicId)
          fx%state = ST_INT_SUBSET
        elseif (checkSystemId(str_vs(fx%token))) then
            fx%systemId => fx%token
            nullify(fx%token)
            fx%state = ST_DTD_NOTATION_END
        else
          call add_parse_error(fx, "Invalid SYSTEM id in NOTATION")
        endif
        
      case (ST_DTD_NOTATION_END)
        print*,'ST_DTD_NOTATION_END'
        if (str_vs(fx%token)=='>') then
          if (notation_exists(fx%nlist, str_vs(fx%name))) then
            call add_parse_error(fx, "Two notations share the same Name")
            exit
          endif
          print*,'ADDED NOTATION: ', str_vs(fx%name)
          if (associated(fx%publicId)) then
            call add_notation(fx%nlist, str_vs(fx%name), &
              publicId=str_vs(fx%publicId), systemId=str_vs(fx%systemId))
            if (present(notationDecl_handler)) &
              call notationDecl_handler(str_vs(fx%name), &
              publicId=str_vs(fx%publicId), systemId=str_vs(fx%systemId)) 
            deallocate(fx%publicId)
            deallocate(fx%systemId)
          else
            call add_notation(fx%nlist, str_vs(fx%name), &
              systemId=str_vs(fx%systemId))
            if (present(notationDecl_handler)) &
              call notationDecl_handler(str_vs(fx%name), &
              systemId=str_vs(fx%systemId)) 
            deallocate(fx%systemId)
          endif
          print*,'NOTATION EXISTS: ', notation_exists(fx%nlist,str_vs(fx%name))
          deallocate(fx%name)
          fx%state = ST_INT_SUBSET
          fx%whitespace = WS_DISCARD
        else
          call add_parse_error(fx, "Unexpected token in NOTATION")
        endif

      case (ST_CLOSE_DTD)
        print*, 'ST_CLOSE_DTD'
        ! token must be '>'
        if (present(endDTD_handler)) &
          call endDTD_handler
        fx%state = ST_MISC
        fx%context = CTXT_BEFORE_CONTENT
            
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
      print*,'an error'
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

      subroutine add_entity
        !Parameter or General Entity?
        if (fx%pe) then
          !Does entity with this name exist?
          if (.not.existing_entity(fx%pe_list, str_vs(fx%name))) then
            ! Internal or external?
            if (associated(fx%attname)) then ! it's internal
              call add_internal_entity(fx%pe_list, str_vs(fx%name), &
                str_vs(fx%attname))
            else ! PE can't have Ndata declaration
              if (associated(fx%publicId)) then
                call add_external_entity(fx%pe_list, str_vs(fx%name), &
                  str_vs(fx%systemId), publicId=str_vs(fx%publicId))
              else
                call add_external_entity(fx%pe_list, str_vs(fx%name), &
                  str_vs(fx%systemId))
              endif
            endif
            ! else we ignore it
          endif
        else !It's a general entity
          if (.not.existing_entity(fx%ge_list, str_vs(fx%name))) then
            ! Internal or external?
            if (associated(fx%attname)) then ! it's internal
              call add_internal_entity(fx%ge_list, str_vs(fx%name), &
                str_vs(fx%attname))
            else
              if (associated(fx%publicId).and.associated(fx%Ndata)) then
                call add_external_entity(fx%ge_list, str_vs(fx%name), &
                  str_vs(fx%systemId), publicId=str_vs(fx%publicId), &
                  notation=str_vs(fx%Ndata))
                if (present(unparsedEntityDecl_handler)) &
                  call unparsedEntityDecl_Handler(str_vs(fx%name), &
                  str_vs(fx%systemId), str_vs(fx%publicId), &
                  str_vs(fx%Ndata))
              elseif (associated(fx%Ndata)) then
                call add_external_entity(fx%ge_list, str_vs(fx%name), &
                  str_vs(fx%systemId), notation=str_vs(fx%Ndata))
                if (present(unparsedEntityDecl_handler)) &
                  call unparsedEntityDecl_Handler(str_vs(fx%name), &
                  systemId=str_vs(fx%systemId), notation=str_vs(fx%Ndata))
              elseif (associated(fx%publicId)) then
                call add_external_entity(fx%ge_list, str_vs(fx%name), &
                  str_vs(fx%systemId), publicId=str_vs(fx%publicId))
              else
                call add_external_entity(fx%ge_list, str_vs(fx%name), &
                  str_vs(fx%systemId))
              endif
            endif
          endif
        endif
        deallocate(fx%name)
        if (associated(fx%attname)) deallocate(fx%attname)
        if (associated(fx%systemId)) deallocate(fx%systemId)
        if (associated(fx%publicId)) deallocate(fx%publicId)
        if (associated(fx%Ndata)) deallocate(fx%Ndata)
      end subroutine add_entity

      subroutine parse_attlist
      end subroutine parse_attlist
      
      subroutine parse_element
      end subroutine parse_element

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

! public-facing SAX subroutine.

module m_sax_operate

  use m_sax_reader, only: open_file, close_file
  use m_sax_parser, only: sax_parser_init, sax_parser_destroy, sax_parse
  use m_sax_types, only: xml_t

  implicit none
  private

  public :: xml_t
  public :: open_xml_file
  public :: open_xml_string
  public :: close_xml_t
  public :: sax_parse_go

contains

  subroutine open_xml_file(xt, file, iostat, lun)
    type(xml_t), intent(out) :: xt
    character(len=*), intent(in) :: file
    integer, intent(out) :: iostat
    integer, intent(in), optional :: lun

    call open_file(xt%fb, file=file, iostat=iostat, lun=lun)
    call sax_parser_init(xt%fx)

  end subroutine open_xml_file

  subroutine open_xml_string(xt, string)
    type(xml_t), intent(out) :: xt
    character(len=*), intent(in) :: string

    integer :: iostat

    call open_file(xt%fb, string=string, iostat=iostat)
    call sax_parser_init(xt%fx)

  end subroutine open_xml_string

  subroutine close_xml_t(xt)
    type(xml_t), intent(inout) :: xt

    call close_file(xt%fb)
    call sax_parser_destroy(xt%fx)
  end subroutine close_xml_t

  subroutine sax_parse_go(xt,        &
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
    startDTD_handler,                     &
    endDTD_handler,                       &
    startCdata_handler,                   &
    endCdata_handler,                     &
    unparsedEntityDecl_handler,           &
    internalEntityDecl_handler,           &
    externalEntityDecl_handler,           &
    notationDecl_handler,                 &
    skippedEntity_handler,                &
    elementDecl_handler,                  &
    attributeDecl_handler)

    type(xml_t), intent(inout) :: xt
    optional :: begin_element_handler
    optional :: end_element_handler
    optional :: start_prefix_handler
    optional :: end_prefix_handler
    optional :: characters_handler
    optional :: comment_handler
    optional :: processing_instruction_handler
    optional :: error_handler
    optional :: start_document_handler
    optional :: end_document_handler
    optional :: startDTD_handler
    optional :: endDTD_handler
    optional :: startCdata_handler
    optional :: endCdata_handler
    optional :: notationDecl_handler
    optional :: unparsedEntityDecl_handler
    optional :: internalEntityDecl_handler
    optional :: externalEntityDecl_handler
    optional :: skippedEntity_handler
    optional :: elementDecl_handler
    optional :: attributeDecl_handler

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

      subroutine internalEntityDecl_handler(name, value)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: value
      end subroutine internalEntityDecl_handler

      subroutine externalEntityDecl_handler(name, publicId, systemId)
        character(len=*), intent(in) :: name
        character(len=*), optional, intent(in) :: publicId
        character(len=*), intent(in) :: systemId
      end subroutine externalEntityDecl_handler

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

      subroutine skippedEntity_handler(name)
        character(len=*), intent(in) :: name
      end subroutine skippedEntity_handler

      subroutine elementDecl_handler(name, model)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: model
      end subroutine elementDecl_handler

      subroutine attributeDecl_handler(eName, aName, type, mode, value)
        character(len=*), intent(in) :: eName
        character(len=*), intent(in) :: aName
        character(len=*), intent(in) :: type
        character(len=*), intent(in), optional :: mode
        character(len=*), intent(in), optional :: value
      end subroutine attributeDecl_handler
    end interface

    ! check xt is initialized

    call sax_parse(xt%fx, xt%fb,     &
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
    startDTD_handler,                     &
    endDTD_handler,                       &
    startCdata_handler,                   &
    endCdata_handler,                     &
    internalEntityDecl_handler,           &
    externalEntityDecl_handler,           &
    unparsedEntityDecl_handler,           &
    notationDecl_handler,                 &
    skippedEntity_handler,                &
    elementDecl_handler,              &
    attributeDecl_handler)

  end subroutine sax_parse_go

end module m_sax_operate

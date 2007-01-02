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

  subroutine sax_parse_go(xt,      &
    characters_handler,            &
    endDocument_handler,           &
    endElement_handler,            &
    endPrefixMapping_handler,      &
    ignorableWhitespace_handler,   &
    processingInstruction_handler, &
    ! setDocumentLocator
    skippedEntity_handler,         &
    startDocument_handler,         & 
    startElement_handler,          &
    startPrefixMapping_handler,    &
    notationDecl_handler,          &
    unparsedEntityDecl_handler,    &
    error_handler,                 &
    ! fatalError
    ! warning
    attributeDecl_handler,         &
    elementDecl_handler,           &
    externalEntityDecl_handler,    &
    internalEntityDecl_handler,    &
    comment_handler,               &
    endCdata_handler,              &
    endDTD_handler,                &
    endEntity_handler,             &
    startCdata_handler,            &
    startDTD_handler,              &
    startEntity_handler)

    type(xml_t), intent(inout) :: xt
    optional :: characters_handler
    optional :: endDocument_handler
    optional :: endElement_handler
    optional :: endPrefixMapping_handler
    optional :: ignorableWhitespace_handler
    optional :: startElement_handler
    optional :: startDocument_handler
    optional :: startPrefixMapping_handler
    optional :: comment_handler
    optional :: processingInstruction_handler
    optional :: error_handler
    optional :: startDTD_handler
    optional :: endDTD_handler
    optional :: startCdata_handler
    optional :: endCdata_handler
    optional :: internalEntityDecl_handler
    optional :: externalEntityDecl_handler
    optional :: unparsedEntityDecl_handler
    optional :: notationDecl_handler
    optional :: skippedEntity_handler
    optional :: elementDecl_handler
    optional :: attributeDecl_handler
    optional :: startEntity_handler
    optional :: endEntity_handler

    interface
      subroutine startElement_handler(namespaceURI, localName, name, attributes)
        use FoX_common
        character(len=*), intent(in)     :: namespaceUri
        character(len=*), intent(in)     :: localName
        character(len=*), intent(in)     :: name
        type(dictionary_t), intent(in)   :: attributes
      end subroutine startElement_handler

      subroutine endElement_handler(namespaceURI, localName, name)
        character(len=*), intent(in)     :: namespaceURI
        character(len=*), intent(in)     :: localName
        character(len=*), intent(in)     :: name
      end subroutine endElement_handler

      subroutine startPrefixMapping_handler(namespaceURI, prefix)
        character(len=*), intent(in) :: namespaceURI
        character(len=*), intent(in) :: prefix
      end subroutine startPrefixMapping_handler

      subroutine endPrefixMapping_handler(prefix)
        character(len=*), intent(in) :: prefix
      end subroutine endPrefixMapping_handler

      subroutine characters_handler(chunk)
        character(len=*), intent(in) :: chunk
      end subroutine characters_handler

      subroutine comment_handler(comment)
        character(len=*), intent(in) :: comment
      end subroutine comment_handler

      subroutine processingInstruction_handler(name, content)
        character(len=*), intent(in)     :: name
        character(len=*), intent(in)     :: content
      end subroutine processingInstruction_handler

      subroutine error_handler(msg)
        character(len=*), intent(in)     :: msg
      end subroutine error_handler

      subroutine startDocument_handler()   
      end subroutine startDocument_handler

      subroutine endDocument_handler()     
      end subroutine endDocument_handler

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
        character(len=*), optional, intent(in) :: systemId
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

      subroutine ignorableWhitespace_handler(chars)
        character(len=*), intent(in) :: chars
      end subroutine ignorableWhitespace_handler

      subroutine startEntity_handler(name)
        character(len=*), intent(in) :: name
      end subroutine startEntity_handler

      subroutine endEntity_handler(name)
        character(len=*), intent(in) :: name
      end subroutine endEntity_handler
    end interface

    ! check xt is initialized

    call sax_parse(xt%fx, xt%fb,     &
      characters_handler,            &
      endDocument_handler,           &
      endElement_handler,            &
      endPrefixMapping_handler,      &
      ignorableWhitespace_handler,   &
      processingInstruction_handler, &
      ! setDocumentLocator
      skippedEntity_handler,         &
      startDocument_handler,         & 
      startElement_handler,          &
      startPrefixMapping_handler,    &
      notationDecl_handler,          &
      unparsedEntityDecl_handler,    &
      error_handler,                 &
      ! fatalError
      ! warning
      attributeDecl_handler,         &
      elementDecl_handler,           &
      externalEntityDecl_handler,    &
      internalEntityDecl_handler,    &
      comment_handler,               &
      endCdata_handler,              &
      endDTD_handler,                &
      endEntity_handler,             &
      startCdata_handler,            &
      startDTD_handler,              &
      startEntity_handler)

  end subroutine sax_parse_go

end module m_sax_operate

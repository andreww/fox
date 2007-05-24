module FoX_dom

  use m_dom_types
  use m_dom_namednodemap
  use m_dom_nodelist
  use m_dom_attribute
  use m_dom_document
  use m_dom_node
  use m_dom_element
  use m_dom_text
  use m_dom_parse
  use m_dom_utils
  
  use m_strings

  private

  !We make public only features from the DOM Core Level 2,
  !plus a couple of our own devising for input & output.
  !Some of the modules above expose symbols only
  !for internal library management, so must be kept private.

  ! DOM DOMString
  ! no

  ! DOM DOMTimestamp
  public :: DOMTimestamp

  ! DOM Exceptions
  public :: DOMException

  public :: INDEX_SIZE_ERR
  public :: DOMSTRING_SIZE_ERR
  public :: HIERARCHY_REQUEST_ERR
  public :: WRONG_DOCUMENT_ERR
  public :: INVALID_CHARACTER_ERR
  public :: NO_DATA_ALLOWED_ERR
  public :: NO_MODIFICATION_ALLOWED_ERR
  public :: NOT_FOUND_ERR
  public :: NOT_SUPPORTED_ERR
  public :: INUSE_ATTRIBUTE_ERR
  public :: INVALID_STATE_ERR
  public :: SYNTAX_ERR
  public :: INVALID_MODIFICATION_ERR
  public :: NAMESPACE_ERR
  public :: INVALID_ACCESS_ERR

  ! DOM Implementation
  public :: hasFeature
  public :: createDocumentType
  public :: createDocument

  ! DOM Document
  public :: documentType
  public :: createDocumentFragment
  public :: createTextNode
  public :: createComment
  public :: createCDATASection
  public :: createProcessingInstruction
  public :: createAttribute
  public :: createEntityReference
  public :: getElementsByTagName
  public :: importNode
  public :: createElementNS
  public :: createAttributeNS
  public :: getElementByTagNameNS
  public :: getElementById

  ! DOM Node
  public :: nodeType

  public :: ELEMENT_NODE
  public :: ATTRIBUTE_NODE
  public :: TEXT_NODE
  public :: CDATA_SECTION_NODE
  public :: ENTITY_REFERENCE_NODE
  public :: ENTITY_NODE
  public :: PROCESSING_INSTRUCTION_NODE
  public :: COMMENT_NODE
  public :: DOCUMENT_NODE
  public :: DOCUMENT_TYPE_NODE
  public :: DOCUMENT_FRAGMENT_NODE
  public :: NOTATION_NODE

  public :: insertBefore
  public :: replaceChild
  public :: removeChild
  public :: appendChild
  public :: hasChildNodes
  public :: cloneNode
  public :: normalize
  public :: isSupported
  public :: hasAttributes

  ! DOM NodeList
  public :: nodeListType
  
  public :: item

  ! DOM NamedNodeMap
  public :: namedNodeMapType
  
  public :: getNamedItem
  public :: setNamedItem
  public :: removeNamedItem
!  public :: item
  public :: getNamedItemNS
  public :: setNamedItemNS
  public :: removeNamedItemNS

  ! DOM CharacterData
  ! NB We use the native Fortran string type here
  ! rather than inventing a DOM String, thus no
  ! string type to make public
  public :: substringData
  public :: appendData
  public :: insertData
  public :: deleteData
  public :: replaceData

  ! DOM Attr
  public :: attrType

  ! DOM Element
  public :: getAttribute
  public :: setAttribute
  public :: removeAttribute
  public :: getAttributeNode
  public :: setAttributeNode
  public :: removeAttributeNode
!  public :: getElementsByTagName
  public :: getAttributeNS
  public :: setAttributeNS
  public :: removeAttributeNS
  public :: getAttributeNodeNS
  public :: setAttributeNodeNS
  public :: getElementsByTagNameNS
  public :: hasAttribute
  public :: hasAttributeNS

  !DOM Text
  public :: splitText

  !DOM CData

  !DOM DocumentType
!  public :: documentType
  
  !DOM Notation
  public :: notation

  !DOM ENtity
  public :: Entity

  !DOM EntityReference

  !DOM ProcessingInstruction

end module FoX_dom

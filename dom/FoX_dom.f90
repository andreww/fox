module FoX_dom

  use m_dom_dom
  use m_dom_error
  use m_dom_parse
  use m_dom_utils

  implicit none
  private

  !We make public only features from the DOM Core Level 2,
  !plus a couple of our own devising for input & output.
  !Some of the modules above expose symbols only
  !for internal library management, so must be kept private.

  public :: DOMImplementation
  public :: Node
  public :: NodeList
  public :: NamedNodeMap

  ! DOM DOMString
  ! no

  ! DOM DOMTimestamp
!  public :: DOMTimestamp

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
  public :: destroyDocument
  public :: destroyDocumentFragment

  ! DOM Document
  public :: getDocumentElement
  public :: createDocumentFragment
  public :: createElement
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
  public :: getElementsByTagNameNS
  public :: getElementById
  public :: destroyNode

  ! DOM Node
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

  public :: getNodeName
  public :: getNodeValue
  public :: setNodeValue
  public :: getNodeType
  public :: getFirstChild
  public :: getLastChild
  public :: getAttributes
  public :: getNextSibling
  public :: getPreviousSibling
  public :: getParentNode
  public :: getChildNodes
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
  public :: item
  public :: append
  public :: destroyNodeList

  ! DOM NamedNodeMap
  public :: getLength
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
! public :: getData
! public :: setData
  public :: substringData
  public :: appendData
  public :: insertData
  public :: deleteData
  public :: replaceData

  ! DOM Attr
!  public :: getName
  public :: getSpecified
  public :: getValue
  public :: setValue
  public :: getOwnerElement

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
!  public :: getElementsByTagNameNS
  public :: hasAttribute
  public :: hasAttributeNS

  !DOM Text
  public :: splitText

  !DOM CData
! public :: getData
! public :: setData

  !DOM DocumentType
  public :: getEntities
  public :: getNotations
  
  !DOM Notation

  !DOM Entity
  public :: getNotationName

  !DOM EntityReference

  !DOM ProcessingInstruction
!  public :: getData
!  public :: setData
  public :: getTarget

  !DOM common
  public :: getData
  public :: setData
  public :: getName
  public :: getPublicId
  public :: getSystemId

  ! FoX-only interfaces
  public :: parsefile
  public :: dumpTree
  public :: serialize

  ! FIXME rename this
  public :: is_in_error
  public :: getCode

end module FoX_dom

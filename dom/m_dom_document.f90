module m_dom_document

use m_dom_types, only: fnode,  createNode, destroyNode, &
     DOCUMENT_NODE, DOCUMENT_FRAGMENT_NODE, &
     TEXT_NODE, ATTRIBUTE_NODE, ELEMENT_NODE, &
     COMMENT_NODE, CDATA_SECTION_NODE
use m_dom_types, only : fDocumentType, fDocumentNode

use m_dom_node, only : getChildNodes

use m_dom_error, only : NOT_FOUND_ERR, dom_error

use m_strings, only: string, assignment(=)

implicit none

private

  !-------------------------------------------------------  
  ! METHODS FOR DOCUMENT NODES
  !-------------------------------------------------------   
  public :: createDocument
  public :: destroyDocument
  public :: createElement
  public :: createDocumentFragment
  public :: createTextNode
  public :: createComment
  public :: createCdataSection
!  public :: createProcessingInstruction
  public :: createAttribute
!  public :: createEntityReference
!  public :: getElementsByTagName
!  public :: importNode
  public :: createElementNS
  public :: createAttributeNS
!  public :: getElementsByTagNameNS
!  public :: getElementById 
CONTAINS

  !-----------------------------------------------------------
  ! METHODS FOR DOCUMENT NODES
  !-----------------------------------------------------------


  function createDocumentType(qualifiedName, publicID, systemID)
    type(fDocumentType), pointer :: createDocumentType
    type(string), intent(in) :: qualifiedName, publicID, systemID

    createDocumentType => null()
  end function createDocumentType

  subroutine createDocument(doc, main) 
    type(fDocumentNode), pointer :: doc
    type(fNode), pointer :: main
    type(fDocumentType), pointer :: docType

    type(fnode), pointer :: child
    
    allocate(doc)
    doc % doctype => null()

    child => main % firstChild
    do while (associated(child))
      if (child % nodeType == ELEMENT_NODE) then
        doc % documentElement => child
        exit
      endif
      child => child % nextSibling
    enddo
    if (associated(doc % documentElement)) then
      !if (parsefile % documentElement % namespaceURI == 'http://www.w3.org/1999/xhtml' ) then
      continue
    else
      call dom_error('createDocument', NOT_FOUND_ERR, "No documentElement found.")
    endif

  end subroutine createDocument

!FIXMETOHW I think I got documentNodes & head nodes mixed up here.
  subroutine destroyDocument(doc)
    type(fDocumentNode), pointer :: doc

    call destroyNode(doc % documentElement)
    deallocate(doc)

  end subroutine destroyDocument
    

!-------------------------------------------------------------------
  function createDocumentFragment(doc)
    type(fDocumentNode), pointer :: doc
    type(fnode), pointer :: createDocumentFragment
    
    createDocumentFragment => createNode()
    createDocumentFragment % nodeType = DOCUMENT_FRAGMENT_NODE
    createDocumentFragment % nodeName = "#document-fragment"
    createDocumentFragment % ownerDocument => doc
    
  end function createDocumentFragment
!-------------------------------------------------------------------
  function createTextNode(doc, data)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: data
    type(fnode), pointer :: createTextNode
    
    createTextNode => createNode()
    createTextNode % nodeType = TEXT_NODE
    createTextNode % nodeName = "#text"
    createTextNode % nodeValue = data
    createtextNode % ownerDocument => doc
    
  end function createTextNode

  !-----------------------------------------------------------

  function createAttribute(doc, name)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: name
    type(fnode), pointer :: createAttribute
  
    createAttribute => createNode()
    createAttribute % nodeName = name
    createAttribute % nodeType = ATTRIBUTE_NODE
    createAttribute % ownerDocument => doc
  
  end function createAttribute

  !-----------------------------------------------------------

  function createElement(doc, tagName)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: tagName
    type(fnode), pointer :: createElement
  
    createElement => createNode()
    createElement % nodeName = tagName
    createElement % nodeType = ELEMENT_NODE
    createElement % ownerDocument => doc
  
  end function createElement

  !-----------------------------------------------------------

  function createComment(doc, data)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: data
    type(fnode), pointer :: createComment
  
    createComment => createNode()
    createComment % nodeName = "#comment"
    createComment % nodeValue = data
    createComment % nodeType = COMMENT_NODE
    createComment % ownerDocument => doc
  
  end function createComment

  !-----------------------------------------------------------

  function createCdataSection(doc, data)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: data
    type(fnode), pointer :: createCdataSection
  
    createCdataSection => createNode()
    createCdataSection % nodeName = "#cdata-section"
    createCdataSection % nodeValue = data
    createCdataSection % nodeType = CDATA_SECTION_NODE
    createCdataSection % ownerDocument => doc
  
  end function createCdataSection

  function createElementNS(doc, namespaceURI, qualifiedName)
    type(fDocumentNode), pointer :: doc
    type(string), intent(in) :: namespaceURI, qualifiedName
    type(fnode), pointer :: createElementNS
  
    createElementNS => createNode()
    createElementNS % nodeName = qualifiedName
    createElementNS % nodeType = ELEMENT_NODE
    createElementNS % ownerDocument => doc
    createElementNS % namespaceURI = namespaceURI
  
  end function createElementNS
  
  function createAttributeNS(doc, namespaceURI,  qualifiedname)
    type(fDocumentNode), pointer :: doc
    type(string), intent(in) :: namespaceURI, qualifiedName
    type(fnode), pointer :: createAttributeNS
  
    createAttributeNS => createNode()
    createAttributeNS % nodeName = qualifiedname
    createAttributeNS % nodeType = ATTRIBUTE_NODE
    createAttributeNS % ownerDocument => doc
    createAttributeNS % namespaceURI = namespaceURI
    
  end function createAttributeNS


end module m_dom_document

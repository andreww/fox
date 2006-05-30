module m_dom_document

use m_dom_documenttype, only : fDocumentType
use m_dom_documenttype, only : createDocumentType, destroyDocumentType

use m_dom_types, only: fnode,  createNode, destroyNode, &
     DOCUMENT_NODE, DOCUMENT_FRAGMENT_NODE, &
     TEXT_NODE, ATTRIBUTE_NODE, ELEMENT_NODE, &
     COMMENT_NODE, CDATA_SECTION_NODE
use m_dom_types, only : fDocument

use m_dom_node, only : getChildNodes

use m_dom_error, only : NOT_FOUND_ERR, dom_error

use m_strings, only: string, assignment(=)

implicit none

private

! from DOMImplementation
  public :: createDocument
  public :: destroyDocument

! from DOMDocument
  public :: createElement
  public :: createDocumentFragment
  public :: createTextNode
  public :: createComment
  public :: createCdataSection
  public :: createProcessingInstruction
  public :: createAttribute
  public :: createEntityReference
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

  !This function is part of m_dom_implementation but has
  !been transplanted here for reasons of inheritance.

  
  function createDocument(namespaceURI, qualifiedName, docType) result(doc)
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: qualifiedName
    type(fDocumentType), pointer :: docType
    type(fDocument), pointer :: doc
    
    !DOM implementation ...
    doc%documentType => docType
    
  !   !FIXME
  end function createDocument

  subroutine attachDocumentElement(doc, main) 
    type(fDocument), pointer :: doc
    type(fNode), pointer :: main

    type(fnode), pointer :: child

    if (.not.associated(doc%docType)) then
       !FIXME error out
    endif

    if (.not.child % nodeType == ELEMENT_NODE) then
       !FIXME error out
    endif
    
    doc%documentElement => main

    !if (associated(doc % documentElement)) then
      !if (parsefile % documentElement % namespaceURI == 'http://www.w3.org/1999/xhtml' ) then
    !  continue
    !else
    !  call dom_error('createDocument', NOT_FOUND_ERR, "No documentElement found.")
   ! endif

  end subroutine createDocument

!FIXMETOHW I think I got documentNodes & head nodes mixed up here.
  subroutine destroyDocument(doc)
    type(fDocument), pointer :: doc
    
    call destroyNode(doc % documentElement)
    deallocate(doc)

  end subroutine destroyDocument

  function createElement(doc, tagName)
    type(fDocument), pointer :: doc
    character(len=*), intent(in) :: tagName
    type(fnode), pointer :: createElement
  
    createElement => createNode()
    createElement % nodeName = tagName
    createElement % nodeType = ELEMENT_NODE
    createElement % ownerDocument => doc
  
  end function createElement
    
  function createDocumentFragment(doc)
    type(fDocument), pointer :: doc
    type(fnode), pointer :: createDocumentFragment
    
    createDocumentFragment => createNode()
    createDocumentFragment % nodeType = DOCUMENT_FRAGMENT_NODE
    createDocumentFragment % nodeName = "#document-fragment"
    createDocumentFragment % ownerDocument => doc
    
  end function createDocumentFragment

  function createTextNode(doc, data)
    type(fDocument), pointer :: doc
    character(len=*), intent(in) :: data
    type(fnode), pointer :: createTextNode
    
    createTextNode => createNode()
    createTextNode % nodeType = TEXT_NODE
    createTextNode % nodeName = "#text"
    createTextNode % nodeValue = data
    createtextNode % ownerDocument => doc
    
  end function createTextNode

  function createComment(doc, data)
    type(fDocument), pointer :: doc
    character(len=*), intent(in) :: data
    type(fnode), pointer :: createComment
  
    createComment => createNode()
    createComment % nodeName = "#comment"
    createComment % nodeValue = data
    createComment % nodeType = COMMENT_NODE
    createComment % ownerDocument => doc
  
  end function createComment

  function createCdataSection(doc, data)
    type(fDocument), pointer :: doc
    character(len=*), intent(in) :: data
    type(fnode), pointer :: createCdataSection
  
    createCdataSection => createNode()
    createCdataSection % nodeName = "#cdata-section"
    createCdataSection % nodeValue = data
    createCdataSection % nodeType = CDATA_SECTION_NODE
    createCdataSection % ownerDocument => doc
  
  end function createCdataSection

  function createProcessingInstruction(doc, target, data)
    type(fDocument), pointer :: doc
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data

    createProcessingInstruction => createNode()
    createProcessingInstruction % nodeName = target
    createProcessingInstruction % nodeValue = data
    createProcessingInstruction % nodeType = PROCESSING_INSTRUCTION_NODE
    createProcesssingInstruction % ownerDocument => doc
    
  end function createProcessingInstruction

  function createAttribute(doc, name)
    type(fDocument), pointer :: doc
    character(len=*), intent(in) :: name
    type(fnode), pointer :: createAttribute
  
    createAttribute => createNode()
    createAttribute % nodeName = name
    createAttribute % nodeType = ATTRIBUTE_NODE
    createAttribute % ownerDocument => doc
  
  end function createAttribute

  function createEntityReference(doc, name)
    type(fDocument), pointer :: doc
    character(len=*), intent(in) :: name
    type(fnode), pointer :: createEntityReference  

    createEntityReference => createNode()
    createEntityReference % nodeName = name
    createAttribute % nodeType = ENTITY_REFERENCE_NODE
    createAttribute % ownerDocument => doc

  end function createEntityReference

  !where does this live? In somewhere that has namednodemaps ...
  !function getElementsByTagName(doc, tagname)
  !  type(fDocument), pointer :: doc
  !  character(len=*), intent(in) :: tagname!
  !
  !  blah
  !end function getElementsByTagName

  !function importNode(doc, importedNode, deep)
  !  type(fDocument), pointer :: doc
  !  type(fNode), pointer :: importedNode
  !  logical :: deep

    !FIXME
  !end function importNode

  function createElementNS(doc, namespaceURI, qualifiedName)
    type(fDocument), pointer :: doc
    type(string), intent(in) :: namespaceURI, qualifiedName
    type(fnode), pointer :: createElementNS
  
    createElementNS => createNode()
    createElementNS % nodeName = qualifiedName
    createElementNS % nodeType = ELEMENT_NODE
    createElementNS % ownerDocument => doc
    createElementNS % namespaceURI = namespaceURI
  
  end function createElementNS
  
  function createAttributeNS(doc, namespaceURI,  qualifiedname)
    type(fDocument), pointer :: doc
    type(string), intent(in) :: namespaceURI, qualifiedName
    type(fnode), pointer :: createAttributeNS
  
    createAttributeNS => createNode()
    createAttributeNS % nodeName = qualifiedname
    createAttributeNS % nodeType = ATTRIBUTE_NODE
    createAttributeNS % ownerDocument => doc
    createAttributeNS % namespaceURI = namespaceURI
    
  end function createAttributeNS

end module m_dom_document

module m_dom_document
!
!use m_dom_documenttype, only : fDocumentType
!use m_dom_documenttype, only : createDocumentType, destroyDocumentType

use m_dom_types, only: fnode,  createNode, destroyNode, &
  ELEMENT_NODE, ATTRIBUTE_NODE, TEXT_NODE, CDATA_SECTION_NODE, &
  ENTITY_REFERENCE_NODE, ENTITY_NODE, PROCESSING_INSTRUCTION_NODE, &
  COMMENT_NODE, DOCUMENT_NODE, DOCUMENT_TYPE_NODE, DOCUMENT_FRAGMENT_NODE, &
  NOTATION_NODE
use m_dom_types, only : fDocumentNode, fDocumentType

use m_dom_node, only : getChildNodes

use m_dom_error, only : NOT_FOUND_ERR, dom_error

use m_strings, only: string, assignment(=)

implicit none

private

! from DOMImplementation
!  public :: createDocument
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

  
!  function createDocument(namespaceURI, qualifiedName, docType) result(doc)
!    character(len=*), intent(in) :: namespaceURI
!    character(len=*), intent(in) :: qualifiedName
!    type(fDocumentType), pointer :: docType
!    type(fDocumentNode), pointer :: doc
!    
!    !DOM implementation ...
!    doc%doctype => docType
!    
!  !   !FIXME
!  end function createDocument

  subroutine attachDocumentElement(doc, main) 
    type(fDocumentNode), pointer :: doc
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

  end subroutine attachDocumentElement

!FIXMETOHW I think I got documentNodes & head nodes mixed up here.
  subroutine destroyDocument(doc)
    type(fDocumentNode), pointer :: doc
    
    call destroyNode(doc % documentElement)
    deallocate(doc)

  end subroutine destroyDocument

  function createElement(doc, tagName)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: tagName
    type(fnode), pointer :: createElement
  
    createElement => createNode()
    createElement % nodeName = tagName
    createElement % nodeType = ELEMENT_NODE
    createElement % ownerDocument => doc
  
  end function createElement
    
  function createDocumentFragment(doc)
    type(fDocumentNode), pointer :: doc
    type(fnode), pointer :: createDocumentFragment
    
    createDocumentFragment => createNode()
    createDocumentFragment % nodeType = DOCUMENT_FRAGMENT_NODE
    createDocumentFragment % nodeName = "#document-fragment"
    createDocumentFragment % ownerDocument => doc
    
  end function createDocumentFragment

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

  function createProcessingInstruction(doc, target, data)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data
    type(fnode), pointer :: createProcessingInstruction

    createProcessingInstruction => createNode()
    createProcessingInstruction % nodeName = target
    createProcessingInstruction % nodeValue = data
    createProcessingInstruction % nodeType = PROCESSING_INSTRUCTION_NODE
    createProcessingInstruction % ownerDocument => doc
    
  end function createProcessingInstruction

  function createAttribute(doc, name)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: name
    type(fnode), pointer :: createAttribute
  
    createAttribute => createNode()
    createAttribute % nodeName = name
    createAttribute % nodeType = ATTRIBUTE_NODE
    createAttribute % ownerDocument => doc
  
  end function createAttribute

  function createEntityReference(doc, name)
    type(fDocumentNode), pointer :: doc
    character(len=*), intent(in) :: name
    type(fnode), pointer :: createEntityReference  

    createEntityReference => createNode()
    createEntityReference % nodeName = name
    createEntityReference % nodeType = ENTITY_REFERENCE_NODE
    createEntityReference % ownerDocument => doc

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

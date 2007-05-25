module m_dom_document
!
!use m_dom_documenttype, only : fDocumentType
!use m_dom_documenttype, only : createDocumentType, destroyDocumentType
  use m_common_array_str, only: vs_str_alloc
  use m_common_namecheck, only: prefixOfQName, localPartOfQName
  
  use m_dom_types, only: Node, createNode, destroyNode, &
    ELEMENT_NODE, ATTRIBUTE_NODE, TEXT_NODE, CDATA_SECTION_NODE, &
    ENTITY_REFERENCE_NODE, ENTITY_NODE, PROCESSING_INSTRUCTION_NODE, &
    COMMENT_NODE, DOCUMENT_NODE, DOCUMENT_TYPE_NODE, DOCUMENT_FRAGMENT_NODE, &
    NOTATION_NODE
  
!  use m_dom_node, only : getChildNodes
  
  use m_dom_error, only : NOT_FOUND_ERR, dom_error
  
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
    type(Node), pointer :: doc
    type(Node), pointer :: main

    type(Node), pointer :: child

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
    type(Node), pointer :: doc
    
    call destroyNode(doc%documentElement)
    deallocate(doc)

  end subroutine destroyDocument

  function createElement(doc, tagName)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: tagName
    type(Node), pointer :: createElement
  
    createElement => createElementNS(doc, "", tagName)
  
  end function createElement
    
  function createDocumentFragment(doc)
    type(Node), pointer :: doc
    type(Node), pointer :: createDocumentFragment
    
    createDocumentFragment => createNode()
    createDocumentFragment%nodeType = DOCUMENT_FRAGMENT_NODE
    createDocumentFragment%nodeName => vs_str_alloc("#document-fragment")
    createDocumentFragment%ownerDocument => doc
    
  end function createDocumentFragment

  function createTextNode(doc, data)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: createTextNode
    
    createTextNode => createNode()
    createTextNode%nodeType = TEXT_NODE
    createTextNode%nodeName => vs_str_alloc("#text")
    createTextNode%nodeValue => vs_str_alloc(data)
    createTextNode%ownerDocument => doc
    createTextNode%data => createTextNode%nodeValue
    
  end function createTextNode

  function createComment(doc, data)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: createComment
  
    createComment => createNode()
    createComment%nodeName => vs_str_alloc("#comment")
    createComment%nodeValue => vs_str_alloc(data)
    createComment%nodeType = COMMENT_NODE
    createComment%ownerDocument => doc
    createComment%data => createComment%data
  
  end function createComment

  function createCdataSection(doc, data)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: createCdataSection
  
    createCdataSection => createNode()
    createCdataSection%nodeName => vs_str_alloc("#cdata-section")
    createCdataSection%nodeValue => vs_str_alloc(data)
    createCdataSection%nodeType = CDATA_SECTION_NODE
    createCdataSection%ownerDocument => doc
  
  end function createCdataSection

  function createProcessingInstruction(doc, target, data)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data
    type(Node), pointer :: createProcessingInstruction

    createProcessingInstruction => createNode()
    createProcessingInstruction%nodeName => vs_str_alloc(target)
    createProcessingInstruction%nodeValue => vs_str_alloc(data)
    createProcessingInstruction%nodeType = PROCESSING_INSTRUCTION_NODE
    createProcessingInstruction%ownerDocument => doc
    
  end function createProcessingInstruction

  function createAttribute(doc, name)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    type(Node), pointer :: createAttribute
  
    createAttribute => createAttributeNS(doc, name, "")
  
  end function createAttribute

  function createEntityReference(doc, name)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    type(Node), pointer :: createEntityReference  

    createEntityReference => createNode()
    createEntityReference%nodeName => vs_str_alloc(name)
    createEntityReference%nodeType = ENTITY_REFERENCE_NODE
    createEntityReference%ownerDocument => doc

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
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: createElementNS
  
    createElementNS => createNode()
    createElementNS%nodeName => vs_str_alloc(qualifiedName)
    createElementNS%nodeType = ELEMENT_NODE
    createElementNS%ownerDocument => doc
    createElementNS%tagName => createElementNS%nodeName
    createElementNS%namespaceURI => vs_str_alloc(namespaceURI)
    createElementNS%prefix => vs_str_alloc(prefixOfQName(qualifiedname))
    createElementNS%localName => vs_str_alloc(localpartOfQName(qualifiedname))
  
  end function createElementNS
  
  function createAttributeNS(doc, namespaceURI,  qualifiedname)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: createAttributeNS
  
    createAttributeNS => createNode()
    createAttributeNS%nodeName => vs_str_alloc(qualifiedname)
    createAttributeNS%nodeType = ATTRIBUTE_NODE
    createAttributeNS%ownerDocument => doc
    createAttributeNS%namespaceURI => vs_str_alloc(namespaceURI)
    createAttributeNS%localname => vs_str_alloc(localPartofQName(qualifiedname))
    createAttributeNS%prefix => vs_str_alloc(PrefixofQName(qualifiedname))
    
  end function createAttributeNS

end module m_dom_document

module m_dom_document

  use m_common_array_str, only: vs_str_alloc
  use m_common_namecheck, only: prefixOfQName, localPartOfQName
  
  use m_dom_types, only: Node, createNode, destroyNode, &
    ELEMENT_NODE, ATTRIBUTE_NODE, TEXT_NODE, CDATA_SECTION_NODE, &
    ENTITY_REFERENCE_NODE, ENTITY_NODE, PROCESSING_INSTRUCTION_NODE, &
    COMMENT_NODE, DOCUMENT_NODE, DOCUMENT_TYPE_NODE, DOCUMENT_FRAGMENT_NODE, &
    NOTATION_NODE, DOCUMENT_FRAGMENT_NODE
  
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

  subroutine destroyDocument(doc)
    type(Node), pointer :: doc
    
    call destroyNode(doc%documentElement)
    deallocate(doc)

  end subroutine destroyDocument

  function createElement(doc, tagName) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: tagName
    type(Node), pointer :: np
  
    np => createElementNS(doc, "", tagName)
  
  end function createElement
    
  function createDocumentFragment(doc) result(np)
    type(Node), pointer :: doc
    type(Node), pointer :: np
    
    np => createNode(doc, DOCUMENT_FRAGMENT_NODE, "", "")
    
  end function createDocumentFragment

  function createTextNode(doc, data) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np
    
    np => createNode(doc, TEXT_NODE, "#text", data)

    ! No exceptions - but what if invalid chars? FIXME
    
  end function createTextNode

  function createComment(doc, data) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np
  
    np => createNode(doc, COMMENT_NODE, "#comment", data)

    ! No exceptions - but what if invalid chars or --? FIXME
  
  end function createComment

  function createCdataSection(doc, data) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np
  
    np => createNode(doc, CDATA_SECTION_NODE, "#text", data)

    ! No exceptions - but what if invalid chars or --? FIXME
  
  end function createCdataSection

  function createProcessingInstruction(doc, target, data) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    ! check target for legal chars else raise INVALID_CHARACTER_ERR
    np => createNode(doc, PROCESSING_INSTRUCTION_NODE, target, data)

  end function createProcessingInstruction

  function createAttribute(doc, name) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    type(Node), pointer :: np
  
    np => createAttributeNS(doc, name, "")
  
  end function createAttribute

  function createEntityReference(doc, name) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    np => createNode(doc, ENTITY_REFERENCE_NODE, name, "")

  end function createEntityReference

  function createElementNS(doc, namespaceURI, qualifiedName) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: np
  
    np => createNode(doc, ELEMENT_NODE, qualifiedName, "")
    np%namespaceURI => vs_str_alloc(namespaceURI)
    np%prefix => vs_str_alloc(prefixOfQName(qualifiedname))
    np%localName => vs_str_alloc(localpartOfQName(qualifiedname))

    ! FIXME not sure about the above ...
  
  end function createElementNS
  
  function createAttributeNS(doc, namespaceURI,  qualifiedname) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: np
  
    np => createNode(doc, ATTRIBUTE_NODE, qualifiedName, "")
    np%namespaceURI => vs_str_alloc(namespaceURI)
    np%localname => vs_str_alloc(localPartofQName(qualifiedname))
    np%prefix => vs_str_alloc(PrefixofQName(qualifiedname))
    
  end function createAttributeNS

end module m_dom_document

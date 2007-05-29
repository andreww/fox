module m_dom_document

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_namecheck, only: prefixOfQName, localPartOfQName
  
  use m_dom_types, only: DOMImplementation, Node, NodeList, createNode, destroyNode, &
    ELEMENT_NODE, ATTRIBUTE_NODE, TEXT_NODE, CDATA_SECTION_NODE, &
    ENTITY_REFERENCE_NODE, ENTITY_NODE, PROCESSING_INSTRUCTION_NODE, &
    COMMENT_NODE, DOCUMENT_NODE, DOCUMENT_TYPE_NODE, DOCUMENT_FRAGMENT_NODE, &
    NOTATION_NODE, DOCUMENT_FRAGMENT_NODE
  use m_dom_nodelist, only: append, pop_nl, destroyNodeList
  
  use m_dom_error, only : NOT_FOUND_ERR, dom_error
  
  implicit none
  private

  public :: createElement
  public :: createDocumentFragment
  public :: createTextNode
  public :: createComment
  public :: createCdataSection
  public :: createProcessingInstruction
  public :: createAttribute
  public :: createEntityReference
  public :: getElementsByTagName
  public :: importNode
  public :: createElementNS
  public :: createAttributeNS
  public :: getElementsByTagNameNS
  public :: getElementById 

contains

  ! Getters and setters:

  function getDocumentType(doc) result(np)
    type(Node), intent(in) :: doc
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
    
    np => doc%docType

  end function getDocumentType

  function getImplementation(doc) result(imp)
    type(Node), intent(in) :: doc
    type(DOMImplementation), pointer :: imp

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
    
    imp => doc%implementation
    
  end function getImplementation

  function getDocumentElement(doc) result(np)
    type(Node), intent(in) :: doc
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
    
    np => doc%documentElement

  end function getDocumentElement

  ! Methods

  function createElement(doc, tagName) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: tagName
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
  
    np => createElementNS(doc, "", tagName)
  
  end function createElement
    
  function createDocumentFragment(doc) result(np)
    type(Node), pointer :: doc
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
    
    np => createNode(doc, DOCUMENT_FRAGMENT_NODE, "", "")
    
  end function createDocumentFragment

  function createTextNode(doc, data) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
    
    np => createNode(doc, TEXT_NODE, "#text", data)

    ! No exceptions - but what if invalid chars? FIXME
    
  end function createTextNode

  function createComment(doc, data) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
  
    np => createNode(doc, COMMENT_NODE, "#comment", data)

    ! No exceptions - but what if invalid chars or --? FIXME
  
  end function createComment

  function createCdataSection(doc, data) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
  
    np => createNode(doc, CDATA_SECTION_NODE, "#text", data)

    ! No exceptions - but what if invalid chars or --? FIXME
  
  end function createCdataSection

  function createProcessingInstruction(doc, target, data) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

    ! check target for legal chars else raise INVALID_CHARACTER_ERR
    np => createNode(doc, PROCESSING_INSTRUCTION_NODE, target, data)

  end function createProcessingInstruction

  function createAttribute(doc, name) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
  
    np => createAttributeNS(doc, name, "")
  
  end function createAttribute

  function createEntityReference(doc, name) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

    np => createNode(doc, ENTITY_REFERENCE_NODE, name, "")

  end function createEntityReference

  function getElementsByTagName(doc, tagName) result(list)
    type(Node), intent(in) :: doc
    character(len=*), intent(in) :: tagName
    type(NodeList) :: list

    type(NodeList) :: np_stack
    type(Node), pointer :: np
    logical :: alreadyDone

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

    np => doc%documentElement

    if (.not.associated(np)) then
      ! FIXME internal eror
      continue
    endif

    ! Use iteration, not recursion, to save stack space.
    alreadyDone = .false.
    do
      print*, 'iterating ...', associated(np)
      if (alreadyDone) then
        np => pop_nl(np_stack)
        if (np_stack%length==0) then
          exit
        else
          if (associated(np%nextSibling)) then
            np => np%nextSibling
            alreadyDone = .false.
          else
            cycle
          endif
        endif
      endif
      if (np%nodeType/=ELEMENT_NODE) then
        if (associated(np%nextSibling)) then
          np => np%nextSibling
        else
          alreadyDone = .true.
        endif
        cycle
      endif
      if (str_vs(np%nodeName) == tagName) then
        call append(list, np)
      endif
      if (associated(np%firstChild)) then
        call append(np_stack, np)
        np => np%firstChild
        cycle
      else
        alreadyDone = .true.
      endif
    enddo
    call destroyNodeList(np_stack)

  end function getElementsByTagName

  function importNode(doc, arg, deep) result(np)
    type(Node), intent(in) :: doc
    type(Node), intent(in) :: arg
    logical, intent(in) :: deep
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

    continue

    ! FIXME logic
  end function importNode

  function createElementNS(doc, namespaceURI, qualifiedName) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: np
  
    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

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

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif
  
    np => createNode(doc, ATTRIBUTE_NODE, qualifiedName, "")
    np%namespaceURI => vs_str_alloc(namespaceURI)
    np%localname => vs_str_alloc(localPartofQName(qualifiedname))
    np%prefix => vs_str_alloc(PrefixofQName(qualifiedname))
    
  end function createAttributeNS

  function getElementsByTagNameNS(doc, namespaceURI, localName) result(list)
    type(Node), intent(in) :: doc
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(NodeList) :: list

    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

    continue

    ! FIXME logic
  end function getElementsByTagNameNS

  function getElementById(doc, elementId) result(np)
    type(Node), intent(in) :: doc
    character(len=*), intent(in) :: elementId
    type(NodeList), pointer :: np
    if (doc%nodeType/=DOCUMENT_NODE) then
      ! FIXME throw an error
      continue
    endif

    continue

    ! FIXME logic
    ! cannot fix until IDs implemented
  end function getElementById

end module m_dom_document

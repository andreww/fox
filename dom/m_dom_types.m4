TOHW_m_dom_publics(`
  integer, parameter ::     ELEMENT_NODE                   = 1
  integer, parameter ::     ATTRIBUTE_NODE                 = 2
  integer, parameter ::     TEXT_NODE                      = 3
  integer, parameter ::     CDATA_SECTION_NODE             = 4
  integer, parameter ::     ENTITY_REFERENCE_NODE          = 5
  integer, parameter ::     ENTITY_NODE                    = 6
  integer, parameter ::     PROCESSING_INSTRUCTION_NODE    = 7
  integer, parameter ::     COMMENT_NODE                   = 8
  integer, parameter ::     DOCUMENT_NODE                  = 9
  integer, parameter ::     DOCUMENT_TYPE_NODE             = 10
  integer, parameter ::     DOCUMENT_FRAGMENT_NODE         = 11
  integer, parameter ::     NOTATION_NODE                  = 12
  integer, parameter ::     XPATH_NAMESPACE_NODE           = 13


  type DOMImplementation
    private
    character(len=7) :: id = "FoX_DOM"
    logical :: FoX_checks = .true. ! Do extra checks not mandated by DOM
  end type DOMImplementation

  type ListNode
    private
    type(Node), pointer :: this => null()
  end type ListNode 

  type NodeList
    private
    character, pointer :: nodeName(:) => null() ! What was getByTagName run on?
    character, pointer :: localName(:) => null() ! What was getByTagNameNS run on?
    character, pointer :: namespaceURI(:) => null() ! What was getByTagNameNS run on?
    type(Node), pointer :: element => null() ! which element or document was the getByTagName run from?
    type(ListNode), pointer :: nodes(:) => null()
    integer :: length = 0
  end type NodeList

  type NodeListptr
    private
    type(NodeList), pointer :: this
  end type NodeListptr

  type NamedNodeMap
    private
    logical :: readonly = .false.
    type(Node), pointer :: ownerElement => null()
    type(ListNode), pointer :: nodes(:) => null()
    integer :: length = 0
  end type NamedNodeMap

  type documentExtras
    type(DOMImplementation), pointer :: implementation => null() ! only for doctype
    type(Node), pointer :: docType => null()
    type(Node), pointer :: documentElement => null()
    character, pointer :: inputEncoding => null()
    character, pointer :: xmlEncoding => null()
    type(NodeListPtr), pointer :: nodelists(:) => null() ! document
    ! In order to keep track of all nodes not connected to the document
    logical :: liveNodeLists ! For the document, are nodelists live?
    type(NodeList) :: hangingNodes ! For the document. list of nodes not associated with doc
    type(xml_doc_state), pointer :: xds => null()
    type(namedNodeMap) :: entities ! actually for doctype
    type(namedNodeMap) :: notations ! actually for doctype
    logical :: strictErrorChecking = .false.
    character, pointer :: documentURI(:) => null()
  end type documentExtras

  type elementOrAttributeExtras
    type(NamedNodeMap) :: attributes
    character, pointer, dimension(:) :: namespaceURI => null() ! \
    character, pointer, dimension(:) :: prefix => null()       !  - only useful for element & attribute
    character, pointer, dimension(:) :: localName => null()    ! /
    logical :: specified
    type(Node), pointer :: ownerElement => null()
    logical :: isId
    type(NodeList) :: namespaceNodes
  end type elementOrAttributeExtras

  type DTDExtras
    character, pointer :: publicId(:) => null() ! doctype, entity, notation 
    character, pointer :: systemId(:) => null() ! doctype, entity, notation
    character, pointer :: internalSubset(:) => null() ! doctype
    character, pointer :: notationName(:) => null() ! entity
  end type DTDExtras

  type Node
    private
    logical :: readonly = .false.
    character, pointer, dimension(:)         :: nodeName => null()
    character, pointer, dimension(:)         :: nodeValue => null()
    integer             :: nodeType        = 0
    type(Node), pointer :: parentNode      => null()
    type(Node), pointer :: firstChild      => null()
    type(Node), pointer :: lastChild       => null()
    type(Node), pointer :: previousSibling => null()
    type(Node), pointer :: nextSibling     => null()
    type(Node), pointer :: ownerDocument   => null()
    type(NodeList) :: childNodes ! not for text, cdata, PI, comment, notation,  docType, XPath
    ! Introduced in DOM Level 2:
    character, pointer, dimension(:) :: namespaceURI => null() ! \
    character, pointer, dimension(:) :: prefix => null()       !  - only useful for element & attribute
    character, pointer, dimension(:) :: localName => null()    ! /

    ! Introduced in DOM Level 2
    type(Node), pointer :: ownerElement => null() ! only for attribute
    character, pointer :: publicId(:) => null() ! doctype, entity, notation 
    character, pointer :: systemId(:) => null() ! doctype, entity, notation
    character, pointer :: internalSubset(:) => null() ! doctype
    character, pointer :: notationName(:) => null() ! entity
    ! Introduced in DOM Level 3
    character, pointer :: inputEncoding(:) => null() ! document/doctype?
    character, pointer :: xmlEncoding(:) => null()   ! document/doctype?
    logical :: strictErrorChecking = .false. ! document/doctype
    character, pointer :: documentURI(:) => null() ! document/doctype
    ! DOMCONFIGURATION
    logical :: illFormed = .false. ! entity
    logical :: inDocument = .false.! For a node, is this node associated to the doc?
    type(documentExtras), pointer :: docExtras
    type(elementOrAttributeExtras), pointer :: elExtras
  end type Node

  type(DOMImplementation), save, target :: FoX_DOM

  interface destroy
    module procedure destroyNode, destroyNodeList, destroyNamedNodeMap
  end interface destroy

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

  public :: DOMImplementation
  public :: Node

  public :: ListNode
  public :: NodeList
  public :: NamedNodeMap

  public :: destroyAllNodesRecursively
  public :: setIllFormed

')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_function(createNode, (arg, nodeType, nodeName, nodeValue), np)
    type(Node), pointer :: arg
    integer, intent(in) :: nodeType
    character(len=*), intent(in) :: nodeName
    character(len=*), intent(in) :: nodeValue
    type(Node), pointer :: np

    allocate(np)
    np%ownerDocument => arg
    np%nodeType = nodeType
    np%nodeName => vs_str_alloc(nodeName)
    np%nodeValue => vs_str_alloc(nodeValue)

    allocate(np%childNodes%nodes(0))

  end function createNode

  TOHW_subroutine(destroyNode, (np))
    type(Node), pointer :: np

    if (.not.associated(np)) return

    select case(np%nodeType)
    case (ELEMENT_NODE)
      call destroyElement(np)
    case (DOCUMENT_NODE)
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    case default
      call destroyNodeContents(np)
      deallocate(np)
    end select

  end subroutine destroyNode

  TOHW_subroutine(destroyElement, (element))
    type(Node), pointer :: element

    if (element%nodeType /= ELEMENT_NODE) then
       TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    if (associated(element%elExtras%attributes%nodes)) deallocate(element%elExtras%attributes%nodes)
    deallocate(element%elExtras)
    call destroyNodeContents(element)
    deallocate(element)

  end subroutine destroyElement

  subroutine destroyAllNodesRecursively(arg)
    type(Node), pointer :: arg
    
    type(Node), pointer :: this, deadNode, treeroot
    logical :: doneChildren, doneAttributes
    integer :: i_tree

    if (.not.associated(arg)) return

    treeroot => arg
TOHW_m_dom_treewalk(`',`',`deadNode', `')

    deallocate(arg%childNodes%nodes)
    allocate(arg%childNodes%nodes(0))
    arg%firstChild => null()
    arg%lastChild => null()

  end subroutine destroyAllNodesRecursively

  subroutine destroyNodeContents(np)
    type(Node), intent(inout) :: np
    
    if (associated(np%nodeName)) deallocate(np%nodeName)
    if (associated(np%nodeValue)) deallocate(np%nodeValue)
    if (associated(np%namespaceURI)) deallocate(np%namespaceURI)
    if (associated(np%prefix)) deallocate(np%prefix)
    if (associated(np%localname)) deallocate(np%localname)
    if (associated(np%publicId)) deallocate(np%publicId)
    if (associated(np%systemId)) deallocate(np%systemId)
    if (associated(np%internalSubset)) deallocate(np%internalSubset)
    if (associated(np%notationName)) deallocate(np%notationName)

    if (associated(np%inputEncoding)) deallocate(np%inputEncoding)
    if (associated(np%xmlEncoding)) deallocate(np%xmlEncoding)
    !if (associated(np%xmlVersion)) deallocate(np%xmlVersion)
    if (associated(np%documentURI)) deallocate(np%documentURI)

    deallocate(np%childNodes%nodes)

  end subroutine destroyNodeContents

')`'dnl

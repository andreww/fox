include(`m_dom_exception.m4')`'dnl
TOHW_m_dom_imports(`

  use m_common_array_str, only: vs_str_alloc
  use m_common_struct, only: xml_doc_state, destroy_xml_doc_state

')`'dnl
dnl
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


  type DOMImplementation
    private
    character(len=7) :: id = "FoX_DOM"
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
    type(Node), pointer :: docType
    type(Node), pointer :: documentElement
    character, pointer :: inputEncoding
    character, pointer :: xmlEncoding
    type(NodeListPtr), pointer :: nodelists(:) ! document
    ! In order to keep track of all nodes not connected to the document
    logical :: liveNodeLists ! For the document, are nodelists live? (FIXME should be in xds)
    type(NodeList) :: hangingNodes ! For the document. list of nodes not associated with doc
  end type documentExtras

  type ElementOrAttributeExtras
    type(NamedNodeMap) :: attributes
    character, pointer, dimension(:) :: namespaceURI => null() ! \
    character, pointer, dimension(:) :: prefix => null()       !  - only useful for element & attribute
    character, pointer, dimension(:) :: localName => null()    ! /
    logical :: specified
    type(Node), pointer :: ownerElement => null()
    logical :: isId
    type(NodeList) :: namespaceNodes
  end type ElementOrAttributeExtras

  type DTDExtras
    type(namedNodeMap) :: entities ! only for doctype
    type(namedNodeMap) :: notations ! only for doctype
    character, pointer :: publicId(:) => null() ! doctype, entity, notation 
    character, pointer :: systemId(:) => null() ! doctype, entity, notation
    character, pointer :: internalSubset(:) => null() ! doctype
    character, pointer :: notationName(:) => null() ! entity
  end type DTDExtras

  type Node
    private
    logical :: readonly = .false. ! FIXME must check this everywhere
    character, pointer, dimension(:)         :: nodeName => null()
    character, pointer, dimension(:)         :: nodeValue => null()
    integer             :: nodeType        = 0
    type(Node), pointer :: parentNode      => null()
    type(Node), pointer :: firstChild      => null()
    type(Node), pointer :: lastChild       => null()
    type(Node), pointer :: previousSibling => null()
    type(Node), pointer :: nextSibling     => null()
    type(Node), pointer :: ownerDocument   => null()
    type(NamedNodeMap) :: attributes ! only for elements
    type(NodeList) :: childNodes ! not for text, cdata, PI, comment, notation,  docType, XPath
    ! Introduced in DOM Level 2:
    character, pointer, dimension(:) :: namespaceURI => null() ! \
    character, pointer, dimension(:) :: prefix => null()       !  - only useful for element & attribute
    character, pointer, dimension(:) :: localName => null()    ! /
    type(Node), pointer :: doctype => null()  ! only for document
    type(DOMImplementation), pointer :: implementation => null() ! only for doctype
    type(Node), pointer :: documentElement => null() ! only for document
    logical :: specified ! only for attribute
    ! Introduced in DOM Level 2
    type(Node), pointer :: ownerElement => null() ! only for attribute
    type(namedNodeMap) :: entities ! only for doctype
    type(namedNodeMap) :: notations ! only for doctype
    ! FIXME The two above should be held in xds below
    character, pointer :: publicId(:) => null() ! doctype, entity, notation 
    character, pointer :: systemId(:) => null() ! doctype, entity, notation
    character, pointer :: internalSubset(:) => null() ! doctype
    character, pointer :: notationName(:) => null() ! entity
    ! Introduced in DOM Level 3
    character, pointer :: inputEncoding(:) => null() ! document/doctype?
    character, pointer :: xmlEncoding(:) => null()   ! document/doctype?
    ! logical :: xmlStandalone = .false.
    ! character, pointer :: xmlVersion(:) => null() 
    ! The two above are held in xds below
    logical :: strictErrorChecking = .false. ! document/doctype
    character, pointer :: documentURI(:) => null() ! document/doctype
    ! DOMCONFIGURATION
    type(xml_doc_state), pointer :: xds => null()
    !TYPEINFO schemaTypeInfo
    logical :: isId ! attribute
    ! In order to keep all node lists live ..
    type(NodeListPtr), pointer :: nodelists(:) ! document
    ! In order to keep track of all nodes not connected to the document
    logical :: liveNodeLists ! For the document, are nodelists live? (FIXME should be in xds)
    type(NodeList) :: hangingNodes ! For the document. list of nodes not associated with doc
    logical :: inDocument = .false.! For a node, is this node associated to the doc?
!!
!!
    type(documentExtras), pointer :: docExtras
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

  public :: setDocBuilding

')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_function(createNode, (doc, nodeType, nodeName, nodeValue), np)
    type(Node), pointer :: doc
    integer, intent(in) :: nodeType
    character(len=*), intent(in) :: nodeName
    character(len=*), intent(in) :: nodeValue
    type(Node), pointer :: np

    print*,"createNode", nodeType

    if (associated(doc)) then
      if (doc%nodeType/=DOCUMENT_NODE) then
        TOHW_m_dom_throw_error(FoX_INVALID_NODE)
      endif
    elseif (nodeType/=DOCUMENT_NODE) then
      print*,"Internal error creating node"
      stop
    endif

    allocate(np)
    np%ownerDocument => doc
    np%nodeType = nodeType
    np%nodeName => vs_str_alloc(nodeName)
    np%nodeValue => vs_str_alloc(nodeValue)

    allocate(np%childNodes%nodes(0))

  end function createNode

  subroutine destroyNode(np)
    type(Node), pointer :: np

    print*,"destroyNode", np%nodeType
    if (.not.associated(np)) return

    select case(np%nodeType)
    case (ELEMENT_NODE)
      call destroyElement(np)
    case (ATTRIBUTE_NODE)
      call destroyAttribute(np)
    case (ENTITY_REFERENCE_NODE)
      ! In principle a DOM might have children here. We dont. ! FIXME we do
      call destroyNodeContents(np)
      deallocate(np)
    case (ENTITY_NODE)
      ! ?? FIXME
      call destroyNodeContents(np)
      deallocate(np)
    case (DOCUMENT_NODE)
      ! well, I dont think this should ever be called, but if it is
      ! then go to destroy_document
      !call destroyDocument(np)
    case (DOCUMENT_TYPE_NODE)
      call destroyDocumentType(np)
    case default
      call destroyNodeContents(np)
      deallocate(np)
    end select

  end subroutine destroyNode

  subroutine destroyDocumentType(dt)
    type(Node), pointer :: dt

    integer :: i

    if (dt%nodeType/=DOCUMENT_TYPE_NODE) then
       ! FIXME internal error
    endif

    print*,"DESTROYDT"
    ! Entities need to be destroyed recursively - if they are done properly ...

    if (associated(dt%entities%nodes)) then
      do i = 1, size(dt%entities%nodes)
        call destroyAllNodesRecursively(dt%entities%nodes(i)%this)
        call destroy(dt%entities%nodes(i)%this)
      enddo
      deallocate(dt%entities%nodes)
    endif
    if (associated(dt%notations%nodes)) then
      do i = 1, size(dt%notations%nodes)
        call destroy(dt%notations%nodes(i)%this)
      enddo
      deallocate(dt%notations%nodes)
    endif

    call destroy_xml_doc_state(dt%xds)
    deallocate(dt%xds)

    call destroyNodeContents(dt)
    deallocate(dt)

    print*,"DONEDESTROYDT"

  end subroutine destroyDocumentType

  subroutine destroyElement(element)
    type(Node), pointer :: element

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME internal error
    endif

    if (associated(element%attributes%nodes)) deallocate(element%attributes%nodes)
    call destroyNodeContents(element)
    deallocate(element)

  end subroutine destroyElement

  TOHW_subroutine(destroyAttribute, (attr))
    type(Node), pointer :: attr

    type(Node), pointer :: np, np_next

    if (attr%nodeType/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    call destroyNodeContents(attr)
    deallocate(attr)

  end subroutine destroyAttribute

  TOHW_subroutine(destroyDocumentFragment, (df))
    type(Node), pointer :: df

    if (df%nodeType/=DOCUMENT_FRAGMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    call destroyAllNodesRecursively(df)

    call destroyNodeContents(df)
    deallocate(df)

  end subroutine destroyDocumentFragment

  subroutine destroyAllNodesRecursively(arg)
    type(Node), pointer :: arg
    
    type(Node), pointer :: this, deadNode
    logical :: doneChildren, doneAttributes
    integer :: i

    if (.not.associated(arg)) return
    this => arg

TOHW_m_dom_treewalk(`',`',`deadNode')

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

! Some convenience functions for internal use:

  subroutine setDocBuilding(doc,b)
    type(Node), pointer :: doc
    logical, intent(in) :: b
    doc%docType%xds%building = b
  end subroutine setDocBuilding

')`'dnl

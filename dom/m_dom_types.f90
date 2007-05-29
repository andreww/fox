module m_dom_types

  use m_common_array_str, only: vs_str_alloc

  implicit none
  private

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
    character, pointer :: id(:)
  end type DOMImplementation

  type ListNode
    type(Node), pointer :: this => null()
  end type ListNode 

  type NodeList
    type(ListNode), pointer :: nodes(:)
    integer :: length = 0
  end type NodeList

  type NamedNode
    type(Node), pointer :: this => null()
    type(NamedNode), pointer :: next => null()
  end type NamedNode 

  type NamedNodeMap
    type(NamedNode), pointer :: head => null()
    type(NamedNode), pointer :: tail => null()
    integer :: length = 0
  end type NamedNodeMap

  type Node
    character, pointer, dimension(:)         :: nodeName => null()
    character, pointer, dimension(:)         :: nodeValue => null()
    integer              :: nc              = 0 
    integer              :: nodeType        = 0
    type(Node), pointer :: parentNode      => null()
    type(Node), pointer :: firstChild      => null()
    type(Node), pointer :: lastChild       => null()
    type(Node), pointer :: previousSibling => null()
    type(Node), pointer :: nextSibling     => null()
    type(Node), pointer :: ownerDocument   => null()
    type(NamedNodeMap), pointer :: attributes => null()
    type(NodeList), pointer :: childNodes  => null()  ! New
    ! Introduced in DOM Level 2:
    character, pointer, dimension(:) :: namespaceURI => null()
    character, pointer, dimension(:) :: prefix => null()
    character, pointer, dimension(:) :: localName => null()
    type(Node), pointer :: doctype => null()
    type(DOMImplementation), pointer :: implementation => null()
    type(Node), pointer :: documentElement => null()
    character, pointer :: name(:) => null()
    logical :: specified
    character, pointer :: value => null()
    ! Introduced in DOM Level 2
    type(Node), pointer :: ownerElement => null()
    character, pointer :: tagName(:) => null()
    type(namedNodeMap), pointer :: entities => null()
    type(namedNodeMap), pointer :: notations => null()
    character, pointer :: publicId(:) => null()
    character, pointer :: systemId(:) => null()
    character, pointer :: internalSubset(:) => null()
    character, pointer :: notationName(:) => null()
    character, pointer :: target(:) => null()
    character, pointer :: data(:) => null()
  end type Node

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

  public :: NamedNode
  public :: ListNode
  public :: NodeList
  public :: NamedNodeMap

  public :: createNode
  public :: destroyNode

contains

  function createNode(doc, nodeType, nodeName, nodeValue) result(np)
    type(Node), pointer :: doc
    integer, intent(in) :: nodeType
    character(len=*), intent(in) :: nodeName
    character(len=*), intent(in) :: nodeValue
    type(Node), pointer :: np

    if (associated(doc)) then
      if (doc%nodeType/=DOCUMENT_NODE) then
        ! internal error
        continue
      endif
    endif

    allocate(np)
    np%ownerDocument => doc
    np%nodeType = nodeType
    np%nodeName => vs_str_alloc(nodeName)
    np%nodeValue => vs_str_alloc(nodeValue)
  end function createNode

  subroutine destroyNode(np)
    type(Node), pointer :: np

    if (associated(np%nodeName)) deallocate(np%nodeName)
    np%nodeName => null()
    if (associated(np%nodeValue)) deallocate(np%nodeValue)
    np%nodeValue => null()
    np%nc = 0
    np%nodeType = 0
    
    np%parentNode => null()
    np%firstChild => null()
    np%lastChild => null()
    np%previousSibling => null()
    np%nextSibling => null()
    np%ownerDocument => null()

    np%attributes => null()
    np%childNodes => null()
    if (associated(np%namespaceURI)) deallocate(np%namespaceURI)
    np%namespaceURI => null()
    if (associated(np%prefix)) deallocate(np%prefix)
    np%prefix => null()
    if (associated(np%localname)) deallocate(np%localname)
    np%localname => null()

    np%docType => null()! FIXME do we deallocate this?
    np%implementation => null()

    np%documentElement => null()

    if (associated(np%name)) deallocate(np%name)
    np%name => null()
    if (associated(np%value)) deallocate(np%value)
    np%value => null()

    np%ownerElement => null()
    if (associated(np%tagname)) deallocate(np%tagname)
    np%tagname => null()

    np%entities => null() ! what about deallocation
    np%notations => null() ! what about deallocation

    ! FIXME more
    deallocate(np)
  end subroutine destroyNode

end module m_dom_types

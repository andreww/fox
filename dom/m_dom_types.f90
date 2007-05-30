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
    type(ListNode), pointer :: nodes(:) => null()
    integer :: length = 0
  end type NodeList

  type NamedNodeMap
    type(NodeList) :: list 
  end type NamedNodeMap

  type Node
    logical :: readonly = .false. ! FIXME must check this everywhere
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
    type(NamedNodeMap) :: attributes
    type(NodeList), pointer :: childNodes  => null()  ! New
    ! Introduced in DOM Level 2:
    character, pointer, dimension(:) :: namespaceURI => null()
    character, pointer, dimension(:) :: prefix => null()
    character, pointer, dimension(:) :: localName => null()
    type(Node), pointer :: doctype => null()
    type(DOMImplementation), pointer :: implementation => null()
    type(Node), pointer :: documentElement => null()
    logical :: specified
    ! Introduced in DOM Level 2
    type(Node), pointer :: ownerElement => null()
    type(namedNodeMap) :: entities
    type(namedNodeMap) :: notations 
    character, pointer :: publicId(:) => null()
    character, pointer :: systemId(:) => null()
    character, pointer :: internalSubset(:) => null()
    character, pointer :: notationName(:) => null()
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

  public :: ListNode
  public :: NodeList
  public :: NamedNodeMap

  public :: createNode
  public :: destroyNode
  public :: destroyNodeContents

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

    print*,'allocating a node:', nodeType, nodeName

    allocate(np)
    np%ownerDocument => doc
    np%nodeType = nodeType
    np%nodeName => vs_str_alloc(nodeName)
    np%nodeValue => vs_str_alloc(nodeValue)
  end function createNode

  recursive subroutine destroyNode(np)
    type(Node), pointer :: np

    select case(np%nodeType)
    case (ELEMENT_NODE)
      call destroyElement(np)
    case (ATTRIBUTE_NODE)
      call destroyAttribute(np)
    case (ENTITY_REFERENCE_NODE)
      ! In principle a DOM might have children here. We don't.
      call destroyNodeContents(np)
      deallocate(np)
    case (ENTITY_NODE)
      ! ?? FIXME
      call destroyNodeContents(np)
      deallocate(np)
    case (DOCUMENT_NODE)
      ! well, I don't think this should ever be called, but if it is
      ! then go to destroy_document
      !call destroyDocument(np)
    case (DOCUMENT_TYPE_NODE)
      call destroyDocumentType(np)
    case (DOCUMENT_FRAGMENT_NODE)
      !call destroyDocumentFragment
    case default
      print*, 'destroying a node:', np%nodeType, np%nodeName
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

    ! Entities need to be destroyed recursively

    do i = 1, dt%notations%list%length
      call destroyNode(dt%notations%list%nodes(i)%this)
    enddo
    if (associated(dt%notations%list%nodes)) deallocate(dt%notations%list%nodes)

    print*, 'destroying a node:', dt%nodeType, dt%nodeName
    call destroyNodeContents(dt)
    deallocate(dt)

  end subroutine destroyDocumentType

  subroutine destroyElement(element)
    type(Node), pointer :: element

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME internal error
    endif

    do i = 1, element%attributes%list%length
      call destroyNode(element%attributes%list%nodes(i)%this)
    enddo
    !    call destroyNamedNodeMap(element%attributes)
    if (associated(element%attributes%list%nodes)) deallocate(element%attributes%list%nodes)
    print*, 'destroying a node:', element%nodeType, element%nodeName
    call destroyNodeContents(element)
    deallocate(element)

  end subroutine destroyElement

  subroutine destroyAttribute(attr)
    type(Node), pointer :: attr

    type(Node), pointer :: np, np_next

    if (attr%nodeType/=ATTRIBUTE_NODE) then
       ! FIXME internal error
    endif

    np => attr%firstChild
    do while (associated(np))
      np_next => np%nextSibling
      call destroyNode(np)
      np => np_next
    enddo

    print*, 'destroying a node:', attr%nodeType, attr%nodeName
    call destroyNodeContents(attr)
    deallocate(attr)

  end subroutine destroyAttribute


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
  end subroutine destroyNodeContents

end module m_dom_types

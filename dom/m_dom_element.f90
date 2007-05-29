module m_dom_element

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_namecheck, only: prefixOfQName, localpartOfQName

  use m_dom_types, only: Node, NodeList, Node
  use m_dom_types, only: destroyNode
  use m_dom_types, only: DOCUMENT_NODE, ELEMENT_NODE, TEXT_NODE
  
  !use m_dom_node, only: getnodename
  !use m_dom_node, only: getfirstchild
  !use m_dom_node, only: getnextsibling
  use m_dom_node, only: haschildnodes
  
  use m_dom_nodelist, only: append
  
  use m_dom_namednodemap, only: removenameditem
  use m_dom_namednodemap, only: setnameditem
  use m_dom_namednodemap, only: getnameditem
  
  use m_dom_document, only: createAttribute, createAttributeNS
  use m_dom_debug, only: dom_debug
  
  implicit none
  private

  public :: getTagName
  public :: getAttribute
  public :: getAttributeNode
  public :: setAttribute
  public :: setAttributeNS
  public :: setAttributeNode
  public :: setAttributeNodeNS
  public :: removeAttribute

contains

  function getTagName(element)
    type(Node), intent(in) :: element   
    character(len=size(element%nodeName)) :: getTagName
    
    if (element % nodeType == ELEMENT_NODE) then
       getTagName = str_vs(element%nodeName )
    else
       getTagName = ''
    endif
    
  end function getTagName

    
  function getAttribute(element, name) result(attr)
    
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: name
    character(len=100) :: attr ! FIXME

    type(Node), pointer :: nn

    attr = ""  ! as per specs, if not found
    if (element % nodeType /= ELEMENT_NODE) return ! or throw an error FIXME?
    nn => getNamedItem(element%attributes,name)
    if (.not.associated(nn)) return ! or throw an error FIXME?
    
    attr = str_vs(nn%nodeValue)

        
  end function getAttribute


  function getAttributeNode(element, name)
    
    type(Node), intent(in) :: element
    type(Node), pointer    :: getAttributeNode
    character(len=*), intent(in) :: name

    getAttributeNode => null()     ! as per specs, if not found
    if (element % nodeType /= ELEMENT_NODE) RETURN
    getAttributeNode => getNamedItem(element%attributes,name)

  end function getAttributeNode
  
  function setAttributeNode(element, newattr) result(attr)
    type(Node), pointer :: element
    type(Node), pointer :: newattr
    type(Node), pointer :: attr

    if (element % nodeType /= ELEMENT_NODE) then
       if (dom_debug) print *, "not an element node in setAttributeNode..."
       return
    endif

    ! this checks if attribute exist already
    attr => setNamedItem(element%attributes,newattr)
  end function setAttributeNode

  function setAttributeNodeNS(element, newattr) result(attr)
    type(Node), pointer :: element
    type(Node), pointer :: newattr
    type(Node), pointer :: attr

    ! FIXME is this right or not - ie what are the rules for replacing here?
    attr => setAttributeNode(element, newattr)
  end function setAttributenodeNS

  function setAttribute(element, name, value) result(newattr)
    type(Node), pointer :: element
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    type(Node), pointer      :: newattr

    ! FIXME: Check does one exist already.

    newattr => createAttribute(element % ownerDocument, name)
    newattr%nodeValue => vs_str_alloc(value)
    newattr => setAttributeNode(element,newattr)

  end function setAttribute

  function setAttributeNS(element, nsURI, name, value) result(newattr)
    type(Node), pointer :: element
    character(len=*), intent(in) :: nsURI
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    type(Node), pointer      :: newattr

    ! FIXME: Check does one exist already.

    newattr => createAttributeNS(element % ownerDocument, nsURI, name)
    newattr%nodeValue => vs_str_alloc(value)
    newattr => setAttributeNodeNS(element,newattr)

  end function setAttributeNS

  !-----------------------------------------------------------

  subroutine removeAttribute(element, name)
    type(Node), pointer :: element
    character(len=*), intent(in) :: name

    type(Node), pointer :: dummy

    if (element % nodeType /= ELEMENT_NODE) RETURN
    if (.not. associated(element%attributes)) RETURN

    dummy => removeNamedItem(element%attributes,name)
     
  end subroutine removeAttribute

  !-----------------------------------------------------------
!!$  recursive subroutine normalize(element)
!!$    type(Node), pointer         :: element
!!$
!!$    type(Node), pointer        :: np, ghost
!!$    logical                     :: first
!!$
!!$    type(Node), pointer        :: head
!!$    character, pointer :: temp(:)
!!$
!!$    first = .true.  ! next Text node will be first
!!$
!!$    if (dom_debug) print *, "Normalizing: ", str_vs(element%nodeName)
!!$    np => element%firstChild
!!$    ! 
!!$    do
!!$       if (.not. associated(np)) exit
!!$       select case(np%nodeType)
!!$
!!$          case(TEXT_NODE) 
!!$             if (first) then
!!$                if (dom_debug) print *, "normalize: found first in chain"
!!$                head => np
!!$                first = .false.
!!$                np => np%nextSibling
!!$             else                    ! a contiguous text node
!!$                if (dom_debug) print *, "normalize: found second in chain"
!!$                temp => head%nodeValue
!!$                allocate(head%nodeValue(size(temp)+size(np%nodeValue)))
!!$                head%nodeValue(:size(temp)) = temp
!!$                head%nodeValue(size(temp)+1:) = np%nodeValue
!!$                head%nextSibling => np%nextSibling
!!$                if (associated(np,np%parentNode%lastChild)) then
!!$                   np%parentNode%lastChild => head
!!$                   head%nextSibling => null()
!!$                else
!!$                   np%nextSibling%previousSibling => head
!!$                endif
!!$                ghost => np
!!$                np => np%nextSibling
!!$                call destroyNode(ghost)
!!$             endif
!!$
!!$          case(ELEMENT_NODE)
!!$
!!$             first = .true.
!!$             if (dom_debug) print *, "element sibling: ", str_vs(np%nodeName)
!!$             if (hasChildNodes(np)) call normalize(np)
!!$             np => np%nextSibling
!!$
!!$          case default
!!$             
!!$             ! do nothing, just mark that we break the chain of text nodes
!!$             if (dom_debug) print *, "other sibling: ", str_vs(np%nodeName)
!!$             first = .true.
!!$             np => np%nextSibling
!!$
!!$        end select
!!$
!!$     enddo
!!$
!!$    end subroutine normalize

end module m_dom_element

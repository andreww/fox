module m_dom_element

  use m_common_array_str, only: str_vs

use m_dom_types, only: fnode, fnodelist, fDocumentNode
use m_dom_types, only: element_node, document_node
use m_dom_types, only: text_node
use m_dom_types, only: destroyNode

use m_dom_attribute, only: setValue

use m_dom_node, only: getnodename
use m_dom_node, only: getfirstchild
use m_dom_node, only: getnextsibling
use m_dom_node, only: haschildnodes

use m_dom_nodelist, only: append

use m_dom_namednodemap, only: removenameditem
use m_dom_namednodemap, only: setnameditem
use m_dom_namednodemap, only: getnameditem

use m_dom_document, only: createattribute
use m_dom_debug, only: dom_debug

use m_strings, only: string, assignment(=)
use m_strings, only: stringify, operator(==)
use m_strings, only: operator(+)

implicit none

private

interface getElementById
  module procedure getElementById_doc, getElementById_node
end interface getElementById

  public :: getTagName
  public :: getAttribute
  public :: getAttributeNode
  public :: setAttribute
  public :: setAttributeNS
  public :: setAttributeNode
  public :: removeAttribute
  public :: normalize

  public :: getElementsByTagName
  public :: getElementById

contains

  function getTagName(element)
    type(fnode), intent(in) :: element   
    character(len=size(element%nodeName)) :: getTagName
    
    if (element % nodeType == ELEMENT_NODE) then
       getTagName = str_vs(element%nodeName )
    else
       getTagName = ''
    endif
    
  end function getTagName

    
  !-----------------------------------------------------------
  function getElementsByTagName(element, tag) result(nodelist)
    type(fnode), pointer         :: element
    character(len=*), intent(in) :: tag
    type(fnodeList), pointer     :: nodelist 

    type(fnode), pointer        :: np

    nodelist => null()

    np => element
    if (dom_debug) print *, "Going into search for tag: ", tag
    call search(np)

    CONTAINS

    recursive subroutine search(np)
    type(fnode), pointer        :: np

    type(string)                :: name

    !
    ! Could replace the calls to helper methods by direct lookups of node 
    ! components to make it faster.
    ! 
    do
       if (.not. associated(np)) exit
       select case(np%nodeType)
          case(DOCUMENT_NODE) 
             ! special case ... search its children 
             if (hasChildNodes(np)) call search(getFirstChild(np))
             ! will exit for lack of siblings
          case(ELEMENT_NODE)
             name = getNodeName(np)
             if (dom_debug) print *, "exploring node: ", stringify(name)
             if ((tag == "*") .or. (tag == name)) then
                call append(nodelist,np)
                if (dom_debug) print *, "found match ", nodelist%length
             endif
             if (hasChildNodes(np)) call search(getFirstChild(np))
          case default
             
             ! do nothing

        end select

        if (associated(np,element)) exit  ! no siblings of element...
        np => getNextSibling(np)

     enddo
    
    end subroutine search

  end function getElementsByTagName

  !-----------------------------------------------------------

  function getAttribute(element, name)
    
    type(fnode), intent(in) :: element
    character(len=*), intent(in) :: name
    ! FIXME
    character(len=100) :: getAttribute

    type(fnode), pointer :: nn

    getAttribute = ""  ! as per specs, if not found
    if (element % nodeType /= ELEMENT_NODE) RETURN
    nn => getNamedItem(element%attributes,name)
    if (.not. associated(nn)) RETURN
    
    getAttribute = str_vs(nn%nodeValue)

        
  end function getAttribute

  !-----------------------------------------------------------

  function getAttributeNode(element, name)
    
    type(fnode), intent(in) :: element
    type(fnode), pointer    :: getAttributeNode
    character(len=*), intent(in) :: name

    getAttributeNode => null()     ! as per specs, if not found
    if (element % nodeType /= ELEMENT_NODE) RETURN
    getAttributeNode => getNamedItem(element%attributes,name)

  end function getAttributeNode
  
  !-----------------------------------------------------------

  subroutine setAttributeNode(element, newattr)
    type(fnode), pointer :: element
    type(fnode), pointer :: newattr

    type(fnode), pointer :: dummy

    if (element % nodeType /= ELEMENT_NODE) then
       if (dom_debug) print *, "not an element node in setAttributeNode..."
       return
    endif

    dummy => setNamedItem(element%attributes,newattr)
    nullify(dummy)
    
  end subroutine setAttributeNode

!-------------------------------------------------------------------
  subroutine setAttribute(element, name, value)
    type(fnode), pointer :: element
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    type(fnode), pointer      :: newattr

    newattr => createAttribute(element % ownerDocument, name)
    call setValue(newattr,value)
    call setAttributeNode(element,newattr)

  end subroutine setAttribute
  subroutine setAttributeNS(element, name, value, nsURI, localname)
    type(fnode), pointer :: element
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value
    character(len=*), intent(in) :: nsURI
    character(len=*), intent(in) :: localName

    type(fnode), pointer      :: newattr

    newattr => createAttribute(element % ownerDocument, name)
    call setValue(newattr,value)
    call setnsURI(newattr, nsURI)
    call setlocalName(newattr, localName)
    call setAttributeNode(element,newattr)

  end subroutine setAttributeNS

  !-----------------------------------------------------------

  subroutine removeAttribute(element, name)
    type(fnode), pointer :: element
    character(len=*), intent(in) :: name

    type(fnode), pointer :: dummy

    if (element % nodeType /= ELEMENT_NODE) RETURN
    if (.not. associated(element%attributes)) RETURN

    dummy => removeNamedItem(element%attributes,name)
     
  end subroutine removeAttribute

  !-----------------------------------------------------------
  recursive subroutine normalize(element)
    type(fnode), pointer         :: element

    type(fnode), pointer        :: np, ghost
    logical                     :: first

    type(fnode), pointer        :: head
    character, pointer :: temp(:)

    first = .true.  ! next Text node will be first

    if (dom_debug) print *, "Normalizing: ", str_vs(element%nodeName)
    np => element%firstChild
    ! 
    do
       if (.not. associated(np)) exit
       select case(np%nodeType)

          case(TEXT_NODE) 
             if (first) then
                if (dom_debug) print *, "normalize: found first in chain"
                head => np
                first = .false.
                np => getNextSibling(np)
             else                    ! a contiguous text node
                if (dom_debug) print *, "normalize: found second in chain"
                temp => head%nodeValue
                allocate(head%nodeValue(size(temp)+size(np%nodeValue)))
                head%nodeValue(:size(temp)) = temp
                head%nodeValue(size(temp)+1:) = np%nodeValue
                head%nextSibling => np%nextSibling
                if (associated(np,np%parentNode%lastChild)) then
                   np%parentNode%lastChild => head
                   head%nextSibling => null()
                else
                   np%nextSibling%previousSibling => head
                endif
                ghost => np
                np => getNextSibling(np)
                call destroyNode(ghost)
             endif

          case(ELEMENT_NODE)

             first = .true.
             if (dom_debug) print *, "element sibling: ", str_vs(np%nodeName)
             if (hasChildNodes(np)) call normalize(np)
             np => getNextSibling(np)

          case default
             
             ! do nothing, just mark that we break the chain of text nodes
             if (dom_debug) print *, "other sibling: ", str_vs(np%nodeName)
             first = .true.
             np => getNextSibling(np)

        end select

     enddo

    end subroutine normalize

  function getElementById_doc(doc, id) result(node)
    type(fDocumentNode), pointer :: doc
    type(string), intent(in) :: id
    type(fNode), pointer :: node

    node => getElementById(doc%documentElement, id)
  end function getElementById_doc

  recursive function getElementById_node(nodeIn, id) result(nodeOut)
    type(fnode), pointer :: nodeIn
    type(string), intent(in) :: id
    type(fnode), pointer :: nodeOut

    type(fnode), pointer :: child

    if (nodeIn%nodeType == ELEMENT_NODE) then
      if (getAttribute(nodeIn, "id") == id) then
        nodeOut => nodeIn
        return
      else
        child => getFirstChild(nodeIn)
        do while (associated(child))
          nodeOut => getElementById_node(child, id)
          if (associated(nodeOut)) return
          child => getNextSibling(child)
        enddo
        nodeOut => null()
      endif
    else
      nodeOut => null()
    endif
  end function getElementById_node

  function getElementsByTagNameNS(element, tag, namespaceURI) result(nodelist)
    type(fnode), pointer         :: element
    character(len=*), intent(in) :: tag
    character(len=*), intent(in) :: namespaceURI
    type(fnodeList), pointer     :: nodelist 

    type(fnode), pointer        :: np

    nodelist => null()

    np => element
    if (dom_debug) print *, "Going into search for tag: ", tag
    call search(np)

    CONTAINS

    recursive subroutine search(np)
    type(fnode), pointer        :: np

    type(string)                :: name

    !
    ! Could replace the calls to helper methods by direct lookups of node 
    ! components to make it faster.
    ! 
    do
       if (.not. associated(np)) exit
       select case(np%nodeType)
          case(DOCUMENT_NODE) 
             ! special case ... search its children 
             if (hasChildNodes(np)) call search(getFirstChild(np))
             ! will exit for lack of siblings
          case(ELEMENT_NODE)
             name = getNodeName(np)
             if (dom_debug) print *, "exploring node: ", stringify(name)
             if ((tag == "*") .or. (tag == name)) then
                call append(nodelist,np)
                if (dom_debug) print *, "found match ", nodelist%length
             endif
             if (hasChildNodes(np)) call search(getFirstChild(np))
          case default
             
             ! do nothing

        end select

        if (associated(np,element)) exit  ! no siblings of element...
        np => getNextSibling(np)

     enddo
    
    end subroutine search

  end function getElementsByTagNameNS



end module m_dom_element

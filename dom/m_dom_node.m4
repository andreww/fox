include(`m_dom_exception.m4')`'dnl
TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_dom_error, only: DOMException, throw_exception, is_in_error, &
    NO_MODIFICATION_ALLOWED_ERR, NOT_FOUND_ERR, HIERARCHY_REQUEST_ERR, &
    WRONG_DOCUMENT_ERR

')`'dnl
dnl
TOHW_m_dom_publics(`
  
  public :: getNodeName
  public :: getNodevalue	
  public :: setNodeValue
  public :: getNodeType
  public :: getParentNode
  public :: getChildNodes
  public :: getFirstChild
  public :: getLastChild
  public :: getNextSibling
  public :: getPreviousSibling
  public :: getAttributes
  public :: getOwnerDocument
  public :: insertBefore
  public :: replaceChild
  public :: removeChild
  public :: appendChild
  public :: hasChildNodes
  public :: cloneNode  
  public :: normalize
  public :: isSupported
  public :: getNamespaceURI
  public :: getPrefix
  public :: setPrefix
  public :: getLocalName
  public :: hasAttributes
!  public :: isSameNode

')`'dnl
TOHW_m_dom_contents(`

  ! Getters and setters

  function getNodeName(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%nodeName)) :: c
    
    c = str_vs(arg%nodeName)
  end function getNodeName

  function getNodeValue(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%nodeName)) :: c
    
    c = str_vs(arg%nodeName)
  end function getNodeValue
  
  TOHW_subroutine(setNodeValue, (arg, nodeValue))
    type(Node), intent(inout) :: arg
    character(len=*) :: nodeValue

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif
      
    !FIXME check what kind of node is it, what is nodeValue allowed to be ...
    ! if it is an attribute node we need to reset TEXT/ENTITYREF children.

    deallocate(arg%nodeValue)
    arg%nodeValue => vs_str_alloc(nodeValue)
  end subroutine setNodeValue

  function getNodeType(arg) result(n)
    type(Node), intent(in) :: arg
    integer :: n

    n = arg%nodeType
  end function getNodeType

  function getParentNode(arg) result(np)
    type(Node), intent(in) :: arg
    type(Node), pointer :: np

    np => arg%parentNode
  end function getParentNode
  
  function getChildNodes(arg) result(nl)
    type(Node), intent(in) :: arg
    type(NodeList), pointer :: nl

    nl => arg%childnodes
  end function getChildNodes
  
  function getFirstChild(arg) result(np)
    type(Node), intent(in) :: arg
    type(Node), pointer :: np

    np => arg%firstChild
  end function getFirstChild
  
  function getLastChild(arg) result(np)
    type(Node), intent(in) :: arg
    type(Node), pointer :: np

    np => arg%lastChild
  end function getLastChild

  function getPreviousSibling(arg) result(np)
    type(Node), intent(in) :: arg
    type(Node), pointer :: np

    np => arg%previousSibling
  end function getPreviousSibling
  
  function getNextSibling(arg) result(np)
    type(Node), intent(in) :: arg
    type(Node), pointer :: np

    np => arg%nextSibling
  end function getNextSibling

  function getAttributes(arg) result(nnm)
    type(Node), intent(in) :: arg
    type(NamedNodeMap), pointer :: nnm

! FIXME surely only if this is an element node?

    nnm = arg%attributes
  end function getAttributes

  function getOwnerDocument(arg) result(np)
    type(Node), intent(in) :: arg
    type(Node), pointer :: np

    np => arg%ownerDocument
  end function getOwnerDocument

  TOHW_function(insertBefore, (arg, newChild, refChild))
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: refChild
    type(Node), pointer :: insertBefore

    type(Node), pointer :: np

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif
    
!   FIXME what about this next?
    if (.not. associated(arg)) call dom_error("insertBefore",0,"Node not allocated")

! FIXME need to special case this for inserting documentElement and documentType on document nodes
    select case(arg%nodeType)
    case (ELEMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=CDATA_SECTION_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (ATTRIBUTE_NODE)
      if (newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (DOCUMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=DOCUMENT_TYPE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (DOCUMENT_FRAGMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=CDATA_SECTION_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case default
      TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    end select

    if (.not.associated(arg%ownerDocument, newChild%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif
    
    if (.not.associated(refChild)) then
      insertBefore => appendChild(arg, newChild)
      return
    endif
    
    np => arg%firstChild
    do while (associated(np))
      if (associated(np, refChild)) then
        if (associated(np, arg%firstChild)) then
          arg%firstChild => newChild
        else
          np%previousSibling%nextSibling => newChild
        endif
        refChild%previousSibling => newChild
        newChild%nextSibling => refChild
        newChild%parentNode => arg
        insertBefore => newChild
        return
      endif
      np => np%nextSibling
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function insertBefore
  

  TOHW_function(replaceChild, (arg, newChild, oldChild))
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: oldChild
    type(Node), pointer :: replaceChild

    type(Node), pointer :: np
    
    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not. associated(arg)) call dom_error("replaceChild",0,"Node not allocated")

    select case(arg%nodeType)
    case (ELEMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=CDATA_SECTION_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (ATTRIBUTE_NODE)
      if (newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (DOCUMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=DOCUMENT_TYPE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (DOCUMENT_FRAGMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=CDATA_SECTION_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case default
      TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    end select

    if (.not.associated(arg%ownerDocument, newChild%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    np => arg%firstChild

    do while (associated(np))    
       if (associated(np, oldChild)) then
          if (associated(np, arg%firstChild)) then
             arg%firstChild => newChild
             if (associated(np%nextSibling)) then
                np%nextSibling%previousSibling => newChild
             else
                arg%lastChild => newChild    ! there was just 1 node
             endif
          elseif (associated(np, arg%lastChild)) then
             ! one-node-only case covered above
             arg%lastChild => newChild
             np%previousSibling%nextSibling => newChild
          else
             np%previousSibling%nextSibling => newChild
             np%nextSibling%previousSibling => newChild
          endif
          newChild%parentNode => arg
          newChild%nextSibling => oldChild%nextSibling
          newChild% previousSibling => oldChild%previousSibling
          replaceChild => oldChild
          return
       endif
       np => np%nextSibling
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function replaceChild


  TOHW_function(removeChild, (arg, oldChild))
    type(Node), pointer :: removeChild
    type(Node), pointer :: arg
    type(Node), pointer :: oldChild
    type(Node), pointer :: np
    
    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not.associated(arg)) call dom_error("removeChild",0,"Node not allocated")
    np => arg%firstChild
    
    do while (associated(np))
      if (associated(np, oldChild)) then
        if (associated(np, arg%firstChild)) then
          arg%firstChild => np%nextSibling
          if (associated(np%nextSibling)) then
            arg%firstChild%previousSibling => null()
          else
            arg%lastChild => null()    ! there was just 1 node
          endif
        else if (associated(np, arg%lastChild)) then
          ! one-node-only case covered above
          arg%lastChild => np%previousSibling
          np%lastChild%nextSibling => null()
        else
          np%previousSibling%nextSibling => np%nextSibling
          np%nextSibling%previousSibling => np%previousSibling
        endif
        arg%nc = arg%nc -1
        np%previousSibling => null()    ! Are these necessary?
        np%nextSibling => null()
        np%parentNode => null()
        removeChild => oldChild
        return
      endif
      np => np%nextSibling
    enddo
    
    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function removeChild


  TOHW_function(appendChild, (arg, newChild))
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: appendChild
    
    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not. associated(arg))  & 
      call dom_error("appendChild",0,"Node not allocated")
    
    select case(arg%nodeType)
    case (ELEMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=CDATA_SECTION_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (ATTRIBUTE_NODE)
      if (newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (DOCUMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=DOCUMENT_TYPE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case (DOCUMENT_FRAGMENT_NODE)
      if (newChild%nodeType/=ELEMENT_NODE &
        .and. newChild%nodeType/=TEXT_NODE &
        .and. newChild%nodeType/=COMMENT_NODE &
        .and. newChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
        .and. newChild%nodeType/=CDATA_SECTION_NODE &
        .and. newChild%nodeType/=ENTITY_REFERENCE_NODE) &
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    case default
      TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
    end select

    if (.not.associated(arg%ownerDocument, newChild%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif
    
    if (.not.(associated(arg%firstChild))) then
      arg%firstChild => newChild
    else 
      newChild%previousSibling => arg%lastChild
      arg%lastChild%nextSibling => newChild 
    endif
    
    arg%lastChild => newChild
    newChild%parentNode => arg
    arg%nc = arg%nc + 1
    
    appendChild => newChild
    
  end function appendChild


  function hasChildNodes(arg)
    type(Node), pointer :: arg
    logical :: hasChildNodes
    
    if (.not. associated(arg)) call dom_error("hasChildNodes",0,"Node not allocated")
    hasChildNodes = associated(arg%firstChild)
    
  end function hasChildNodes

  function cloneNode(arg, deep) result(np)
    type(Node), pointer :: arg
    logical :: deep
    type(Node), pointer :: np

    ! FIXME implement
  end function cloneNode

!!$  recursive function cloneNode(arg, deep) result(np) 
!!$    type(Node), pointer :: arg
!!$    logical :: deep
!!$    type(Node), pointer :: np
!!$
!!$    logical :: do_children
!!$    integer :: i
!!$
!!$    type(Node), pointer :: original
!!$    type(Node), pointer :: dummy
!!$    
!!$    if (.not.associated(arg)) call dom_error("cloneNode",0,"Node not allocated")
!!$
!!$    do_children = .false.
!!$    if (present(deep)) then
!!$      do_children = deep
!!$    endif
!!$
!!$    select case (arg%nodeType)
!!$    case (ELEMENT_NODE)
!!$      np => createElementNS(arg%ownerDocument, str_vs(arg%namespaceURI), str_vs(arg%tagName))
!!$      do i = 1, arg%attributes%length
!!$        dummy => append(np%attributes, cloneNode(item(arg%attributes, i), .false.))
!!$        ! FIXME what about children of attribuytes? entities? do these happen?
!!$      enddo
!!$    case (ATTRIBUTE_NODE)
!!$      np => createAttributeNS(arg%ownerDocument, str_vs(arg%namespaceURI), str_vs(arg%tagName), str_vs(arg%value))
!!$    case (TEXT_NODE)
!!$      np => createTextNode(arg%ownerDocument, arg%data)
!!$    case (CDATA_SECTION_NODE)
!!$      np => createCDataSection(arg%ownerDocument, arg%data)
!!$    case (ENTITY_REFERENCE_NODE)
!!$      np => createEntityReference(arg%ownerDocument, arg%nodeName, arg%nodeValue)
!!$    case (ENTITY_NODE)
!!$      np => createEntity(arg%ownerDocument, arg%nodeName, arg%nodeValue)
!!$    case (PROCESSING_INSTRUCTION_NODE)
!!$      np => createProcessingInstruction(arg%ownerDocument, arg%target, arg%data)
!!$    case (COMMENT_NODE)
!!$      np => createComment(arg%ownerDocument, arg%data)
!!$    case (DOCUMENT_NODE)
!!$      np => createDocument(arg%ownerDocument, arg%name, arg%publicId, arg%systemId)
!!$    case (DOCUMENT_TYPE_NODE)
!!$      np => createDocumentType(arg%ownerDocument, arg%name, arg%entities, &
!!$        arg%notations, arg%publicId, arg%systemId, arg%internalSubset)
!!$    case (DOCUMENT_FRAGMENT_NODE)
!!$      np => createDocumentFragment(arg%ownerDocument)
!!$    case (NOTATION_NODE)
!!$      np => createNotation(arg%ownerDocument, arg%publicId, arg%systemId)
!!$    end select
!!$      
!!$    if (deep) then
!!$      continue
!!$    endif
!!$    ! and fix up siblings and parents and stuff
!!$    ! FIXME FIXME FIXME
!!$  end function cloneNode

  
  function hasAttributes(arg)
    type(Node), pointer :: arg
    logical :: hasAttributes
    
    if (.not.associated(arg)) call dom_error("hasAttributes",0,"Node not allocated")
    hasAttributes = (arg%nodeType /= ELEMENT_NODE) &
      .and. (arg%attributes%list%length > 0)
    
  end function hasAttributes
  
  subroutine normalize(arg)
    type(Node), intent(inout) :: arg
    
    ! FIXME implement
  end subroutine normalize

  function isSupported(arg, feature, version) result(p)
    type(Node), intent(in) :: arg
    character(len=*), intent(in) :: feature
    character(len=*), intent(in) :: version
    logical :: p

    ! FIXME implement
    p = .true.
  end function isSupported

  ! FIXME should the below instead just decompose the QName on access?
  function getNamespaceURI(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%namespaceURI)) :: c

    c = str_vs(arg%namespaceURI)
  end function getNamespaceURI

  function getPrefix(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%prefix)) :: c

    c = str_vs(arg%prefix)
  end function getPrefix
  
  subroutine setPrefix(arg, prefix)
    type(Node), intent(inout) :: arg
    character(len=*) :: prefix

    deallocate(arg%prefix)
    arg%prefix => vs_str_alloc(prefix)
  end subroutine setPrefix

  function getLocalName(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%localName)) :: c

    c = str_vs(arg%localName)
  end function getLocalName

  function isSameNode(node1, node2)    ! DOM 3.0
    type(Node), pointer :: node1
    type(Node), pointer :: node2
    logical :: isSameNode

    isSameNode = associated(node1, node2)

  end function isSameNode

')`'dnl

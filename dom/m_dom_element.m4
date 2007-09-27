dnl
TOHW_m_dom_publics(`
  
  public :: getTagName
  public :: getAttribute
  public :: setAttribute
  public :: removeAttribute
  public :: getAttributeNode
  public :: setAttributeNode
  public :: removeAttributeNode
  public :: getAttributeNS
  public :: setAttributeNS
  public :: removeAttributeNS
  public :: getAttributeNodeNS
  public :: setAttributeNodeNS
  public :: removeAttributeNodeNS
  public :: hasAttribute
  public :: hasAttributeNS

  public :: appendNSNode
')`'dnl
dnl
TOHW_m_dom_contents(`

TOHW_m_dom_get(DOMString, tagName, np%nodeName, (ELEMENT_NODE))

  pure function getAttributes_len(arg, p, name) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    character(len=*), intent(in) :: name
    integer :: n
    
    integer :: i
    
    n = 0
    if (.not.p) return
    if (arg%nodeType/=ELEMENT_NODE) return

    do i = 1, arg%elExtras%attributes%length
      if (str_vs(arg%elExtras%attributes%nodes(i)%this%nodeName)==name) then
        n = getValue_len(arg%elExtras%attributes%nodes(i)%this, .true.)
        exit
      endif
    enddo

  end function getAttributes_len

  TOHW_function(getAttribute, (arg, name), c)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    character(len=getAttributes_len(arg, associated(arg), name)) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg) /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = getValue(getNamedItem(getAttributes(arg), name))
        
  end function getAttribute


  TOHW_subroutine(setAttribute, (arg, name, value))
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    type(Node), pointer :: nn, dummy
    logical :: quickFix

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodetype(arg)/=ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.checkName(name, getXds(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkChars(value, getXmlVersionEnum(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif

    quickFix = getGCstate(getOwnerDocument(arg)) &
      .and. arg%inDocument

    if (quickFix) call setGCstate(getOwnerDocument(arg), .false.)
    ! then the created attribute is going straight into the document,
    ! so dont faff with hanging-node lists.

    nn => createAttribute(arg%ownerDocument, name)
    call setValue(nn, value)
    dummy => setNamedItem(getAttributes(arg), nn)
    if (associated(dummy)) then
      if (getGCstate(getOwnerDocument(arg)).and..not.dummy%inDocument) &
        call putNodesInDocument(getOwnerDocument(arg), dummy) 
      ! ... so that dummy & children are removed from hangingNodes list.
      call destroyAllNodesRecursively(dummy)
      call destroyNode(dummy)
    endif

    if (quickFix) call setGCstate(getOwnerDocument(arg), .true.)

  end subroutine setAttribute


  TOHW_subroutine(removeAttribute, (arg, name))
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name

    type(DOMException) :: ex2
    type(Node), pointer :: dummy
    integer :: e

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodetype(arg)/=ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif
    
    if (arg%inDocument) &
      call setGCstate(getOwnerDocument(arg), .false.)

    dummy => removeNamedItem(getAttributes(arg), name, ex2)
    ! removeNamedItem took care of any default attributes
    if (inException(ex2)) then
      e = getExceptionCode(ex2)
      if (e/=NOT_FOUND_ERR) then
        TOHW_m_dom_throw_error(e)
      endif
    else
      if (.not.arg%inDocument) then
        ! dummy was not in the doc, so was on hangingNode list.
        ! To remove it from the list:
        call putNodesInDocument(arg%ownerDocument, dummy)
      endif
      call destroyAllNodesRecursively(dummy)
      call destroyNode(dummy)
    endif
      
    if (arg%inDocument) &
      call setGCstate(arg%ownerDocument, .true.)

  end subroutine removeAttribute


  TOHW_function(getAttributeNode, (arg, name), attr)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    type(Node), pointer :: attr

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    attr => getNamedItem(getAttributes(arg), name)

  end function getAttributeNode
  

  TOHW_function(setAttributeNode, (arg, newattr), attr)
    type(Node), pointer :: arg
    type(Node), pointer :: newattr
    type(Node), pointer :: attr
    type(Node), pointer :: dummy

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.associated(arg%ownerDocument, newattr%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (associated(getOwnerElement(newattr), arg)) then
      attr => newattr
      return
      ! Nothing to do, this attribute is already in this element
    elseif (associated(getOwnerElement(newattr))) then
      TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)
    endif

    ! this checks if attribute exists already
    ! It also does any adding/removing of hangingnodes
    ! and sets ownerElement appropriately
    dummy => setNamedItem(getAttributes(arg), newattr, ex)
    attr => dummy

  end function setAttributeNode


  TOHW_function(removeAttributeNode, (arg, oldattr), attr)
    type(Node), pointer :: arg
    type(Node), pointer :: oldattr
    type(Node), pointer :: attr

    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.associated(arg%ownerDocument, oldattr%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    do i = 1, getLength(getAttributes(arg))
      if (associated(item(getAttributes(arg), i-1), oldattr)) then
        attr => removeNamedItem(getAttributes(arg), str_vs(oldattr%nodeName))
        ! removeNamedItem took care of any default attributes
        attr%elExtras%ownerElement => null()
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function removeAttributeNode


!  function getElementsByTagName - see m_dom_document


  pure function getAttributesNS_len(arg, p, localname, namespaceURI) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    character(len=*), intent(in) :: localname
    character(len=*), intent(in) :: namespaceURI
    integer :: n
    
    integer :: i
    
    n = 0
    if (.not.p) return
    if (arg%nodeType/=ELEMENT_NODE) return

    do i = 1, arg%elExtras%attributes%length
      if ((str_vs(arg%elExtras%attributes%nodes(i)%this%elExtras%localName)==localname &
        .and. str_vs(arg%elExtras%attributes%nodes(i)%this%elExtras%namespaceURI)==namespaceURI) &
        .or. (namespaceURI=="".and.str_vs(arg%elExtras%attributes%nodes(i)%this%nodeName)==localname)) then
        n = getValue_len(arg%elExtras%attributes%nodes(i)%this, .true.)
        exit
      endif
    enddo

  end function getAttributesNS_len

  TOHW_function(getAttributeNS, (arg, namespaceURI, localName), c)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    character(len=getAttributesNS_len(arg, associated(arg), localname, namespaceURI)) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = getNamedItemNS_Value(getAttributes(arg), namespaceURI, localName)
        
  end function getAttributeNS


  TOHW_subroutine(setAttributeNS, (arg, namespaceURI, qualifiedname, value))
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: qualifiedName
    character(len=*), intent(in) :: value

    type(Node), pointer :: nn, dummy
    logical :: quickfix

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.checkName(qualifiedname, getXds(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedname, getXds(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)=="xml" .neqv. & 
      namespaceURI=="http://www.w3.org/XML/1998/namespace") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (namespaceURI=="http://www.w3.org/2000/xmlns/" .neqv. &
      (qualifiedName=="xmlns" .or. prefixOfQName(qualifiedName)=="xmlns")) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    endif

! FIXME what if namespace is undeclared? Throw an error *only* if FoX_errors is on, otherwise its taken care of by namespace fixup on serialization

    quickFix = getGCstate(getOwnerDocument(arg)) &
      .and. arg%inDocument

    if (quickFix) call setGCstate(getOwnerDocument(arg), .false.)
    ! then the created attribute is going straight into the document,
    ! so dont faff with hanging-node lists.

    nn => createAttributeNS(arg%ownerDocument, namespaceURI, qualifiedname)
    call setValue(nn, value)
    dummy => setNamedItemNS(getAttributes(arg), nn)

    if (associated(dummy)) then
      if (getGCstate(getOwnerDocument(arg)).and..not.dummy%inDocument) &
        call putNodesInDocument(getOwnerDocument(arg), dummy) 
      ! ... so that dummy & children are removed from hangingNodes list.
      call destroyAllNodesRecursively(dummy)
      call destroyNode(dummy)
    endif

    if (quickFix) call setGCstate(getOwnerDocument(arg), .true.)

  end subroutine setAttributeNS


  TOHW_subroutine(removeAttributeNS, (arg, namespaceURI, localName))
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName

    type(DOMException) :: ex2
    type(Node), pointer :: dummy
    integer :: e

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (arg%inDocument) &
      call setGCstate(getOwnerDocument(arg), .false.)
    ! So we dont add the removed nodes to the hanging node list

    dummy => removeNamedItemNS(getAttributes(arg), namespaceURI, localName, ex2)
    ! removeNamedItemNS took care of any default attributes
    if (inException(ex2)) then
      e = getExceptionCode(ex2)
      if (e/=NOT_FOUND_ERR) then
        TOHW_m_dom_throw_error(e)
      endif
    else
      if (.not.arg%inDocument) then
        ! dummy was not in the doc, so was already on hangingNode list.
        ! To remove it from the list:
        call putNodesInDocument(arg%ownerDocument, dummy)
      endif
      call destroyAllNodesRecursively(dummy)
      call destroyNode(dummy)
    endif
      
    if (arg%inDocument) &
      call setGCstate(arg%ownerDocument, .true.)

  end subroutine removeAttributeNS


  TOHW_function(getAttributeNodeNS, (arg, namespaceURI, localName), attr)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(Node), pointer :: attr

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    attr => null()     ! as per specs, if not found
    attr => getNamedItemNS(getAttributes(arg), namespaceURI, localname)

  end function getAttributeNodeNS
  

  TOHW_function(setAttributeNodeNS, (arg, newattr), attr)
    type(Node), pointer :: arg
    type(Node), pointer :: newattr
    type(Node), pointer :: attr
    type(Node), pointer :: dummy

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.associated(arg%ownerDocument, newattr%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (associated(getOwnerElement(newattr), arg)) then
      attr => newattr
      return
      ! Nothing to do, this attribute is already in this element
    elseif (associated(getOwnerElement(newattr))) then
      TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)
    endif

    ! this checks if attribute exists already
    ! It also does any adding/removing of hangingnodes
    ! and sets ownerElement appropriately
    dummy => setNamedItemNS(getAttributes(arg), newattr, ex)
    newattr%elExtras%ownerElement => arg
    attr => dummy

  end function setAttributeNodeNS


  TOHW_function(removeAttributeNodeNS, (arg, oldattr), attr)
    type(Node), pointer :: arg
    type(Node), pointer :: oldattr
    type(Node), pointer :: attr

    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    do i = 1, getLength(getAttributes(arg))
      if (associated(item(getAttributes(arg), i-1), oldattr)) then
        attr => removeNamedItemNS(getAttributes(arg), &
          getNamespaceURI(oldattr), getLocalName(oldattr))
        ! removeNamedItemNS took care of any default attributes
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

    attr%elExtras%ownerElement => null()

  end function removeAttributeNodeNS


!  function getElementsByTagNameNS - see m_dom_document


  TOHW_function(hasAttribute, (arg, name), p)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    logical :: p

    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
 
   if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    p = .false.
    do i = 1, getLength(getAttributes(arg))
      if (getNodeName(item(getAttributes(arg), i-1))==name) then
        p = .true.
        exit
      endif
    enddo

  end function hasAttribute


  TOHW_function(hasAttributeNS, (arg, namespaceURI, localName), p)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    logical :: p

    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
 
   if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    p = .false.
    do i = 1, getLength(getAttributes(arg))
      if (getNamespaceURI(item(getAttributes(arg), i-1))==namespaceURI &
        .and. getLocalName(item(getAttributes(arg), i-1))==localName) then
        p = .true.
        exit
      endif
    enddo

  end function hasAttributeNS

! setIdAttribute
! setIdAttributeNS
! setIdAttributeNode

  TOHW_subroutine(appendNSNode, (np, prefix, namespaceURI, specified))
    type(Node), pointer :: np
    character(len=*), intent(in) :: prefix
    character(len=*), intent(in) :: namespaceURI
    logical, intent(in) :: specified

    type(Node), pointer :: nnp
    logical :: quickFix

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    
    if (np%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    ! We never put namespace nodes in the hanging nodes
    ! list since they can never be separated from their
    ! parent element node, so will always be destroyed alongside it.

    quickFix = getGCState(getOwnerDocument(np))
    call setGCState(getOwnerDocument(np), .false.)
    call append_nl(np%elExtras%namespaceNodes, &
      createNamespaceNode(getOwnerDocument(np), prefix, namespaceURI, specified))
    call setGCState(getOwnerDocument(np), quickFix)

  end subroutine appendNSNode

dnl  subroutine rationalizeNS(np)
dnl    type(Node), pointer :: np
dnl
dnl    if (.not.associated(np)) then
dnl      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
dnl    endif
dnl
dnl          nsParent => current
dnl      if (getNodeType(nsParent)==DOCUMENT_NODE) then
dnl        nsParent => null()
dnl      else
dnl        ! it is either an element or an entity reference node;
dnl        ! if the latter, we must climb the tree to find the first element ancestor.
dnl        do while (getNodeType(nsParent)/=ELEMENT_NODE)
dnl          nsParent => getParentNode(nsParent)
dnl        enddo
dnl      endif
dnl      if (associated(nsParent)) then
dnl        do while (getNodeType(nsParent)/=ELEMENT_NODE)
dnl          if (nsPa
dnl        parentNS: do i = 0, getLength(getNamespaces(getParentNode(el))) - 1
dnl          do j = 1, getNumberOfPrefixes(nsd)
dnl            if (getPrefixByIndex(nsd, j) == getPrefix(item(getNamespaces(el), i))) cycle parentNS
dnl          enddo
dnl          call appendNSNode(el, getPrefix(item(getNamespaces(el), i))), getNamespaceURI(item(getNamespaces(el), i)), specified=.false.)
dnl        enddo parentNS
dnl      endif
dnl    !FIXME DOM-XPath section 1.2.3 - implicit declaration of prefix?
dnl    endif
dnl    xml = .false.
dnl    ! By XML Infoset 2.2, the xmlns namespace should not appear.
dnl    do i = 0, getLength(getNamespaces(el)) - 1
dnl      if (getPrefix(item(getNamespaces(el), i)) == "xml") then
dnl        xml = .true.
dnl        exit
dnl      endif
dnl    enddo
dnl    if (.not.xml) call appendNSNode(el, "xml", "http://www.w3.org/XML/1998/namespace", .false.)
dnl
dnl  end subroutine rationalizeNS

dnl    if (len(URI)>0) then
dnl      ! This is a namespace-aware element node.
dnl      ! Do all namespace resolution by creating namespace nodes ...
dnl
dnl      ! First, attach all nodes specified directly on the current node.
dnl      nsd => getnsDict(fxml%fx)
dnl      ! FIXME check specified properly
dnl      if (isDefaultNSInForce(nsd)) call appendNSNode(el, "", getNamespaceURI(nsd), specified=.true.)
dnl      do i = 1, getNumberOfPrefixes(nsd)
dnl        if (getNamespaceURI(nsd, getPrefixByIndex(nsd, i))/="") then
dnl          call appendNSNode(el, getPrefixByIndex(nsd, i), getNamespaceURI(nsd, getPrefixByIndex(nsd, i)), specified=.true.)
dnl        endif
dnl      enddo
dnl    endif

')`'dnl

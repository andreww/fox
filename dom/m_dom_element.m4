TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_namecheck, only: prefixOfQName, localpartOfQName
  use m_dom_error, only: is_in_error, getCode

')`'dnl
dnl
TOHW_m_dom_publics(`
  
  public :: getTagName
  public :: getAttribute
  public :: setAttribute
  public :: removeAttribute
  public :: getAttributeNode
  public :: setAttributeNode
  public :: removeAttributeNode
  ! public :: getElementsByTagName
  public :: getAttributeNS
  public :: setAttributeNS
  public :: removeAttributeNS
  public :: getAttributeNodeNS
  public :: setAttributeNodeNS
  public :: removeAttributeNodeNS
  public :: hasAttribute
  public :: hasAttributeNS
')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_function(getTagName, (arg), c)
    type(Node), pointer :: arg   
    character(len=size(arg%nodeName)) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg) /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    c = str_vs(arg%nodeName)    
     
  end function getTagName

  pure function getAttributes_len(arg, p, name) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    character(len=*), intent(in) :: name
    integer :: n
    
    integer :: i
    
    n = 0
    if (.not.p) return
    if (arg%nodeType/=ELEMENT_NODE) return

    do i = 1, arg%attributes%length
      if (str_vs(arg%attributes%nodes(i)%this%nodeName)==name) then
        n = getValue_len(arg%attributes%nodes(i)%this, .true.)
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
    nn%ownerElement => arg

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
    if (is_in_error(ex2)) then
      e = getCode(ex2)
      if (e/=NOT_FOUND_ERR) then
        TOHW_m_dom_throw_error(e)
      endif
    else
      if (.not.arg%inDocument) then
        ! dummy was not in the doc, so was on hangingNode list.
        ! To remove it from the list:
        call putNodesInDocument(arg%ownerDocument, arg)
      endif
      call destroyAllNodesRecursively(dummy)
      call destroyNode(dummy)
    endif
      
    if (arg%inDocument) &
      call setGCstate(arg%ownerDocument, .true.)

  ! FIXME recreate a default value if there is one
     
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

    integer :: i

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

    if (associated(newattr%ownerElement, arg)) then
      attr => newattr
      return
      ! Nothing to do, this attribute is already in this element
    elseif (associated(newattr%ownerElement)) then
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

    do i = 1, arg%attributes%length
      if (associated(arg%attributes%nodes(i)%this, oldattr)) then
        attr => removeNamedItem(getAttributes(arg), str_vs(oldattr%nodeName))
        attr%ownerElement => null()
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function removeAttributeNode


!  function getElementsByTagName - see m_dom_document


  TOHW_function(getAttributeNS, (arg, namespaceURI, localName), c)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    character(len= &
      getNamedItemNS_Value_length(arg%attributes, namespaceURI, localName)) :: c

    type(Node), pointer :: nn

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = ""  ! as per specs, if not found Not sure ahout this FIXME
    c = getNamedItemNS_Value(getAttributes(arg), namespaceURI, localName)

    ! FIXME dont need both above
        
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
    elseif (.not.checkChars(qualifiedname, getXmlVersionEnum(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.checkQName(qualifiedname, getXds(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)=="xml" .and. & 
      namespaceURI/="http://www.w3.org/XML/1998/namespace") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    ! FIXME is this all possible errors? 
      ! what if prefix = "xmlns"? or other "xml"
    endif

! FIXME what if namespace is undeclared ... will be recreated on serialization,
! but we might need a new namespace node here for xpath ...

    quickFix = getGCstate(getOwnerDocument(arg)) &
      .and. arg%inDocument

    if (quickFix) call setGCstate(getOwnerDocument(arg), .false.)
    ! then the created attribute is going straight into the document,
    ! so dont faff with hanging-node lists.

    nn => createAttributeNS(arg%ownerDocument, namespaceURI, qualifiedname)
    call setValue(nn, value)
    if (associated(dummy)) then
      if (getGCstate(getOwnerDocument(arg)).and..not.dummy%inDocument) &
        call putNodesInDocument(getOwnerDocument(arg), dummy) 
      ! ... so that dummy & children are removed from hangingNodes list.
      call destroyAllNodesRecursively(dummy)
      call destroyNode(dummy)
    endif

    dummy => setNamedItemNS(getAttributes(arg), nn)
    nn%ownerElement => arg

    if (quickFix) call setGCstate(getOwnerDocument(arg), .true.)

    !FIXME catch exception

  end subroutine setAttributeNS


  TOHW_subroutine(removeAttributeNS, (arg, namespaceURI, localName))
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName

    type(Node), pointer :: dummy

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    dummy => removeNamedItemNS(getAttributes(arg), namespaceURI, localName)

    call destroyAttribute(dummy)
     
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
    elseif (associated(newattr%ownerElement)) then
      TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)
    endif

    ! this checks if attribute exists already, and does hangingnodes
    dummy => setNamedItemNS(getAttributes(arg), newattr)
    newattr%ownerElement => arg
    attr => newattr

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

    do i = 1, arg%attributes%length
      if (associated(item(getAttributes(arg), i-1), oldattr)) then
        attr => removeNamedItemNS(getAttributes(arg), &
          str_vs(oldattr%namespaceURI), str_vs(oldattr%localName))
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

    attr%ownerElement => null()

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
    do i = 1, arg%attributes%length
      if (str_vs(arg%attributes%nodes(i)%this%nodeName)==name) then
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
    do i = 1, arg%attributes%length
      if (str_vs(arg%attributes%nodes(i)%this%namespaceURI)==namespaceURI &
        .and. str_vs(arg%attributes%nodes(i)%this%localName)==localName) then
        p = .true.
        exit
      endif
    enddo

  end function hasAttributeNS

! setIdAttribute
! setIdAttributeNS
! setIdAttributeNode

')`'dnl

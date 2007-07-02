TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_namecheck, only: prefixOfQName, localpartOfQName

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

  TOHW_function(getTagName, (element), c)
    type(Node), intent(in) :: element   
    character(len=size(element%nodeName)) :: c

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    c = str_vs(element%nodeName)    
     
  end function getTagName

    
  TOHW_function(getAttribute, (element, name), c)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: name
    character(len=getNamedItem_Value_length(element%attributes, name)) :: c

    type(Node), pointer :: nn

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    c = ""  ! as per specs, if not found
    c = getNamedItem_Value(element%attributes, name)

    ! FIXME do we need to catch the exception above if it doesnt exist?
        
  end function getAttribute


  TOHW_subroutine(setAttribute, (element, name, value))
    type(Node), pointer :: element
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    type(Node), pointer :: nn, dummy
    logical :: quickFix

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)

      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.checkChars(name, element%ownerDocument%docType%xds%xml_version)) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkName(value, element%ownerDocument%docType%xds)) then
      TOHW_m_dom_throw_error(FoX_INVALID_XML_NAME)
    elseif (.not.checkChars(value, element%ownerDocument%docType%xds%xml_version)) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif

    quickFix = .not.element%ownerDocument%xds%building &
      .and. element%inDocument

    if (quickFix) call setDocBuilding(element%ownerDocument, .true.)
    ! then the created attribute is going straight into the document,
    ! so dont faff with hanging-node lists.

    nn => createAttribute(element%ownerDocument, name)
    call setValue(nn, value)

    dummy => setNamedItem(element%attributes, nn)
    nn%ownerElement => element

    if (quickFix) call setDocBuilding(element%ownerDocument, .true.)

  end subroutine setAttribute


  TOHW_subroutine(removeAttribute, (element, name))
    type(Node), pointer :: element
    character(len=*), intent(in) :: name

    type(Node), pointer :: dummy

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    if (element%inDocument) &
      call setDocBuilding(element%ownerDocument, .true.)

    dummy => removeNamedItem(element%attributes, name)
    print*,"DESTROYING ATTRIBUTE:"
    call destroyAllNodesRecursively(dummy)
    call destroyNode(dummy)

    if (element%inDocument) &
      call setDocBuilding(element%ownerDocument, .false.)

  ! FIXME recreate a default value if there is one
     
  end subroutine removeAttribute


  TOHW_function(getAttributeNode, (element, name), attr)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: name
    type(Node), pointer :: attr

    attr => null()     ! as per specs, if not foundo

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    attr => getNamedItem(element%attributes, name)

    ! FIXME catch and throw away exception

  end function getAttributeNode
  

  TOHW_function(setAttributeNode, (element, newattr), attr)
    type(Node), pointer :: element
    type(Node), pointer :: newattr
    type(Node), pointer :: attr
    type(Node), pointer :: dummy

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.associated(element%ownerDocument, newattr%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    elseif (element%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (associated(attr%ownerElement)) then
      TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)
    endif

    ! this checks if attribute exists already
    ! It also does any adding/removing of hangingnodes
    dummy => setNamedItem(element%attributes, newattr, ex)
    attr%ownerElement => element

  end function setAttributeNode


  TOHW_function(removeAttributeNode, (element, oldattr), attr)
    type(Node), pointer :: element
    type(Node), pointer :: oldattr
    type(Node), pointer :: attr

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (element%readonly) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    do i = 1, element%attributes%length
      if (associated(item(element%attributes, i), oldattr)) then
        attr => removeNamedItem(element%attributes, str_vs(oldattr%nodeName))
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

    attr%ownerElement => null()

    ! FIXME hangingnodes

  end function removeAttributeNode


!  function getElementsByTagName - see m_dom_document


  TOHW_function(getAttributeNS, (element, namespaceURI, localName), c)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    character(len= &
      getNamedItemNS_Value_length(element%attributes, namespaceURI, localName)) :: c

    type(Node), pointer :: nn

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = ""  ! as per specs, if not found Not sure ahout this FIXME
    c = getNamedItemNS_Value(element%attributes, namespaceURI, localName)

    ! FIXME dont need both above
        
  end function getAttributeNS


  TOHW_subroutine(setAttributeNS, (element, namespaceURI, qualifiedname, value))
    type(Node), pointer :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: qualifiedName
    character(len=*), intent(in) :: value

    type(Node), pointer :: nn, dummy
    logical :: quickfix

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(qualifiedname, element%ownerDocument%docType%xds%xml_version)) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (element%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.checkQName(qualifiedname, element%ownerDocument%docType%xds)) then
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

    quickFix = .not.element%ownerDocument%xds%building &
      .and. element%inDocument

    if (quickFix) call setDocBuilding(element%ownerDocument, .true.)
    ! then the created attribute is going straight into the document,
    ! so dont faff with hanging-node lists.

    nn => createAttributeNS(element%ownerDocument, namespaceURI, qualifiedname)
    call setValue(nn, value)

    dummy => setNamedItemNS(element%attributes, nn)
    nn%ownerElement => element

    if (quickFix) call setDocBuilding(element%ownerDocument, .true.)

    !FIXME catch exception

  end subroutine setAttributeNS


  TOHW_subroutine(removeAttributeNS, (element, namespaceURI, localName))
    type(Node), pointer :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName

    type(Node), pointer :: dummy

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (element%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    dummy => removeNamedItemNS(element%attributes, namespaceURI, localName)

    call destroyAttribute(dummy)
     
  end subroutine removeAttributeNS


  TOHW_function(getAttributeNodeNS, (element, namespaceURI, localName), attr)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(Node), pointer :: attr

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    attr => null()     ! as per specs, if not found
    attr => getNamedItemNS(element%attributes, namespaceURI, localname)

  end function getAttributeNodeNS
  

  TOHW_function(setAttributeNodeNS, (element, newattr), attr)
    type(Node), pointer :: element
    type(Node), pointer :: newattr
    type(Node), pointer :: attr
    type(Node), pointer :: dummy

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.associated(element%ownerDocument, newattr%ownerDocument)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    elseif (element%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (associated(attr%ownerElement)) then
      TOHW_m_dom_throw_error(INUSE_ATTRIBUTE_ERR)
    endif

    ! this checks if attribute exists already
    dummy => setNamedItemNS(element%attributes, newattr)
    attr%ownerElement => element

    ! FIXME hangingnodes

  end function setAttributeNodeNS


  TOHW_function(removeAttributeNodeNS, (element, oldattr), attr)
    type(Node), pointer :: element
    type(Node), pointer :: oldattr
    type(Node), pointer :: attr

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (element%readonly) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    do i = 1, element%attributes%length
      if (associated(item(element%attributes, i), oldattr)) then
        attr => removeNamedItemNS(element%attributes, &
          str_vs(oldattr%namespaceURI), str_vs(oldattr%localName))
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

    attr%ownerElement => null()
    ! FIXME hangingnodes
  end function removeAttributeNodeNS


!  function getElementsByTagNameNS - see m_dom_document


  TOHW_function(hasAttribute, (element, name), p)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: name
    logical :: p

    integer :: i
 
   if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    p = .false.
    do i = 1, element%attributes%length
      if (str_vs(element%attributes%nodes(i)%this%nodeName)==name) then
        p = .true.
        exit
      endif
    enddo

  end function hasAttribute


  TOHW_function(hasAttributeNS, (element, namespaceURI, localName), p)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    logical :: p

    integer :: i

 
   if (element%nodeType /= ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    p = .false.
    do i = 1, element%attributes%length
      if (str_vs(element%attributes%nodes(i)%this%namespaceURI)==namespaceURI &
        .and. str_vs(element%attributes%nodes(i)%this%localName)==localName) then
        p = .true.
        exit
      endif
    enddo

  end function hasAttributeNS

! setIdAttribute
! setIdAttributeNS
! setIdAttributeNode

')`'dnl

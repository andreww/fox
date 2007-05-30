module m_dom_element

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_namecheck, only: prefixOfQName, localpartOfQName

  use m_dom_types, only: Node, NodeList, NamedNode
  use m_dom_types, only: destroyNode
  use m_dom_types, only: DOCUMENT_NODE, ELEMENT_NODE, TEXT_NODE
  
  use m_dom_namednodemap, only: getNamedItem, setNamedItem, removeNamedItem
  use m_dom_namednodemap, only: getNamedItem_Value, getNamedItem_Value_length
  use m_dom_namednodemap, only: getNamedItemNS, setNamedItemNS, removeNamedItemNS
  use m_dom_namednodemap, only: item

  use m_dom_attribute, only: getValue, setValue, destroyAttribute
  
  use m_dom_document, only: createAttribute, createAttributeNS
  use m_dom_debug, only: dom_debug
  
  implicit none
  private

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

contains

  function getTagName(element) result(c)
    type(Node), intent(in) :: element   
    character(len=size(element%nodeName)) :: c
    
    if (element%nodeType == ELEMENT_NODE) then
      c = str_vs(element%nodeName )
    else
      c = '' ! FIXME error
    endif
    
  end function getTagName

    
  function getAttribute(element, name) result(c)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: name
    character(len=getNamedItem_Value_length(element%attributes, name)) :: c

    type(Node), pointer :: nn

    c = ""  ! as per specs, if not found Not sure ahout this FIXME
    if (element%nodeType /= ELEMENT_NODE) return ! or throw an error FIXME?
    c = getNamedItem_Value(element%attributes, name)

    ! FIXME catch exception
        
  end function getAttribute


  subroutine setAttribute(element, name, value)
    type(Node), intent(inout) :: element
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    type(Node), pointer :: nn

    if (element%nodeType /= ELEMENT_NODE) return ! or throw an error FIXME?

    nn => createAttribute(element%ownerDocument, name)
    call setValue(nn, value)
    nn => setNamedItem(element%attributes, nn)

    ! FIXME catch exception

  end subroutine setAttribute


  subroutine removeAttribute(element, name)
    type(Node), pointer :: element
    character(len=*), intent(in) :: name

    type(Node), pointer :: dummy

    if (element % nodeType /= ELEMENT_NODE) return
    ! WHat about remove text ...

    dummy => removeNamedItem(element%attributes, name)
    ! FIXME and free memory from dummy
    ! call destroyAttribute(dummy)
     
  end subroutine removeAttribute


  function getAttributeNode(element, name) result(attr)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: name
    type(Node), pointer :: attr

    attr => null()     ! as per specs, if not found
    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif
    attr => getNamedItem(element%attributes, name)

    ! FIXME catch and throw awaye xception

  end function getAttributeNode
  

  function setAttributeNode(element, newattr) result(attr)
    type(Node), pointer :: element
    type(Node), pointer :: newattr
    type(Node), pointer :: attr

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif

    ! this checks if attribute exists already
    attr => setNamedItem(element%attributes, newattr)
  end function setAttributeNode


  function removeAttributeNode(element, oldattr) result(attr)
    type(Node), pointer :: element
    type(Node), pointer :: oldattr
    type(Node), pointer :: attr

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif

    do i = 1, element%attributes%list%length
      if (associated(item(element%attributes, i), oldattr)) then
        attr => removeNamedItem(element%attributes, str_vs(oldattr%nodeName))
        return
      endif
    enddo

    ! FIXME exceptions

  end function removeAttributeNode


!  function getElementsByTagName - see m_dom_document


  function getAttributeNS(element, namespaceURI, localname) result(c)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    character(len=100) :: c ! FIXME

    type(Node), pointer :: nn

    c = ""  ! as per specs, if not found Not sure ahout this FIXME
    if (element%nodeType /= ELEMENT_NODE) return ! or throw an error FIXME?
    c = getValue(getNamedItemNS(element%attributes, namespaceURI, localName))

    ! FIXME catch exception
        
  end function getAttributeNS


  subroutine setAttributeNS(element, namespaceURI, localname, value)
    type(Node), intent(inout) :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localname
    character(len=*), intent(in) :: value

    type(Node), pointer :: nn

    if (element%nodeType /= ELEMENT_NODE) return ! or throw an error FIXME?

    nn => createAttributeNS(element%ownerDocument, namespaceURI, localname)
    call setValue(nn, value)
    nn => setNamedItemNS(element%attributes, nn)

    ! FIXME catch exception

  end subroutine setAttributeNS


  subroutine removeAttributeNS(element, namespaceURI, localName)
    type(Node), pointer :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName

    type(Node), pointer :: dummy

    if (element % nodeType /= ELEMENT_NODE) return
    ! WHat about remove text ...
    dummy => removeNamedItemNS(element%attributes, namespaceURI, localName)
    ! FIXME and clean up memory
    ! call destroyAttribute(dummy)
     
  end subroutine removeAttributeNS


  function getAttributeNodeNS(element, namespaceURI, localName) result(attr)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    type(Node), pointer :: attr

    attr => null()     ! as per specs, if not found
    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif
    attr => getNamedItemNS(element%attributes, namespaceURI, localname)

    ! FIXME catch and throw awaye xception

  end function getAttributeNodeNS
  

  function setAttributeNodeNS(element, newattr) result(attr)
    type(Node), pointer :: element
    type(Node), pointer :: newattr
    type(Node), pointer :: attr

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif

    ! this checks if attribute exists already
    attr => setNamedItemNS(element%attributes, newattr)
  end function setAttributeNodeNS


  function removeAttributeNodeNS(element, oldattr) result(attr)
    type(Node), pointer :: element
    type(Node), pointer :: oldattr
    type(Node), pointer :: attr

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif

    do i = 1, element%attributes%list%length
      if (associated(item(element%attributes, i), oldattr)) then
        attr => removeNamedItemNS(element%attributes, &
          str_vs(oldattr%namespaceURI), str_vs(oldattr%localName))
        return
      endif
    enddo

    ! FIXME exceptions

  end function removeAttributeNodeNS


!  function getElementsByTagName - see m_dom_document


  function hasAttribute(element, name) result(p)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: name
    logical :: p

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif

    p = .false.
    do i = 1, element%attributes%list%length
      if (str_vs(element%attributes%list%nodes(i)%this%nodeName)==name) then
        p = .true.
        exit
      endif
    enddo

  end function hasAttribute


  function hasAttributeNS(element, namespaceURI, localName) result(p)
    type(Node), intent(in) :: element
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: localName
    logical :: p

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif

    p = .false.
    do i = 1, element%attributes%list%length
      if (str_vs(element%attributes%list%nodes(i)%this%namespaceURI)==namespaceURI &
        .and. str_vs(element%attributes%list%nodes(i)%this%localName)==localName) then
        p = .true.
        exit
      endif
    enddo

  end function hasAttributeNS


  subroutine destroyElement(element)
    type(Node), pointer :: element

    integer :: i

    if (element%nodeType /= ELEMENT_NODE) then
      ! FIXME error
    endif

    do i = 1, element%attributes%list%length
      call destroyAttribute(element%attributes%list%nodes(i)%this)
    enddo

    call destroyNode(element)

  end subroutine destroyElement

end module m_dom_element

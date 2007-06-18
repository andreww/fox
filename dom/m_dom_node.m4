include(`m_dom_exception.m4')`'dnl
TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_dom_error, only: DOMException, throw_exception, is_in_error, &
    NO_MODIFICATION_ALLOWED_ERR, NOT_FOUND_ERR, HIERARCHY_REQUEST_ERR, &
    WRONG_DOCUMENT_ERR, dom_error

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
    if (arg%nodeType == ATTRIBUTE_NODE) then
      ! destroy children
      ! rebuild new children
    endif
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
    type(Node), pointer :: arg
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

    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i_t

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

    if (.not.(associated(arg%ownerDocument, newChild%ownerDocument) &
      .or. associated(arg, newChild%ownerDocument))) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    if (associated(newChild%parentNode)) &
      newChild => removeChild(newChild%parentNode, newChild, ex)
    
    if (.not.associated(refChild)) then
      insertBefore => appendChild(arg, newChild, ex)
      return
    endif

    allocate(temp_nl(size(arg%childNodes%nodes)+1))
    i_t = 1
    do i = 1, size(arg%childNodes%nodes)
      if (associated(arg%childNodes%nodes(i)%this, refChild)) then 
        i_t = i_t + 1
        temp_nl(i_t)%this => newChild
        newChild%parentNode => arg
        if (i==0) then
          arg%firstChild => newChild
          newChild%previousSibling => null()
        else
          newChild%previousSibling => arg%childNodes%nodes(i-1)%this
          arg%childNodes%nodes(i-1)%this%nextSibling => newChild
        endif
        if (i==size(arg%childNodes%nodes)) then
          arg%lastChild => newChild
          newChild%nextSibling => null()
        else
          newChild%nextSibling => arg%childNodes%nodes(i+1)%this
          arg%childNodes%nodes(i+1)%this%previousSibling => newChild
        endif
      endif
      temp_nl(i_t)%this => arg%childNodes%nodes(i)%this
      i_t = i_t + 1     
    enddo
    deallocate(arg%childNodes%nodes)
    arg%childNodes%nodes => temp_nl

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

  end function insertBefore
  

  TOHW_function(replaceChild, (arg, newChild, oldChild), np)
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: oldChild
    type(Node), pointer :: np

    integer :: i
    
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

    if (.not.(associated(arg%ownerDocument, newChild%ownerDocument) &
      .or. associated(arg, newChild%ownerDocument))) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    if (associated(newChild%parentNode)) then
      newChild => removeChild(newChild%parentNode, newChild, ex)
    elseif (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      !FIXME

    endif

    do i = 1, size(arg%childNodes%nodes)
      if (associated(arg%childNodes%nodes(i)%this, oldChild)) then
        np => oldChild
        if (i==0) then
          arg%firstChild => newChild
          newChild%previousSibling => null()
        else 
          arg%childNodes%nodes(i-1)%this%nextSibling => newChild
          newChild%previousSibling => arg%childNodes%nodes(i-1)%this
        endif
        arg%childNodes%nodes(i)%this => newChild
        newChild%parentNode => arg 
        if (i==size(arg%childNodes%nodes)) then
          arg%lastChild => newChild
          newChild%nextSibling => null()
        else
          arg%childNodes%nodes(i+1)%this%previousSibling => newChild
          newChild%nextSibling => arg%childNodes%nodes(i+1)%this
        endif
        return
      endif
    enddo

    TOHW_m_dom_throw_error(NOT_FOUND_ERR)

    np%parentNode => null()
    np%previousSibling => null()
    np%nextSibling => null()

  end function replaceChild


  TOHW_function(removeChild, (arg, oldChild), np)
    type(Node), pointer :: arg
    type(Node), pointer :: oldChild
    type(Node), pointer :: np

    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i_t

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not.associated(arg)) call dom_error("removeChild",0,"Node not allocated")
    
    allocate(temp_nl(size(arg%childNodes%nodes)-1))
    i_t = 1
    do i = 1, size(arg%childNodes%nodes)
      if (associated(arg%childNodes%nodes(i)%this, oldChild)) then 
        if (i==0) then
          np => arg%childNodes%nodes(i)%this
          if (size(arg%childNodes%nodes) == 1) then
            arg%firstChild => null()
            arg%lastChild => null()
            exit
          else
            arg%firstChild => arg%childNodes%nodes(1)%this
            arg%childNodes%nodes(1)%this%previousSibling => null()
          endif
        else
          arg%childNodes%nodes(i+1)%this%previousSibling => arg%childNodes%nodes(i-1)%this
        endif
        if (i==size(arg%childNodes%nodes)) then
          arg%lastChild => arg%childNodes%nodes(i-1)%this
          arg%childNodes%nodes(i-1)%this%nextSibling => null()
        endif
      else
        temp_nl(i_t)%this => arg%childNodes%nodes(i)%this
        i_t = i_t + 1     
      endif
    enddo
    deallocate(arg%childNodes%nodes)
    arg%childNodes%nodes => temp_nl

    if (i==i_t) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR)
    endif

    np%parentNode => null()
    np%previousSibling => null()
    np%nextSibling => null()

  end function removeChild


  TOHW_function(appendChild, (arg, newChild))
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: appendChild
    
    type(ListNode), pointer :: temp_nl(:)
    integer :: i

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

    if (.not.(associated(arg%ownerDocument, newChild%ownerDocument) &
      .or. associated(arg, newChild%ownerDocument))) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    if (associated(newChild%parentNode)) &
      newChild => removeChild(newChild%parentNode, newChild, ex) 

    allocate(temp_nl(size(arg%childNodes%nodes)+1))
    do i = 1, size(arg%childNodes%nodes)
      temp_nl(i)%this => arg%childNodes%nodes(i)%this
    enddo
    temp_nl(i)%this => newChild
    temp_nl(i-1)%this%nextSibling => newChild
    newChild%previousSibling => temp_nl(i)%this
    newChild%nextSibling => null()

    deallocate(arg%childNodes%nodes)
    arg%childNodes%nodes => temp_nl

    arg%lastChild => newChild
    newChild%parentNode => arg
    
    appendChild => newChild
    
  end function appendChild


  function hasChildNodes(arg)
    type(Node), pointer :: arg
    logical :: hasChildNodes
    
    if (.not. associated(arg)) call dom_error("hasChildNodes",0,"Node not allocated")
    hasChildNodes = associated(arg%firstChild)
    
  end function hasChildNodes

  TOHW_function(cloneNode, (arg, deep), np)
    type(Node), pointer :: arg
    logical :: deep
    type(Node), pointer :: np

    type(Node), pointer :: np_a1, np_a2, this, that, new, ERchild
    type(NamedNodeMap), pointer :: nnm

    logical :: noChild, readonly
    integer :: i

    noChild = .false.
    readonly = .false.
    
    ERchild => null()
    this => arg
    do
      if (noChild) then
        if (associated(this, arg)) exit
        if (associated(this, ERchild)) then
          ! Weve got back up to the top of the topmost ER.
          readonly = .false.
          ERchild => null()
        endif
        if (associated(this%nextSibling)) then
          this => this%nextSibling
          noChild = .false.
        else
          this => this%parentNode
          that => that%parentNode
          cycle
        endif
      endif
      select case(this%nodeType)
        case (ELEMENT_NODE)
          new => createElementNS(this%ownerDocument, &
            str_vs(this%namespaceURI), str_vs(this%localName))
          ! loop over attributes cloning them
          nnm => getAttributes(this)
          do i = 1, getLength(nnm)
            np_a1 => item(nnm, i)
            np_a2 => createAttributeNS(this%ownerDocument, &
              str_vs(np_a1%namespaceURI), str_vs(np_a1%localName))
            call setValue(new, getValue(np_a1))
            np_a2%specified = np_a1%specified
            np_a2 => setAttributeNodeNS(np, np_a2)
          end do
        case (ATTRIBUTE_NODE)
          new => createAttributeNS(this%ownerDocument, &
            str_vs(this%namespaceURI), str_vs(this%localName))
          call setValue(new, getValue(np_a2))
          new%specified = .true.
        case (TEXT_NODE)
          new => createTextNode(this%ownerDocument, str_vs(this%nodeValue))
        case (CDATA_SECTION_NODE)
          new => createCdataSection(this%ownerDocument, str_vs(this%nodeValue))
        case (ENTITY_REFERENCE_NODE)
          new => createEntityReference(this%ownerDocument, str_vs(this%nodeName))
          ERChild => this
        case (ENTITY_NODE)
          new => null()
        case (PROCESSING_INSTRUCTION_NODE)
          new => createProcessingInstruction(this%ownerDocument, &
            str_vs(this%nodeName), str_vs(this%nodeValue))
        case (COMMENT_NODE)
          new => createComment(this%ownerDocument, str_vs(this%nodeValue))
        case (DOCUMENT_NODE)
          new => null()
        case (DOCUMENT_FRAGMENT_NODE)
          new => createDocumentFragment(this%ownerDocument)
        case (NOTATION_NODE)
          new => null()
        end select
        ! Sort out readonly-ness
        if (readonly) then
          that%readonly = .true. ! We are under a readonly tree
        elseif (associated(ERChild)) then
          readonly = .true. ! This is not readonly, but all nodes below will be
        endif
        ! Append the new node to the tree
        if (associated(this, arg)) then
          that => new ! This is the first we have created, head of the tree
          if (.not.deep) exit ! We only wanted one node anyway
        else
          new => appendChild(that, new)
        endif
        ! Do we continue descending?
        if (associated(this%firstChild)) then
          if (.not.associated(this, arg)) &
            that => that%lastChild
          this => this%firstChild
        else
          noChild = .true.
        endif
      enddo

      np => that

  end function cloneNode

  
  function hasAttributes(arg)
    type(Node), pointer :: arg
    logical :: hasAttributes
    
    if (.not.associated(arg)) call dom_error("hasAttributes",0,"Node not allocated")
    hasAttributes = (arg%nodeType /= ELEMENT_NODE) &
      .and. (arg%attributes%list%length > 0)
    
  end function hasAttributes
  
  recursive TOHW_subroutine(normalize, (arg))
    type(Node), pointer :: arg
  ! NB only ever one level of recursion, for text children of the attributes of an element

    type(Node), pointer :: this, tempNode
    type(NamedNodeMap), pointer :: nnm
    integer :: i
    logical :: noChild
    character, pointer :: temp(:)
    
    ! This ignores readonly status according to the DOM standard.

    noChild = .false.
    do
      if (noChild) then
        if (associated(this, arg)) exit
        if (associated(this%nextSibling)) then
          this => this%nextSibling
          noChild = .false.
        else
          this => this%parentNode
          cycle
        endif
      endif
      if (associated(tempNode)) then
        tempNode => removeChild(tempNode%parentNode, tempNode)
        tempNode => null()
      endif
      if (this%nodeType==ELEMENT_NODE.and.hasAttributes(this)) then
        ! Loop over attributes combining them ...
        nnm => getAttributes(this)
        do i = 1, getLength(nnm)
          tempNode => item(nnm, i)
          call normalize(tempNode)
        enddo
        tempNode => null()
      elseif (this%nodeType==TEXT_NODE.and.associated(this%nextSibling)) then
        ! Keep going until all adjacent TEXT_NODEs are consumed.
        do while (this%nextSibling%nodeType==TEXT_NODE) 
          temp => this%nodeValue
          this%nodeValue => vs_str_alloc(str_vs(temp)//getData(this%nextSibling))
	  deallocate(temp)
          tempNode => removeChild(this%parentNode, this%nextSibling)
        enddo
        tempNode => null()
        if (size(this%nodeValue)==0) &
          tempNode => this
      endif
      if (associated(this%firstChild)) then
        this => this%firstChild
      else
        noChild = .true.
      endif
    enddo
 
    tempNode => removeChild(tempNode%parentNode, tempNode)

  end subroutine normalize

  function isSupported(arg, feature, version) result(p)
    type(Node), intent(in) :: arg
    character(len=*), intent(in) :: feature
    character(len=*), intent(in) :: version
    logical :: p

    p = hasFeature(feature, version)
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

    print*, "why are you doing this?"
    ! FIXME we should implement this but raise a FoX-specific exception if used
    stop
    ! FIXME exceptions
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

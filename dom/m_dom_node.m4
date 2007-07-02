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

  pure function getNodeValue_len(arg) result(n)
    type(Node), intent(in) :: arg
    integer :: n

    integer :: i

    select case(arg%nodeType)
    case (ATTRIBUTE_NODE)
      n = 0
      do i = 1, arg%childNodes%length
        if (arg%childNodes%nodes(i)%this%nodeType == TEXT_NODE) then
          n = n + size(arg%childNodes%nodes(i)%this%nodeValue)
        else
          !FIXME replace entity references
        endif
      enddo
    case (CDATA_SECTION_NODE)
      n = size(arg%nodeValue)
    case (COMMENT_NODE)
      n = size(arg%nodeValue)
    case (PROCESSING_INSTRUCTION_NODE)
      n = size(arg%nodeValue)
    case (TEXT_NODE)
      n = size(arg%nodeValue)
    case default
      n = 0
    end select

  end function getNodeValue_len

  function getNodeValue(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=getNodeValue_len(arg)) :: c

    integer :: i, n

    select case(arg%nodeType)
    case (ATTRIBUTE_NODE)
      n = 1
      do i = 1, arg%childNodes%length
        if (arg%childNodes%nodes(i)%this%nodeType == TEXT_NODE) then
          c(n:n+size(arg%childNodes%nodes(i)%this%nodeValue)-1) = &
            str_vs(arg%childNodes%nodes(i)%this%nodeValue)
          n = n + size(arg%childNodes%nodes(i)%this%nodeValue)
        else
          !FIXME replace entity references
        endif
      enddo
    case (CDATA_SECTION_NODE)
      c = str_vs(arg%nodeValue)
    case (COMMENT_NODE)
      c = str_vs(arg%nodeValue)
    case (PROCESSING_INSTRUCTION_NODE)
      c = str_vs(arg%nodeValue)
    case (TEXT_NODE)
      c = str_vs(arg%nodeValue)
    end select
    
  end function getNodeValue
  
  TOHW_subroutine(setNodeValue, (arg, nodeValue))
    type(Node), pointer :: arg
    character(len=*) :: nodeValue

    type(Node), pointer :: np
    integer :: i, n

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    select case(arg%nodeType)
    case (ATTRIBUTE_NODE)
      ! FIXME check does string contain wrong characters
      ! destroy any existing children ... 
      do i = 1, arg%childNodes%length
        if (.not.arg%inDocument) &
          call remove_node_nl(arg%ownerDocument%hangingNodes, arg%childNodes%nodes(i)%this)
        call destroyNode(arg%childNodes%nodes(i)%this)
      enddo
      deallocate(arg%childNodes%nodes)
      allocate(arg%childNodes%nodes(0))
      arg%childNodes%length = 0
      arg%firstChild => null()
      arg%lastChild => null()
      ! and add the new one.
      ! Avoid manipulaing hangingnode lists
      call setDocBuilding(arg%ownerDocument, .true.)
      np => createTextNode(arg%ownerDocument, nodeValue)
      np => appendChild(arg, np)
      call setDocBuilding(arg%ownerDocument, .false.)
    case (CDATA_SECTION_NODE)
      ! FIXME check does string contain wrong characters
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(nodeValue)
    case (COMMENT_NODE)
      ! FIXME check does string contain wrong characters
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(nodeValue)
    case (PROCESSING_INSTRUCTION_NODE)
      ! FIXME check does string contain wrong characters
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(nodeValue)
    case (TEXT_NODE)
      ! FIXME check does string contain wrong characters
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(nodeValue)
    end select

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
    type(Node), pointer :: arg
    type(NamedNodeMap), pointer :: nnm

! FIXME surely only if this is an element node?

    nnm => arg%attributes
  end function getAttributes

  function getOwnerDocument(arg) result(np)
    type(Node), intent(in) :: arg
    type(Node), pointer :: np
    
    if (np%nodeType==DOCUMENT_NODE) then
      np => null()
    else
      np => arg%ownerDocument
    endif
  end function getOwnerDocument

  TOHW_function(insertBefore, (arg, newChild, refChild))
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: refChild
    type(Node), pointer :: insertBefore

    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i_t

    ! FIXME DOCUMENTFRAGMENT

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
        if (i==1) then
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
    if (i_t == i) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR, (temp_nl))
    endif

    deallocate(arg%childNodes%nodes)
    arg%childNodes%nodes => temp_nl
    arg%childNodes%length = size(temp_nl)

    if (.not.arg%ownerDocument%xds%building) then
      if (arg%inDocument) then
        call putNodesInDocument(arg%ownerDocument, newChild)
      endif
    endif

  end function insertBefore


  TOHW_function(replaceChild, (arg, newChild, oldChild), np)
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: oldChild
    type(Node), pointer :: np

    integer :: i

! FIXME DocFrag argument
    
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
        if (i==1) then
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

    ! FIXME updateNodeLists(*) in case of children
    ! but only if we are replacing a child of the document
    if (.not.arg%ownerDocument%xds%building) then
      if (arg%inDocument) then
        call removeNodesFromDocument(arg%ownerDocument, oldChild)
      endif
    endif
    if (.not.arg%ownerDocument%xds%building) then
      if (arg%inDocument) then
        call putNodesInDocument(arg%ownerDocument, newChild)
      endif
    endif

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
        np => arg%childNodes%nodes(i)%this
        if (associated(arg%firstChild, arg%lastChild)) then
          ! There is only one child, we are removing it.
          arg%firstChild => null()
          arg%lastChild => null()
        elseif (i==1) then
          ! We are removing the first child, but there is a second
          arg%firstChild => arg%childNodes%nodes(2)%this
          arg%childNodes%nodes(2)%this%previousSibling => null()
        elseif (i==size(arg%childNodes%nodes)) then
          ! We are removing the last child, but there is a second-to-last
          arg%lastChild => arg%childNodes%nodes(i-1)%this
          arg%childNodes%nodes(i-1)%this%nextSibling => null()
        else
          ! We are removing a child in the middle
          arg%childNodes%nodes(i-1)%this%nextSibling => arg%childNodes%nodes(i+1)%this
          arg%childNodes%nodes(i+1)%this%previousSibling => arg%childNodes%nodes(i-1)%this
        endif
      else
        temp_nl(i_t)%this => arg%childNodes%nodes(i)%this
        i_t = i_t + 1     
      endif
    enddo
    deallocate(arg%childNodes%nodes)
    arg%childNodes%nodes => temp_nl
    arg%childNodes%length = size(temp_nl)

    if (i==i_t) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR)
    endif

    np%parentNode => null()
    np%previousSibling => null()
    np%nextSibling => null()

    if (.not.arg%ownerDocument%xds%building) then
      if (arg%inDocument) then
        call removeNodesFromDocument(arg%ownerDocument, oldChild)
      endif
    endif

  end function removeChild


  TOHW_function(appendChild, (arg, newChild))
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: appendChild
    
    type(Node), pointer :: testChild, testParent
    type(ListNode), pointer :: temp_nl(:)
    integer :: i

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not. associated(arg))  & 
      call dom_error("appendChild",0,"Node not allocated")

    testParent => arg    
    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      do i = 1, newChild%childNodes%length
        testChild => newChild%childNodes%nodes(i)%this
        TOHW_m_dom_hierarchy_test
      enddo
    else
      testChild => newChild
      TOHW_m_dom_hierarchy_test
    endif

    if (.not.(associated(arg%ownerDocument, newChild%ownerDocument) &
      .or. associated(arg, newChild%ownerDocument))) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE &
      .and. newChild%childNodes%length==0) return
    ! Nothing to do

    if (associated(newChild%parentNode)) &
      newChild => removeChild(newChild%parentNode, newChild, ex) 

    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      allocate(temp_nl(arg%childNodes%length+newChild%childNodes%length))
    else
      allocate(temp_nl(arg%childNodes%length+1))
    endif

    do i = 1, arg%childNodes%length
      temp_nl(i)%this => arg%childNodes%nodes(i)%this
    enddo
    
    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      do i = arg%childNodes%length+1, arg%childNodes%length+newChild%childNodes%length
        temp_nl(i)%this => newChild%childNodes%nodes(i)%this
        if (arg%inDocument) &
          call putNodesInDocument(arg%ownerDocument, newChild%childNodes%nodes(i)%this)
        newChild%childNodes%nodes(i)%this%parentNode => arg
      enddo
      if (arg%childNodes%length==0) then
        arg%firstChild => newChild%firstChild
      else
        newChild%firstChild%previousSibling => arg%lastChild
        arg%lastChild%nextSibling => newChild%firstChild
      endif
      arg%lastChild => newChild%lastChild
      newChild%firstChild => null()
      newChild%lastChild => null()
      deallocate(newChild%childNodes%nodes)
      allocate(newChild%childNodes%nodes(0))
      newChild%childNodes%length = 0
    else
      temp_nl(i)%this => newChild
      if (i==1) then
        arg%firstChild => newChild
        newChild%previousSibling => null()
      else
        temp_nl(i-1)%this%nextSibling => newChild
        newChild%previousSibling => temp_nl(i-1)%this     
      endif
      if (.not.arg%ownerDocument%xds%building) then
        if (arg%inDocument.and..not.newChild%inDocument) then
          print*,"ADDHN", associated(arg%ownerDocument%hangingnodes%nodes(arg%ownerDocument%hangingnodes%length)%this)
          call putNodesInDocument(arg%ownerDocument, newChild)
          print*,"ADDHN", associated(arg%ownerDocument%hangingnodes%nodes(arg%ownerDocument%hangingnodes%length)%this)
        endif
      endif
      newChild%nextSibling => null()
      arg%lastChild => newChild
      newChild%parentNode => arg
    endif

    deallocate(arg%childNodes%nodes)
    arg%childNodes%nodes => temp_nl
    arg%childNodes%length = size(temp_nl)

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

    type(Node), pointer :: np_a1, np_a2, this, thatParent, new, ERchild
    type(NamedNodeMap), pointer :: nnm

    logical :: ascending, readonly
    integer :: i

    ascending = .false.
    readonly = .false.
    
    ERchild => null()
    this => arg

    do
      if (ascending) then
        this => this%parentNode
        if (associated(this, arg)) exit
        thatParent => thatParent%parentNode
        if (associated(this, ERchild)) then
          ! Weve got back up to the top of the topmost ER.
          readonly = .false.
          ERchild => null()
        endif
        ascending = .false.
      endif
      print*,"ASSOCIATEDNODE", associated(this), this%nodeType
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
          !FIXME what if the value is an entity reference?
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
      new%readonly = readonly
      if (associated(ERChild)) readonly = .true. ! This is not readonly, but all nodes below will be
      ! Append the new node to the tree
      if (associated(this, arg)) then
        thatParent => new ! This is the first we have created, head of the tree
        if (.not.deep) exit ! We only wanted one node anyway
      else
        new => appendChild(thatParent, new)
      endif
      ! Do we continue descending?
      if (associated(this%firstChild)) then
        this => this%firstChild
        if (.not.associated(this, arg)) &
          thatParent => thatParent%lastChild
      elseif (.not.associated(this, arg)) then
        if (associated(this%nextSibling)) then
          this => this%nextSibling
          ! but leave thatParent unchanged
        else
          ascending = .true.
        endif
      else ! the top node has no children, so we are finished
        exit
      endif
    enddo

    np => thatParent

  end function cloneNode

  
  function hasAttributes(arg)
    type(Node), pointer :: arg
    logical :: hasAttributes
    
    if (.not.associated(arg)) call dom_error("hasAttributes",0,"Node not allocated")
    hasAttributes = (arg%nodeType /= ELEMENT_NODE) &
      .and. (arg%attributes%length > 0)
    
  end function hasAttributes
  
  recursive TOHW_subroutine(normalize, (arg))
    type(Node), pointer :: arg
  ! NB only ever one level of recursion, for text children of the attributes of an element
    ! FIXME shoulndt be recursive
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

    p = hasFeature(arg%ownerDocument%implementation, feature, version)
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

  subroutine putNodesInDocument(doc, np_orig)
    type(Node), pointer :: doc, np_orig
    type(Node), pointer :: np
    logical :: ascending, attributesdone
    integer :: i

    print*,"PUTTING NODES IN DOCUMENT", doc%hangingNodes%length
    np => np_orig
    ascending = .false.
    attributesdone = .false.
    i = 0
    do
      if (ascending) then
        if (associated(np, np_orig)) exit
        ascending = .false.
        if (np%nodeType==ATTRIBUTE_NODE) then
          np => np%ownerElement
          attributesdone = .true.
          cycle
        else
          np => np%parentNode
        endif
      elseif (np%nodeType==ELEMENT_NODE.and..not.attributesdone) then
        if (np%attributes%length>0) then
          i = 1
          np => np%attributes%nodes(i)%this
        else
          attributesdone = .true.
        endif
        cycle
      elseif (associated(np%firstChild)) then
        np => np%firstChild
        attributesdone = .false.
        cycle
      endif
      np%inDocument = .true.
      call remove_node_nl(np%ownerDocument%hangingNodes, np)
      if (np%nodeType==ATTRIBUTE_NODE) then
        if (i==np%ownerElement%attributes%length) then
          ascending = .true.
        else
          i = i + 1
          np => np%ownerElement%attributes%nodes(i)%this
        endif
      elseif (associated(np%nextSibling)) then
        np => np%nextSibling
        attributesdone = .false.
      else
        ascending = .true.
      endif
    enddo
    print*,"DONE PUTTING NODES IN DOCUMENT", doc%hangingNodes%length
  end subroutine putNodesInDocument

  subroutine removeNodesFromDocument(doc, np_orig)
    type(Node), pointer :: doc, np_orig
    type(Node), pointer :: np
    logical :: ascending, attributesdone
    integer :: i
    np => np_orig
    ascending = .false.
    attributesdone = .false.
    i = 0
    do
      if (ascending) then
        if (associated(np, np_orig)) exit
        ascending = .false.
        if (np%nodeType==ATTRIBUTE_NODE) then
          np => np%ownerElement
          attributesdone = .true.
          cycle
        else
          np => np%parentNode
        endif
      elseif (np%nodeType==ELEMENT_NODE.and..not.attributesdone) then
        if (np%attributes%length>0) then
          i = 1
          np => np%attributes%nodes(i)%this
        else
          attributesdone = .true.
        endif
        cycle
      elseif (associated(np%firstChild)) then
        np => np%firstChild
        attributesdone = .false.
        cycle
      endif
      np%inDocument = .false.
      print*,"HNLEN", np%ownerDocument%hangingNodes%length
      print*,np%nodeType, str_vs(np%nodeName)
      call append_nl(np%ownerDocument%hangingNodes, np)
      print*,"HNLEN", np%ownerDocument%hangingNodes%length
      if (np%nodeType==ATTRIBUTE_NODE) then
        if (i==np%ownerElement%attributes%length) then
          ascending = .true.
        else
          i = i + 1
          np => np%ownerElement%attributes%nodes(i)%this
        endif
      elseif (associated(np%nextSibling)) then
        np => np%nextSibling
        attributesdone = .false.
      else
        ascending = .true.
      endif
    enddo
  end subroutine removeNodesFromDocument

')`'dnl

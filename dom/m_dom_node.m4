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
  public :: getNodeValue	
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
      ! Avoid manipulating hangingnode lists
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
    type(Node), pointer :: arg
    type(Node), pointer :: np
    
    if (arg%nodeType==DOCUMENT_NODE) then
      np => null()
    else
      np => arg%ownerDocument
    endif
  end function getOwnerDocument

  TOHW_function(insertBefore, (arg, newChild, refChild), np)
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: refChild
    type(Node), pointer :: np

    type(Node), pointer :: testChild, testParent
    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i2, i_t

    if (.not.associated(refChild)) then
      np => appendChild(arg, newChild, ex)
    endif

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not. associated(arg)) call dom_error("replaceChild",0,"Node not allocated")

    testParent => arg
    ! Check if you are allowed to put a newChild nodetype under a arg nodetype
    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      do i = 1, newChild%childNodes%length
        testChild => newChild%childNodes%nodes(i)%this
        TOHW_m_dom_hierarchy_test
      enddo
    else
      testChild => newChild
      TOHW_m_dom_hierarchy_test
      ! And then check that newChild is not one of args ancestors
      ! (this would never be true if newChild is a documentFragment)
      testParent => arg%parentNode
      do while (associated(testParent))
        if (associated(testParent, newChild)) then
          TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
        endif
        testParent => testParent%parentNode
      enddo
    endif

    if (.not.(associated(arg%ownerDocument, newChild%ownerDocument) &
      .or. associated(arg, newChild%ownerDocument))) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE &
      .and. newChild%childNodes%length==0) then
      np => newChild
      return
      ! Nothing to do
    endif

    if (associated(newChild%parentNode)) &
      newChild => removeChild(newChild%parentNode, newChild, ex) 

    if (arg%childNodes%length==0) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR)
    elseif (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      allocate(temp_nl(arg%childNodes%length+newChild%childNodes%length))
    else
      allocate(temp_nl(arg%childNodes%length+1))
    endif

    i_t = 0
    np => null()
    do i = 1, arg%childNodes%length
      if (associated(arg%childNodes%nodes(i)%this, refChild)) then
        np => refChild
        if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
          do i2 = 1, newChild%childNodes%length
            i_t = i_t + 1
            temp_nl(i_t)%this => newChild%childNodes%nodes(i2)%this
            temp_nl(i_t)%this%parentNode => arg
          enddo
        else
          i_t = i_t + 1
          temp_nl(i_t)%this => newChild
          temp_nl(i_t)%this%parentNode => arg
        endif
        if (i==1) then
          arg%firstChild => temp_nl(1)%this
          temp_nl(1)%this%previousSibling => null() ! FIXME no-op
        else 
          temp_nl(i-1)%this%nextSibling => temp_nl(i)%this
          temp_nl(i)%this%previousSibling => temp_nl(i-1)%this
        endif
        arg%childNodes%nodes(i)%this%previousSibling => temp_nl(i_t)%this
        temp_nl(i_t)%this%nextSibling => arg%childNodes%nodes(i)%this
      endif
      i_t = i_t + 1
      temp_nl(i_t)%this => arg%childNodes%nodes(i)%this
    enddo

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR)
    endif

    if (.not.arg%ownerDocument%xds%building) then
      if (arg%inDocument) then
        if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
          do i = 1, newChild%childNodes%length
            call putNodesInDocument(arg%ownerDocument, newChild%childNodes%nodes(i)%this)
          enddo
        else
          call putNodesInDocument(arg%ownerDocument, newChild)
        endif
        ! If newChild was originally in document, it was removed above so must be re-added FIXME
      endif
      ! If arg was not in the document, then newChildren were either 
      ! a) removed above in call to removeChild or
      ! b) in a document fragment and therefore not part of doc either
    endif

    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      deallocate(newChild%childNodes%nodes)
      allocate(newChild%childNodes%nodes(0))
      newChild%childNodes%length = 0
    endif
    deallocate(arg%childNodes%nodes)
    arg%childNodes%nodes => temp_nl
    arg%childNodes%length = size(arg%childNodes%nodes)

  end function insertBefore


  TOHW_function(replaceChild, (arg, newChild, oldChild), np)
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: oldChild
    type(Node), pointer :: np

    type(Node), pointer :: testChild, testParent
    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i2, i_t

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not. associated(arg)) call dom_error("replaceChild",0,"Node not allocated")

    testParent => arg
    ! Check if you are allowed to put a newChild nodetype under a arg nodetype
    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      do i = 1, newChild%childNodes%length
        testChild => newChild%childNodes%nodes(i)%this
        TOHW_m_dom_hierarchy_test
      enddo
    else
      testChild => newChild
      TOHW_m_dom_hierarchy_test
      ! And then check that newChild is not one of args ancestors
      ! (this would never be true if newChild is a documentFragment)
      testParent => arg%parentNode
      do while (associated(testParent))
        if (associated(testParent, newChild)) then
          TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
        endif
        testParent => testParent%parentNode
      enddo
    endif

    if (.not.(associated(arg%ownerDocument, newChild%ownerDocument) &
      .or. associated(arg, newChild%ownerDocument))) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    if (associated(newChild%parentNode)) &
      newChild => removeChild(newChild%parentNode, newChild, ex) 

    if (arg%childNodes%length==0) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR)
    elseif (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      allocate(temp_nl(arg%childNodes%length+newChild%childNodes%length-1))
    else
      temp_nl => arg%childNodes%nodes
    endif

    i_t = 0
    np => null()
    do i = 1, arg%childNodes%length
      if (associated(arg%childNodes%nodes(i)%this, oldChild)) then
        np => oldChild
        if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
          do i2 = 1, newChild%childNodes%length
            i_t = i_t + 1
            temp_nl(i_t)%this => newChild%childNodes%nodes(i2)%this
            temp_nl(i_t)%this%parentNode => arg
          enddo
        else
          i_t = i_t + 1
          temp_nl(i_t)%this => newChild
          temp_nl(i_t)%this%parentNode => arg
        endif
        if (i==1) then
          arg%firstChild => temp_nl(1)%this
          temp_nl(1)%this%previousSibling => null() ! FIXME no-op
        else 
          temp_nl(i-1)%this%nextSibling => temp_nl(i)%this
          temp_nl(i)%this%previousSibling => temp_nl(i-1)%this
        endif
        if (i==arg%childNodes%length) then
          arg%lastChild => temp_nl(i_t)%this
          temp_nl(i_t)%this%nextSibling => null() ! FIXME no-op
        else
          arg%childNodes%nodes(i+1)%this%previousSibling => temp_nl(i_t)%this
          temp_nl(i_t)%this%nextSibling => arg%childNodes%nodes(i+1)%this
        endif
      else
        i_t = i_t + 1
        temp_nl(i_t)%this => arg%childNodes%nodes(i)%this
      endif
    enddo

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR)
    endif
    np%parentNode => null()
    np%previousSibling => null()
    np%nextSibling => null()

    if (.not.arg%ownerDocument%xds%building) then
      if (arg%inDocument) then
        call removeNodesFromDocument(arg%ownerDocument, oldChild)
        if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
          do i = 1, newChild%childNodes%length
            call putNodesInDocument(arg%ownerDocument, newChild%childNodes%nodes(i)%this)
          enddo
        else
          call putNodesInDocument(arg%ownerDocument, newChild)
        endif
        ! If newChild was originally in document, it was removed above so must be re-added FIXME
      endif
      ! If arg was not in the document, then newChildren were either 
      ! a) removed above in call to removeChild or
      ! b) in a document fragment and therefore not part of doc either
    endif

    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      deallocate(newChild%childNodes%nodes)
      allocate(newChild%childNodes%nodes(0))
      newChild%childNodes%length = 0
      deallocate(arg%childNodes%nodes)
      arg%childNodes%nodes => temp_nl
      arg%childNodes%length = size(arg%childNodes%nodes)
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

! NOTE BELOW FUCKED-UP WORKAROUND FOR G95!!
    np => arg
    oldChild%parentNode => null()
    oldChild%previousSibling => null()
    oldChild%nextSibling => null()
    arg => np
    if (.not.arg%ownerDocument%xds%building) then
      if (arg%inDocument) then
        call removeNodesFromDocument(arg%ownerDocument, oldChild)
      endif
    endif

    np => oldChild

  end function removeChild


  TOHW_function(appendChild, (arg, newChild), np)
    type(Node), pointer :: arg
    type(Node), pointer :: newChild
    type(Node), pointer :: np
    
    type(Node), pointer :: testChild, testParent
    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i_t

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    if (.not. associated(arg))  & 
      call dom_error("appendChild",0,"Node not allocated")

    testParent => arg
    ! Check if you are allowed to put a newChild nodetype under a arg nodetype
    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE) then
      do i = 1, newChild%childNodes%length
        testChild => newChild%childNodes%nodes(i)%this
        TOHW_m_dom_hierarchy_test
      enddo
    else
      testChild => newChild
      TOHW_m_dom_hierarchy_test
      ! And then check that newChild is not one of args ancestors
      ! (this would never be true if newChild is a documentFragment)
      testParent => arg%parentNode
      do while (associated(testParent))
        if (associated(testParent, newChild)) then
          TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
        endif
        testParent => testParent%parentNode
      enddo
    endif

    if (.not.(associated(arg%ownerDocument, newChild%ownerDocument) &
      .or. associated(arg, newChild%ownerDocument))) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif

    if (newChild%nodeType==DOCUMENT_FRAGMENT_NODE &
      .and. newChild%childNodes%length==0) then
      np => newChild
      return
      ! Nothing to do
    endif

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
      i_t = arg%childNodes%length
      do i = 1, newChild%childNodes%length
        i_t = i_t + 1
        temp_nl(i_t)%this => newChild%childNodes%nodes(i)%this
        print*,"ASS", i_t, associated(temp_nl(i_t)%this)
        if (arg%inDocument) &
          call putNodesInDocument(arg%ownerDocument, temp_nl(i_t)%this)
        temp_nl(i_t)%this%parentNode => arg
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
          call putNodesInDocument(arg%ownerDocument, newChild)
        endif
      endif
      newChild%nextSibling => null()
      arg%lastChild => newChild
      newChild%parentNode => arg
    endif

    deallocate(arg%childNodes%nodes)
    arg%childNodes%nodes => temp_nl
    arg%childNodes%length = size(temp_nl)

    np => newChild

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

    type(Node), pointer :: doc, thatParent, this, new, ERchild

    logical :: doneAttributes, doneChildren, readonly
    integer :: i

    this => arg
    thatParent => null()
    ERchild => null()
    doc => getOwnerDocument(arg)
    np => null()

    readonly = .false.

TOHW_m_dom_treewalk(`

      new => null()
      select case(getNodeType(this))
      case (ELEMENT_NODE)
        if (.not.doneAttributes) then
          ! Are there any new prefixes or namespaces to be declared?
          ! FIXME
          new => createElement(doc, getTagName(this))
        endif
      case (ATTRIBUTE_NODE)
        new => createAttribute(doc, getName(this))
        if (associated(this, arg)) then
          call setSpecified(new, .true.)
        else
          call setSpecified(new, getSpecified(this))
        endif
      case (TEXT_NODE)
        new => createTextNode(doc, getData(this))
      case (CDATA_SECTION_NODE)
        new => createCDataSection(doc, getData(this))
      case (ENTITY_REFERENCE_NODE)
        ERchild => this
        readonly = .true.
        new => createEntityReference(doc, getNodeName(this))
      case (ENTITY_NODE)
        return
      case (PROCESSING_INSTRUCTION_NODE)
        new => createProcessingInstruction(doc, getTarget(this), getData(this))
      case (COMMENT_NODE)
        new => createEntityReference(doc, getNodeValue(this))
      case (DOCUMENT_NODE)
        return
      case (DOCUMENT_TYPE_NODE)
        return
      case (DOCUMENT_FRAGMENT_NODE)
        new => createDocumentFragment(doc)
      case (NOTATION_NODE)
        return
      end select

      if (.not.associated(thatParent)) then
        thatParent => new
      elseif (associated(new)) then
        new%readonly = readonly
        if (this%nodeType==ATTRIBUTE_NODE) then
          new => setAttributeNode(thatParent, new)
        else
          new => appendChild(thatParent, new)
        endif
      endif

      if (.not.deep) then
        if (getNodeType(arg)/=ELEMENT_NODE.and.getNodeType(arg)/=ATTRIBUTE_NODE) return
      endif
', `

      if (getNodeType(this)==ENTITY_REFERENCE_NODE &
        .and.associated(ERchild, this)) then
          ERchild => null()
          readonly = .false.
      endif
      
', `parentNode')

    np => thatParent

  end function cloneNode

  
  function hasAttributes(arg)
    type(Node), pointer :: arg
    logical :: hasAttributes
    
    if (.not.associated(arg)) call dom_error("hasAttributes",0,"Node not allocated")
    hasAttributes = (arg%nodeType /= ELEMENT_NODE) &
      .and. (arg%attributes%length > 0)
    
  end function hasAttributes
  
  TOHW_subroutine(normalize, (arg))
    type(Node), pointer :: arg
    type(Node), pointer :: this, tempNode, oldNode
    integer :: i, i_t
    logical :: doneChildren, doneAttributes
    character, pointer :: temp(:)

! DOM standard requires we ignore readonly status

    this => arg

TOHW_m_dom_treewalk(`

      if (getNodeType(this)==TEXT_NODE) then
        if (associated(this, arg)) exit ! If we are called on a text node itself, then do nothing.
        i_t = getLength(this)

        tempNode => getNextSibling(this)
        do while (associated(tempNode))
          if (getNodeType(tempNode)/=TEXT_NODE) exit
          i_t = i_t + getLength(tempNode)
          tempNode => getNextSibling(tempNode)
        enddo
        if (i_t > getLength(this)) then
          allocate(temp(i_t))
          temp(:getLength(this)) = getData(this)
          i_t = 1
          tempNode => getNextSibling(this)
          do while (associated(tempNode))
            if (getNodeType(tempNode)/=TEXT_NODE) exit
            temp(i_t:getLength(tempNode)-1) = getData(tempNode)
            i_t = i_t + getLength(tempNode)
            tempNode => getNextSibling(tempNode)
          enddo
          deallocate(this%nodeValue)
          this%nodeValue => temp
          do while (associated(tempNode))
            if (getNodeType(tempNode)/=TEXT_NODE) exit
            if (.not.arg%inDocument) call remove_node_nl(arg%ownerDocument%hangingNodes, tempNode)
            oldNode => removeChild(getParentNode(tempNode), tempNode)
            tempNode => tempNode%nextSibling
            call destroy(oldNode)
          enddo
        endif
      end if
',`',`')

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

  ! function isDefaultNamespace
  ! function isEqualNode(np, arg)

  function isSameNode(np, other)    ! DOM 3.0
    type(Node), pointer :: np
    type(Node), pointer :: other
    logical :: isSameNode

    isSameNode = associated(np, other)

  end function isSameNode

  ! function lookupNamespaceURI
  ! function lookupPrefix

  ! function getUserData
  ! function setUserData
  ! will not implement ...

  subroutine putNodesInDocument(doc, np_orig)
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
      np%inDocument = .true.
      call remove_node_nl(np%ownerDocument%hangingNodes, np)
      if (np%nodeType==ATTRIBUTE_NODE) then
        if (associated(np, np_orig)) exit
        if (i==np%ownerElement%attributes%length) then
          ascending = .true.
        else
          i = i + 1
          np => np%ownerElement%attributes%nodes(i)%this
        endif
      elseif (associated(np%nextSibling).and..not.associated(np, np_orig)) then
        np => np%nextSibling
        attributesdone = .false.
      else
        ascending = .true.
      endif
    enddo
  end subroutine putNodesInDocument

  subroutine removeNodesFromDocument(doc, np_orig)
    type(Node), pointer :: doc, np_orig
    type(Node), pointer :: np
    logical :: ascending, attributesdone
    integer :: i
    print*,"REMOVE"
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
      call append_nl(np%ownerDocument%hangingNodes, np)
      if (np%nodeType==ATTRIBUTE_NODE) then
        if (associated(np, np_orig)) exit
        if (i==np%ownerElement%attributes%length) then
          ascending = .true.
        else
          i = i + 1
          np => np%ownerElement%attributes%nodes(i)%this
        endif
      elseif (associated(np%nextSibling).and..not.associated(np, np_orig)) then
        np => np%nextSibling
        attributesdone = .false.
      else
        ascending = .true.
      endif
    enddo
  end subroutine removeNodesFromDocument

')`'dnl

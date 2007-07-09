include(`m_dom_exception.m4')`'dnl
TOHW_m_dom_imports(`

  use m_common_array_str, only: vs_str, str_vs, vs_str_alloc
  use m_dom_error, only: DOMException, throw_exception, is_in_error, &
    NO_MODIFICATION_ALLOWED_ERR, NOT_FOUND_ERR, HIERARCHY_REQUEST_ERR, &
    WRONG_DOCUMENT_ERR, FoX_INTERNAL_ERROR, FoX_NODE_IS_NULL

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
  public :: isSameNode

  public :: setStringValue
  public :: getStringValue
  public :: setReadonlyNode

')`'dnl
TOHW_m_dom_contents(`

  ! Getters and setters

  TOHW_function(getNodeName, (arg), c)
    type(Node), pointer :: arg
    character(len=size(arg%nodeName)) :: c
    
    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

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

  TOHW_function(getNodeValue, (arg), c)
    type(Node), pointer :: arg
    character(len=getNodeValue_len(arg)) :: c

    integer :: i, n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

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

  TOHW_subroutine(setStringValue, (arg, stringValue))
    type(Node), pointer :: arg
    character(len=*) :: stringValue
  
    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)/=ENTITY_NODE) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    if (associated(arg%nodeValue)) deallocate(arg%nodeValue)
    arg%nodeValue => vs_str_alloc(stringValue)

  end subroutine setStringValue

  pure function getStringValue_len(arg, p) result(n)
    type(Node), pointer :: arg
    logical, intent(in) :: p
    integer :: n

    if (p) then
      n = size(arg%nodeValue)
    else
      n = 0
    endif
  end function getStringValue_len

  TOHW_function(getStringValue, (arg), s)
    type(Node), pointer :: arg
    character(len=getStringValue_len(arg, associated(arg))) :: s
  
    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)/=ENTITY_NODE) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    s = str_vs(arg%nodeValue)

  end function getStringValue
    
  TOHW_subroutine(setNodeValue, (arg, nodeValue))
    type(Node), pointer :: arg
    character(len=*) :: nodeValue

    type(Node), pointer :: np
    integer :: i, n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

    select case(arg%nodeType)
    case (ATTRIBUTE_NODE)
      ! FIXME check does string contain wrong characters
      ! destroy any existing children ... 
      do i = 1, arg%childNodes%length
        if (.not.arg%inDocument) &
          call remove_node_nl(arg%ownerDocument%docExtras%hangingNodes, arg%childNodes%nodes(i)%this)
        call destroyNode(arg%childNodes%nodes(i)%this)
      enddo
      deallocate(arg%childNodes%nodes)
      allocate(arg%childNodes%nodes(0))
      arg%childNodes%length = 0
      arg%firstChild => null()
      arg%lastChild => null()
      ! and add the new one.
      ! Avoid manipulating hangingnode lists
      call setGCstate(arg%ownerDocument, .false.)
      np => createTextNode(arg%ownerDocument, nodeValue)
      np => appendChild(arg, np)
      call setGCstate(arg%ownerDocument, .true.)
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

  TOHW_function(getNodeType, (arg), n)
    type(Node), pointer :: arg
    integer :: n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    n = arg%nodeType
  end function getNodeType

  TOHW_function(getParentNode, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    np => arg%parentNode
  end function getParentNode
  
  TOHW_function(getChildNodes, (arg), nl)
    type(Node), pointer :: arg
    type(NodeList), pointer :: nl

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    nl => arg%childnodes
  end function getChildNodes
  
  TOHW_function(getFirstChild, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    np => arg%firstChild
  end function getFirstChild
  
  TOHW_function(getLastChild, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    np => arg%lastChild
  end function getLastChild

  TOHW_function(getPreviousSibling, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    np => arg%previousSibling
  end function getPreviousSibling
  
  TOHW_function(getNextSibling, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    np => arg%nextSibling
  end function getNextSibling

  TOHW_function(getAttributes, (arg), nnm)
    type(Node), pointer :: arg
    type(NamedNodeMap), pointer :: nnm

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)==ELEMENT_NODE) then
      nnm => arg%attributes
    else
      nnm => null()
    endif
  end function getAttributes

  TOHW_function(getOwnerDocument, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    
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

    if (.not.associated(arg).or..not.associated(newChild)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (.not.associated(refChild)) then
      np => appendChild(arg, newChild, ex)
    endif

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

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
      ! And then check that newChild is not arg or one of args ancestors
      ! (this would never be true if newChild is a documentFragment)
      testParent => arg
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

    np => newChild

    if (getGCstate(getownerDocument(arg))) then
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

    if (.not.associated(arg).or..not.associated(newChild).or..not.associated(oldChild)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

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
      ! And then check that newChild is not arg or one of args ancestors
      ! (this would never be true if newChild is a documentFragment)
      testParent => arg
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

    if (getGCstate(arg%ownerDocument)) then
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

    if (.not.associated(arg).or..not.associated(oldChild)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

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
    if (getGCstate(arg%ownerDocument)) then
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

    if (.not.associated(arg).or..not.associated(newChild)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    endif

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
      ! And then check that newChild is not arg or one of args ancestors
      ! (this would never be true if newChild is a documentFragment)
      testParent => arg
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
      if (getGCstate(arg%ownerDocument)) then
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


  TOHW_function(hasChildNodes, (arg))
    type(Node), pointer :: arg
    logical :: hasChildNodes
    
    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    hasChildNodes = associated(arg%firstChild)
    
  end function hasChildNodes

  TOHW_function(cloneNode, (arg, deep), np)
    type(Node), pointer :: arg
    logical :: deep
    type(Node), pointer :: np

    type(Node), pointer :: doc, thatParent, this, new, ERchild

    logical :: doneAttributes, doneChildren, readonly, quickFix
    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

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
        new => createEmptyEntityReference(doc, getNodeName(this))
      case (ENTITY_NODE)
        return
      case (PROCESSING_INSTRUCTION_NODE)
        new => createProcessingInstruction(doc, getTarget(this), getData(this))
      case (COMMENT_NODE)
        new => createComment(doc, getData(this))
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
      this%readonly = readonly
      
', `parentNode',`')

    np => thatParent

  end function cloneNode

  
  TOHW_function(hasAttributes, (arg))
    type(Node), pointer :: arg
    logical :: hasAttributes

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    
    hasAttributes = (arg%nodeType /= ELEMENT_NODE) &
      .and. (arg%attributes%length > 0)
    
  end function hasAttributes
  
  TOHW_subroutine(normalize, (arg))
    type(Node), pointer :: arg
    type(Node), pointer :: this, tempNode, oldNode
    integer :: i, i_t
    logical :: doneChildren, doneAttributes
    character, pointer :: temp(:)

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

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
        if (.not.associated(tempNode, getNextSibling(this))) then
          allocate(temp(i_t))
          temp(:getLength(this)) = vs_str(getData(this))
          i_t = getLength(this)
          tempNode => getNextSibling(this)
          do while (associated(tempNode))
            if (getNodeType(tempNode)/=TEXT_NODE) exit
            temp(i_t+1:i_t+getLength(tempNode)) = vs_str(getData(tempNode))
            i_t = i_t + getLength(tempNode)
            oldNode => tempNode
            tempNode => getNextSibling(tempNode)
            oldNode => removeChild(getParentNode(oldNode), oldNode)
            call remove_node_nl(arg%ownerDocument%docExtras%hangingNodes, oldNode)
            call destroy(oldNode)
          enddo
          deallocate(this%nodeValue)
          this%nodeValue => temp
        endif
      end if
',`',)


  end subroutine normalize

  TOHW_function(isSupported, (arg, feature, version), p)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: feature
    character(len=*), intent(in) :: version
    logical :: p

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    p = hasFeature(getImplementation(arg%ownerDocument), feature, version)
  end function isSupported

  ! FIXME should the below instead just decompose the QName on access?
  TOHW_function(getNamespaceURI, (arg), c)
    type(Node), pointer :: arg
    character(len=size(arg%namespaceURI)) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    c = str_vs(arg%namespaceURI)
  end function getNamespaceURI

  TOHW_function(getPrefix, (arg), c)
    type(Node), pointer :: arg
    character(len=size(arg%prefix)) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    c = str_vs(arg%prefix)
  end function getPrefix
  
  TOHW_subroutine(setPrefix, (arg, prefix))
    type(Node), pointer :: arg
    character(len=*) :: prefix

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    deallocate(arg%prefix)
    arg%prefix => vs_str_alloc(prefix)

    print*, "why are you doing this?"
    ! FIXME we should implement this but raise a FoX-specific exception if used
    stop
    ! FIXME exceptions
  end subroutine setPrefix

  TOHW_function(getLocalName, (arg), c)
    type(Node), pointer :: arg
    character(len=size(arg%localName)) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    c = str_vs(arg%localName)
  end function getLocalName

  ! function isDefaultNamespace
  ! function isEqualNode(np, arg)

  TOHW_function(isSameNode, (arg, other))
    type(Node), pointer :: arg
    type(Node), pointer :: other
    logical :: isSameNode

    if (.not.associated(arg).or..not.associated(other)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    isSameNode = associated(arg, other)

  end function isSameNode

  ! function lookupNamespaceURI
  ! function lookupPrefix

  ! function getUserData
  ! function setUserData
  ! will not implement ...

  subroutine putNodesInDocument(doc, arg)
    type(Node), pointer :: doc, arg
    type(Node), pointer :: this
    logical :: doneChildren, doneAttributes
    integer :: i

    this => arg

TOHW_m_dom_treewalk(`
        this%inDocument = .true.
        call remove_node_nl(doc%docExtras%hangingNodes, this)
',`')

  end subroutine putNodesInDocument

  subroutine removeNodesFromDocument(doc, arg)
    type(Node), pointer :: doc, arg
    type(Node), pointer :: this
    logical :: doneChildren, doneAttributes
    integer :: i

    this => arg

TOHW_m_dom_treewalk(`
        this%inDocument = .false.
        call append_nl(doc%docExtras%hangingNodes, this)
',`')

  end subroutine removeNodesFromDocument

  subroutine setReadOnlyNode(arg, p, deep)
    type(Node), pointer :: arg
    logical, intent(in) :: p
    logical, intent(in) :: deep

    type(Node), pointer :: this
    integer :: i
    logical :: doneAttributes, doneChildren

    if (deep) then
      this => arg

TOHW_m_dom_treewalk(`
      this%readonly = p
',`')
    else
      arg%readonly = p
    endif

  end subroutine setReadOnlyNode

')`'dnl

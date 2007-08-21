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
  public :: getReadOnly

')`'dnl
TOHW_m_dom_contents(`

TOHW_m_dom_get(DOMString, nodeName, np%nodeName)

  pure function getNodeValue_len(np, p) result(n)
    type(Node), intent(in) :: np
    logical, intent(in) :: p
    integer :: n

    integer :: i

    n = 0 
    if (.not.p) return

    select case(np%nodeType)
    case (ATTRIBUTE_NODE)
      do i = 1, np%childNodes%length
        n = n + size(np%childNodes%nodes(i)%this%nodeValue)
      enddo
    case (CDATA_SECTION_NODE, COMMENT_NODE, PROCESSING_INSTRUCTION_NODE, TEXT_NODE)
      n = size(np%nodeValue)
    end select

  end function getNodeValue_len

  TOHW_function(getNodeValue, (np), c)
    type(Node), pointer :: np
    character(len=getNodeValue_len(np, associated(np))) :: c

    integer :: i, n

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    select case(np%nodeType)
    case (ATTRIBUTE_NODE)
      n = 1
      do i = 1, np%childNodes%length
        c(n:n+size(np%childNodes%nodes(i)%this%nodeValue)-1) = &
          str_vs(np%childNodes%nodes(i)%this%nodeValue)
      enddo
    case (CDATA_SECTION_NODE, COMMENT_NODE, PROCESSING_INSTRUCTION_NODE, TEXT_NODE)
      c = str_vs(np%nodeValue)
    case default
      c = ""
    end select
    
  end function getNodeValue

  TOHW_subroutine(setNodeValue, (arg, nodeValue))
    type(Node), pointer :: arg
    character(len=*) :: nodeValue

    type(Node), pointer :: np
    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (associated(getOwnerDocument(arg))) then
      if (.not.checkChars(nodeValue, getXmlVersionEnum(getOwnerDocument(arg)))) then
        TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
      endif
    endif ! Otherwise its a document node, and nothing will happen anyway

    select case(arg%nodeType)
    case (ATTRIBUTE_NODE)
      if (arg%readonly) then
        TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
      endif
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
      !      call setGCstate(arg%ownerDocument, .false.)
      np => createTextNode(arg%ownerDocument, nodeValue)
      np => appendChild(arg, np, ex)
      !      call setGCstate(arg%ownerDocument, .true.)
      !      if (.not.arg%inDocument) call append(arg%document%blah, ...)
    case (CDATA_SECTION_NODE)
      if (arg%readonly) then
        TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
      endif
      if (index(str_vs(arg%nodeValue),"]]>")>0) then
        TOHW_m_dom_throw_error(FoX_INVALID_CDATA_SECTION)
      endif
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(nodeValue)
    case (COMMENT_NODE)
      if (arg%readonly) then
        TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
      endif
      if (index(str_vs(arg%nodeValue),"--")>0) then
        TOHW_m_dom_throw_error(FoX_INVALID_COMMENT)
      endif
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(nodeValue)
    case (PROCESSING_INSTRUCTION_NODE)
      if (arg%readonly) then
        TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
      endif
      if (index(str_vs(arg%nodeValue),"?>")>0) then
        TOHW_m_dom_throw_error(FoX_INVALID_PI_DATA)
      endif
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(nodeValue)
    case (TEXT_NODE)
      if (arg%readonly) then
        TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
      endif
      deallocate(arg%nodeValue)
      arg%nodeValue => vs_str_alloc(nodeValue)
    end select

  end subroutine setNodeValue

TOHW_m_dom_get(integer, nodeType, np%nodeType)

TOHW_m_dom_get(Node, parentNode, np%parentNode)

TOHW_m_dom_get(NodeList, childNodes, np%childNodes)

TOHW_m_dom_get(Node, firstChild, np%firstChild)

TOHW_m_dom_get(Node, lastChild, np%lastChild)

TOHW_m_dom_get(Node, previousSibling, np%previousSibling)

TOHW_m_dom_get(Node, nextSibling, np%nextSibling)

  TOHW_function(getAttributes, (arg), nnm)
    type(Node), pointer :: arg
    type(NamedNodeMap), pointer :: nnm

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)==ELEMENT_NODE) then
      nnm => arg%elExtras%attributes
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

    type(Node), pointer :: testChild, testParent, treeroot, this
    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i2, i_t, i_tree
    logical :: doneChildren, doneAttributes

    if (.not.associated(arg).or..not.associated(newChild)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (.not.associated(refChild)) then
      np => appendChild(arg, newChild, ex)
      return
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

    if (associated(getParentNode(newChild))) &
      newChild => removeChild(getParentNode(newChild), newChild, ex) 

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
          !temp_nl(1)%this%previousSibling => null() ! This is a no-op
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
      TOHW_m_dom_throw_error(NOT_FOUND_ERR, (temp_nl))
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
        ! If newChild was originally in document, it was removed above so must be re-added
        ! Ideally we would avoid the cost of removal & readding to hanging nodelist
      endif
      ! If arg was not in the document, then newChildren were either 
      ! a) removed above in call to removeChild or
      ! b) in a document fragment and therefore not part of doc either
    endif


    if (getNodeType(newChild)==DOCUMENT_FRAGMENT_NODE) then
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

    type(Node), pointer :: testChild, testParent, treeroot, this
    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i2, i_t, i_tree
    logical :: doneChildren, doneAttributes

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

    if (associated(getParentNode(newChild))) &
      newChild => removeChild(getParentNode(newChild), newChild, ex) 

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
          !temp_nl(1)%this%previousSibling => null() ! This is a no-op
        else 
          temp_nl(i-1)%this%nextSibling => temp_nl(i)%this
          temp_nl(i)%this%previousSibling => temp_nl(i-1)%this
        endif
        if (i==arg%childNodes%length) then
          arg%lastChild => temp_nl(i_t)%this
          !temp_nl(i_t)%this%nextSibling => null() ! This is a no-op
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
        ! If newChild was originally in document, it was removed above so must be re-added
        ! Ideally we would avoid the cost of removing & re-adding to hangingnodelist
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
        if (i_t==size(arg%childNodes%nodes)) exit ! We have failed to find the child
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
    oldChild%parentNode => null()
    oldChild%previousSibling => null()
    oldChild%nextSibling => null()
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
    
    type(Node), pointer :: testChild, testParent, treeroot, this
    type(ListNode), pointer :: temp_nl(:)
    integer :: i, i_t, i_tree
    logical :: doneChildren, doneAttributes

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

    if (associated(getParentNode(newChild))) &
      newChild => removeChild(getParentNode(newChild), newChild, ex) 

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
    logical, intent(in) :: deep
    type(Node), pointer :: np

    type(Node), pointer :: doc, treeroot, thatParent, this, new, ERchild

    logical :: doneAttributes, doneChildren, readonly
    integer :: i_tree

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    thatParent => null()
    ERchild => null()
    doc => getOwnerDocument(arg)
    np => null()
    
    readonly = .false.

    treeroot => arg
TOHW_m_dom_treewalk(`

      new => null()
      select case(getNodeType(this))
      case (ELEMENT_NODE)
        ! Are there any new prefixes or namespaces to be declared?
        ! FIXME
        if (getNamespaceURI(this)=="") then
          new => createElement(doc, getTagName(this))
        else
          new => createElementNS(doc, getNamespaceURI(this), getTagName(this))
        endif
      case (ATTRIBUTE_NODE)
        if (getNamespaceURI(this)=="") then 
          new => createAttribute(doc, getName(this))
        else
          new => createAttributeNS(doc, getNamespaceURI(this), getName(this))
        endif
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
        if (getNodeType(arg)==ATTRIBUTE_NODE) then
          continue
        else
          exit
        endif
      endif
', `'`

      if (getNodeType(this)==ENTITY_REFERENCE_NODE &
        .and.associated(ERchild, this)) then
          ERchild => null()
          readonly = .false.
      endif
      this%readonly = readonly
      
', `parentNode')

    np => thatParent

  end function cloneNode

  
  TOHW_function(hasAttributes, (arg))
    type(Node), pointer :: arg
    logical :: hasAttributes

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    
    if (arg%nodeType == ELEMENT_NODE) then
      hasAttributes = (getLength(getAttributes(arg)) > 0)
    else
      hasAttributes = .false.
    endif
    
  end function hasAttributes
  
  TOHW_subroutine(normalize, (arg))
    type(Node), pointer :: arg
    type(Node), pointer :: this, tempNode, oldNode, treeroot
    integer :: i_tree, i_t
    logical :: doneChildren, doneAttributes
    character, pointer :: temp(:)

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

! DOM standard requires we ignore readonly status
    treeroot => arg
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

  pure function getNamespaceURI_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    n = 0
    if (p) then
      if (arg%nodeType==ELEMENT_NODE &
        .or. arg%nodeType==ATTRIBUTE_NODE &
        .or. arg%nodeType==XPATH_NAMESPACE_NODE) then
        n = size(arg%elExtras%namespaceURI)
      endif
    endif

  end function getNamespaceURI_len

  TOHW_function(getNamespaceURI, (arg), c)
    type(Node), pointer :: arg
    character(len=getNamespaceURI_len(arg, associated(arg))) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    c = ""
    if (arg%nodeType==ELEMENT_NODE &
      .or. arg%nodeType==ATTRIBUTE_NODE &
      .or. arg%nodeType==XPATH_NAMESPACE_NODE) then
      c = str_vs(arg%elExtras%namespaceURI)
    endif
  end function getNamespaceURI

  pure function getPrefix_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    n = 0
    if (p) then
      if (arg%nodeType==ELEMENT_NODE &
        .or. arg%nodeType==ATTRIBUTE_NODE &
        .or. arg%nodeType==XPATH_NAMESPACE_NODE) then
        n = size(arg%elExtras%prefix)
      endif
    endif

  end function getPrefix_len

  TOHW_function(getPrefix, (arg), c)
    type(Node), pointer :: arg
    character(len=getPrefix_len(arg, associated(arg))) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    c = ""
    if (arg%nodeType==ELEMENT_NODE &
      .or. arg%nodeType==ATTRIBUTE_NODE &
      .or. arg%nodeType==XPATH_NAMESPACE_NODE) then
      c = str_vs(arg%elExtras%prefix)
    endif

  end function getPrefix
  
  TOHW_subroutine(setPrefix, (arg, prefix))
    type(Node), pointer :: arg
    character(len=*) :: prefix

    character, pointer :: tmp(:)
    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType==ELEMENT_NODE &
      .or. arg%nodeType==ATTRIBUTE_NODE &
      .or. arg%nodeType==XPATH_NAMESPACE_NODE) then
      if (arg%readonly) then
        TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
      elseif (.not.checkName(prefix, getXds(getOwnerDocument(arg)))) then
        TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
      elseif (.not.checkNCName(prefix, getXmlVersionEnum(getOwnerDocument(arg)))) then
        TOHW_m_dom_throw_error(NAMESPACE_ERR)
      elseif (size(arg%elExtras%namespaceURI)==0) then
        TOHW_m_dom_throw_error(NAMESPACE_ERR)
      elseif (prefix=="xml" .and. &
        str_vs(arg%elExtras%namespaceURI)/="http://www.w3.org/XML/1998/namespace") then
        TOHW_m_dom_throw_error(NAMESPACE_ERR)
      elseif (prefix=="xmlns" .and. (getNodeType(arg)/=ATTRIBUTE_NODE &
        .or. str_vs(arg%elExtras%namespaceURI)/="http://www.w3.org/2000/xmlns/")) then
        TOHW_m_dom_throw_error(NAMESPACE_ERR)
      elseif (getNodeType(arg)==ATTRIBUTE_NODE.and.getName(arg)=="xmlns") then
        TOHW_m_dom_throw_error(NAMESPACE_ERR)
      endif
! FIXME check if prefix is declared ...
      deallocate(arg%elExtras%prefix)
      arg%elExtras%prefix = vs_str_alloc(prefix)
      tmp => arg%nodeName
      i = index(str_vs(arg%nodeName), ":")
      if (i==0) then
        arg%nodeName => vs_str_alloc(prefix//":"//str_vs(tmp))
      else
        arg%nodeName => vs_str_alloc(prefix//str_vs(tmp(i+1:)))
      endif
      deallocate(tmp)
    endif

  end subroutine setPrefix

  pure function getLocalName_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    n = 0
    if (p) then
      if (arg%nodeType==ELEMENT_NODE &
        .or. arg%nodeType==ATTRIBUTE_NODE &
        .or. arg%nodeType==XPATH_NAMESPACE_NODE) then
        n = size(arg%elExtras%localName)
      endif
    endif

  end function getLocalName_len

  TOHW_function(getLocalName, (arg), c)
    type(Node), pointer :: arg
    character(len=getLocalName_len(arg, associated(arg))) :: c

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    c = ""
    if (arg%nodeType==ELEMENT_NODE &
      .or. arg%nodeType==ATTRIBUTE_NODE &
      .or. arg%nodeType==XPATH_NAMESPACE_NODE) then
      c = str_vs(arg%elExtras%localName)
    endif

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

  TOHW_function(isDefaultNamespace, (np, namespaceURI), p)
    type(Node), pointer :: np
    character(len=*), intent(in) :: namespaceURI
    logical :: p

    type(Node), pointer :: el
    integer :: i

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    el => null()
    select case(getNodeType(np))
    case (ELEMENT_NODE)
      el => np
    case (ATTRIBUTE_NODE)
      el => getOwnerElement(np)
    case (DOCUMENT_NODE)
      el => getDocumentElement(np)
    end select

    p = .false.
    if (associated(el)) then
      if (size(el%elExtras%namespaceURI)>0) then
        do i = 1, el%elExtras%namespaceNodes%length
          if (size(el%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)==0) then
            p = (str_vs(el%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)==namespaceURI)
            return
          endif
        enddo
      endif
    endif
  end function isDefaultNamespace

  pure function lookupNamespaceURI_len(np, prefix, p) result(n)
    type(Node), pointer :: np
    character(len=*), intent(in) :: prefix
    logical, intent(in) :: p
    integer :: n

    type(Node), pointer :: el
    integer :: i

    if (.not.p) then
      n = 0
      return
    endif

    select case(np%nodeType)
    case (ELEMENT_NODE)
      if (size(np%elExtras%namespaceURI)>0) then
        do i = 1, np%elExtras%namespaceNodes%length
          if (str_vs(np%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)==prefix) then
            n = size(np%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)
            return
          endif
        enddo
      endif
    case (ATTRIBUTE_NODE)
      if (size(np%elExtras%ownerElement%elExtras%namespaceURI)>0) then
        do i = 1, np%elExtras%ownerElement%elExtras%namespaceNodes%length
          if (str_vs(np%elExtras%ownerElement%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)==prefix) then
            n = size(np%elExtras%ownerElement%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)
            return
          endif
        enddo
      endif
    case (DOCUMENT_NODE)
      if (size(np%docExtras%documentElement%elExtras%namespaceURI)>0) then
        do i = 1, np%docExtras%documentElement%elExtras%namespaceNodes%length
          if (str_vs(np%docExtras%documentElement%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)==prefix) then
            n = size(np%docExtras%documentElement%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)
            return
          endif
        enddo
      endif
    end select

    end function lookupNamespaceURI_len

  TOHW_function(lookupNamespaceURI, (np, prefix), c)
    type(Node), pointer :: np
    character(len=*), intent(in) :: prefix
    character(len=lookupNamespaceURI_len(np, prefix, associated(np))) :: c

    type(Node), pointer :: el
    integer :: i

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    el => null()
    select case(getNodeType(np))
    case (ELEMENT_NODE)
      el => np
    case (ATTRIBUTE_NODE)
      el => getOwnerElement(np)
    case (DOCUMENT_NODE)
      el => getDocumentElement(np)
    end select

    c = ""
    if (associated(el)) then
      if (size(el%elExtras%namespaceURI)>0) then
        do i = 1, el%elExtras%namespaceNodes%length
          if (str_vs(el%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)==prefix) then
            c = str_vs(el%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)
            return
          endif
        enddo
      endif
    endif
    end function lookupNamespaceURI

  pure function lookupPrefix_len(np, namespaceURI, p) result(n)
    type(Node), pointer :: np
    character(len=*), intent(in) :: namespaceURI
    logical, intent(in) :: p
    integer :: n

    type(Node), pointer :: el
    integer :: i

    if (.not.p) then
      n = 0
      return
    endif

    select case(np%nodeType)
    case (ELEMENT_NODE)
      if (size(np%elExtras%namespaceURI)>0) then
        do i = 1, np%elExtras%namespaceNodes%length
          if (str_vs(np%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)==namespaceURI) then
            n = size(np%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)
            return
          endif
        enddo
      endif
    case (ATTRIBUTE_NODE)
      if (size(np%elExtras%ownerElement%elExtras%namespaceURI)>0) then
        do i = 1, np%elExtras%ownerElement%elExtras%namespaceNodes%length
          if (str_vs(np%elExtras%ownerElement%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)==namespaceURI) then
            n = size(np%elExtras%ownerElement%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)
            return
          endif
        enddo
      endif
    case (DOCUMENT_NODE)
      if (size(np%docExtras%documentElement%elExtras%namespaceURI)>0) then
        do i = 1, np%docExtras%documentElement%elExtras%namespaceNodes%length
          if (str_vs(np%docExtras%documentElement%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)==namespaceURI) then
            n = size(np%docExtras%documentElement%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)
            return
          endif
        enddo
      endif
    end select

    end function lookupPrefix_len

  TOHW_function(lookupPrefix, (np, namespaceURI), c)
    type(Node), pointer :: np
    character(len=*), intent(in) :: namespaceURI
    character(len=lookupPrefix_len(np, namespaceURI, associated(np))) :: c

    type(Node), pointer :: el
    integer :: i

    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    el => null()
    select case(getNodeType(np))
    case (ELEMENT_NODE)
      el => np
    case (ATTRIBUTE_NODE)
      el => getOwnerElement(np)
    case (DOCUMENT_NODE)
      el => getDocumentElement(np)
    end select

    c = ""
    if (associated(el)) then
      if (size(el%elExtras%namespaceURI)>0) then
        do i = 1, el%elExtras%namespaceNodes%length
          if (str_vs(el%elExtras%namespaceNodes%nodes(i)%this%elExtras%namespaceURI)==namespaceURI) then
            c = str_vs(el%elExtras%namespaceNodes%nodes(i)%this%elExtras%prefix)
            return
          endif
        enddo
      endif
    endif
    end function lookupPrefix

  ! function lookupPrefix

  ! function getUserData
  ! function setUserData
  ! will not implement ...

  subroutine putNodesInDocument(doc, arg)
    type(Node), pointer :: doc, arg
    type(Node), pointer :: this, treeroot
    logical :: doneChildren, doneAttributes
    integer :: i_tree

    treeroot => arg
TOHW_m_dom_treewalk(`
        this%inDocument = .true.
        if (this%nodeType==ELEMENT_NODE.and.doc%docExtras%liveNodeLists) &
          call updateNodeLists(doc, "", getNodeName(this), "", getLocalName(this), "", getNamespaceURI(this))
        ! The above is a bit inefficient; really we should construct a list of all
        ! element names added/removed and then call updateNodeLists on all of them only at the end.
        call remove_node_nl(doc%docExtras%hangingNodes, this)
',`')

!FIXME need to call updateNodeLists elsewhere as well (For name changes)

  end subroutine putNodesInDocument

  subroutine removeNodesFromDocument(doc, arg)
    type(Node), pointer :: doc, arg
    type(Node), pointer :: this, treeroot
    logical :: doneChildren, doneAttributes
    integer :: i_tree

    treeroot => arg
TOHW_m_dom_treewalk(`
        this%inDocument = .false.
        if (this%nodeType==ELEMENT_NODE.and.doc%docExtras%liveNodeLists) &
          call updateNodeLists(doc, getNodeName(this), "", getLocalName(this), "", getNamespaceURI(this), "")
        ! The above is a bit inefficient; really we should construct a list of all
        ! element names added/removed and then call updateNodeLists on all of them only at the end.
        call append_nl(doc%docExtras%hangingNodes, this)
',`')

  end subroutine removeNodesFromDocument

  subroutine setReadOnlyNode(arg, p, deep)
    type(Node), pointer :: arg
    logical, intent(in) :: p
    logical, intent(in) :: deep

    type(Node), pointer :: this, treeroot
    integer :: i_tree
    logical :: doneAttributes, doneChildren

    if (deep) then
      treeroot => arg
TOHW_m_dom_treewalk(`
      this%readonly = p
',`')
    else
      arg%readonly = p
    endif

  end subroutine setReadOnlyNode

TOHW_m_dom_get(logical, readonly, np%readonly)

')`'dnl

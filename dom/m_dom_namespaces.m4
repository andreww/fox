TOHW_m_dom_publics(`
  
  public :: getNamespaceNodes

')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_m_dom_get(NodeList, namespaceNodes, np%elExtras%namespaceNodes, (ELEMENT_NODE))

  TOHW_subroutine(appendNSNode, (np, prefix, namespaceURI, specified))
    type(Node), pointer :: np
    character(len=*), intent(in) :: prefix
    character(len=*), intent(in) :: namespaceURI
    logical, intent(in) :: specified

    type(Node), pointer :: nnp, dummy
    type(NodeList), pointer :: nsnodes
    integer :: i
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
    nsnodes => getNamespaceNodes(np)
    ! If we already have this prefix registered in the list, then remove it
    do i = 0, getLength(nsNodes)-1
      if (getPrefix(item(nsNodes, i))==prefix) then
        dummy => remove_nl(nsNodes, i)
        exit
      endif
    enddo

    call append_nl(nsNodes, &
      createNamespaceNode(getOwnerDocument(np), &
        prefix, namespaceURI, specified))
    call setGCState(getOwnerDocument(np), quickFix)

  end subroutine appendNSNode

  recursive TOHW_subroutine(namespaceFixup, (np))
    type(Node), pointer :: np

    type(Node), pointer :: relevantAncestor, child
    type(NamedNodeMap), pointer :: attrs
    type(NodeList), pointer :: nsNodes, nsNodesParent
    integer :: i

    if (getNodeType(np) /= ELEMENT_NODE &
      .and. getNodeType(np) /= ENTITY_REFERENCE_NODE &
      .and. getNodeType(np)/=DOCUMENT_FRAGMENT_NODE) then
      return
    endif

    if (np%nodeType==ELEMENT_NODE) then
      ! Clear all current namespace nodes:
      nsnodes => getNamespaceNodes(np)
      do i = 1, getLength(nsNodes)
        call destroyNode(nsNodes%nodes(i)%this)
      enddo
      deallocate(nsNodes%nodes)
      
      relevantAncestor => getParentNode(np)
      do while (associated(relevantAncestor))
        ! Go up (through perhaps multiple entref nodes)
        if (getNodeType(relevantAncestor)==ELEMENT_NODE) exit
        relevantAncestor => getParentNode(relevantAncestor)
      enddo
      ! Inherit from parent (or not ...)
      if (associated(relevantAncestor)) then
        nsNodesParent => getNamespaceNodes(relevantAncestor)
        allocate(nsNodes%nodes(getLength(nsNodesParent)))
        nsNodes%length = getLength(nsNodesParent)
        do i = 1, getLength(nsNodes)
          nsNodes%nodes(i)%this => &
            createNamespaceNode(getOwnerDocument(np), &
            getPrefix(item(nsNodesParent, i-1)), &
            getNamespaceURI(item(nsNodesParent, i-1)), &
            specified=.false.)
        enddo
      else
        allocate(nsNodes%nodes(0))
        nsNodes%length = 0
      endif
      
      ! Override according to declarations
      attrs => getAttributes(np)
      do i = 0, getLength(attrs)-1
        if (getNamespaceURI(item(attrs, i))=="http://www.w3.org/2000/xmlns/") then
          if (getLocalName(item(attrs, i))=="xmlns") then
            call appendNSNode(np, "", getValue(item(attrs, i)), specified=.true.)
          else
            call appendNSNode(np, getLocalName(item(attrs, i)), &
              getValue(item(attrs, i)), specified=.true.)
          endif
        endif
      enddo
    endif
    ! And now call this on all appropriate children ...
    child => getFirstChild(np)
    do while (associated(child))
      call namespaceFixup(child)
      child => getNextSibling(child)
    enddo

  end subroutine namespaceFixup

')`'dnl

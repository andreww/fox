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

    type(Node), pointer :: relevantAncestor, child, attr
    type(NamedNodeMap), pointer :: attrs
    type(NodeList), pointer :: nsNodes, nsNodesParent
    integer :: i, nsIndex

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

      ! Now check for broken NS declarations, and add namespace
      ! nodes for all non-broken declarations
      do i = 0, getLength(attrs)-1
        attr => item(attrs, i)
        if ((getLocalName(attr)=="xmlns" &
          .or.getPrefix(attr)=="xmlns") &
          .and.getNamespaceURI(attr)/="http://www.w3.org/2000/xmlns/") then
          ! This can only I think happen if we bugger about with setPrefix ...
          TOHW_m_dom_throw_error(NAMESPACE_ERR)
        endif
        if (getNamespaceURI(attr)=="http://www.w3.org/2000/xmlns/") then
          if (getLocalName(attr)=="xmlns") then
            call appendNSNode(np, "", getValue(attr), specified=.true.)
          else
            call appendNSNode(np, getLocalName(attr), &
              getValue(attr), specified=.true.)
          endif
        endif
      enddo


      if (getNamespaceURI(np)/="") then
        if (lookupNamespaceURI(np, getPrefix(np))/=getNamespaceURI(np)) then
          ! This is a namespaced node, but its nsURI
          ! is not bound to its prefix.
          ! This will automatically do any necessary replacements ...
          if (getPrefix(np)=="") then
            ! We are dealing with the default prefix
            call setAttributeNS(np, "http://www.w3.org/2000/xmlns/", &
              "xmlns", getNamespaceURI(np))
          else
            call setAttributeNS(np, "http://www.w3.org/2000/xmlns/", &
              "xmlns:"//getPrefix(np), getNamespaceURI(np))
          endif
          ! and add a namespace node (so that we can do lookups on it)
          call appendNSNode(np, getPrefix(np), getNamespaceURI(np), specified=.true.)
        endif ! else it was already declared ...
      else
        ! No (or empty) namespace URI ...
        if (getLocalName(np)=="") then
          ! DOM level 1 node ... report error
          TOHW_m_dom_throw_error(NAMESPACE_ERR)
        else
          ! We must declare the elements prefix to have an empty nsURI
          if (lookupNamespaceURI(np, getPrefix(np))/="") then
            if (getPrefix(np)=="") then
              call setAttributeNS(np, "http://www.w3.org/2000/xmlns/", &
                "xmlns", "")
            else
              call setAttributeNS(np, "http://www.w3.org/2000/xmlns/", &
                "xmlns:"//getPrefix(np), "")
            endif
            ! and add a namespace node for the empty nsURI
            call appendNSNode(np, getPrefix(np), "", specified=.true.)
          endif
        endif
      endif

      do i = 0, getLength(attrs)-1
        ! This loops over the number of attrs present initially, so any we
        ! add within this loop will not get checked - but they will only
        ! be namespace declarations about which we dont care anyway.
        attr => item(attrs, i)
        if (getNamespaceURI(attr)=="http://www.w3.org/2000/xmlns/") then
          cycle ! We already worried about it above.
        elseif (getNamespaceURI(attr)/="") then
          ! This is a namespaced attribute
          if (getPrefix(attr)=="" &
            .or. lookupNamespaceURI(np, getPrefix(attr))/=getNamespaceURI(attr)) then
            ! It has an inappropriate prefix
            if (lookupPrefix(np, getNamespaceURI(attr))/="") then
              ! then an appropriate prefix exists, use it.
              call setPrefix(attr, lookupPrefix(np, getNamespaceURI(attr)))
              ! FIXME should be "most local" prefix. Make sure lookupPrefix does that.
            else
              ! No suitable prefix exists, declare one.
              if (getPrefix(attr)/="") then
                ! Then the current prefix is not in use, its just undeclared.
                call setAttributeNS(np, "http://www.w3.org/2000/xmlns/", &
                  "xmlns:"//getPrefix(attr), getNamespaceURI(attr))
                call appendNSNode(np, getPrefix(attr), getNamespaceURI(attr), specified=.true.)
              else
                ! This node has no prefix, but needs one. Make it up.
                nsIndex = 1
                do while (lookupNamespaceURI(np, "NS"//nsIndex)/="")
                  ! FIXME this will exit if the namespace is undeclared *or* if it is declared to be empty.
                  nsIndex = nsIndex+1
                enddo
                call setAttributeNS(np, "http://www.w3.org/2000/xmlns/", &
                  "xmlns:NS"//nsIndex, getNamespaceURI(attr))
                ! and create namespace node
                call setPrefix(attr, "NS"//nsIndex)
              endif
            endif
          endif
        else 
          ! attribute has no namespace URI
          if (getLocalName(np)=="") then
            ! DOM level 1 node ... report error
            TOHW_m_dom_throw_error(NAMESPACE_ERR)
          endif
          ! otherwise no problem
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

TOHW_m_dom_publics(`

!FIXME lots of these should have a check if(namespaces) checkNCName

  public :: getDocType
  public :: getImplementation
  public :: getDocumentElement
  public :: setDocumentElement
  
  public :: createElement
  public :: createDocumentFragment
  public :: createTextNode
  public :: createComment
  public :: createCdataSection
  public :: createProcessingInstruction
  public :: createAttribute
  public :: createEntityReference
  public :: createEmptyEntityReference
  public :: getElementsByTagName
  public :: importNode
  public :: createElementNS
  public :: createAttributeNS
  public :: getElementsByTagNameNS
  public :: getElementById
  public :: getXmlStandalone
  public :: setXmlStandalone
  public :: getXmlVersion
  public :: setXmlVersion
  public :: getXmlEncoding
  public :: getInputEncoding
  public :: getDocumentURI
  public :: setDocumentURI
  public :: getStrictErrorChecking
  public :: setStrictErrorChecking
  public :: getDomConfig
  public :: renameNode

  public :: setDocType
  public :: setDomConfig
  public :: setXds
  public :: createNamespaceNode
  public :: createEntity
  public :: createNotation
  public :: setGCstate
  public :: getXds
  public :: getLiveNodeLists
  public :: setLiveNodeLists
')`'dnl
dnl
TOHW_m_dom_contents(`

TOHW_m_dom_get(Node, docType, np%docExtras%docType, (DOCUMENT_NODE))

  TOHW_subroutine(setDocType, (arg, np))
    type(Node), pointer :: arg
    type(Node), pointer :: np
 
    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    
    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    arg%docExtras%docType => np
!NB special case in order to set ownerDocument
    np%ownerDocument => arg
  end subroutine setDocType

TOHW_m_dom_get(Node, documentElement, np%docExtras%documentElement, (DOCUMENT_NODE))

  TOHW_subroutine(setXds, (arg, xds))
    type(Node), pointer :: arg
    type(xml_doc_state), pointer :: xds

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
!NB special case in order to destroy_xml_doc_state etc
    call destroy_xml_doc_state(arg%docExtras%xds)
    deallocate(arg%docExtras%xds)
    arg%docExtras%xds => xds

  end subroutine setXds

  TOHW_function(getImplementation, (arg), imp)
    type(Node), pointer, optional :: arg
    type(DOMImplementation), pointer :: imp

    ! According to the testsuite, you get to call
    ! getImplementation with no args. Dont know
    ! where they get that from ...
    if (present(arg)) then
      if (.not.associated(arg)) then
        TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
      endif
      
      if (arg%nodeType/=DOCUMENT_NODE) then
        TOHW_m_dom_throw_error(FoX_INVALID_NODE)
      endif
      
      imp => arg%docExtras%implementation
    else
      imp => FoX_DOM
    endif
  end function getImplementation


  TOHW_subroutine(setDocumentElement, (arg, np))
  ! Only for use by FoX, not exported through FoX_DOM interface
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

!NB special case due to additional error conditions:

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (np%nodeType/=ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.associated(np%ownerDocument, arg)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif
    
    arg%docExtras%documentElement => np

  end subroutine setDocumentElement

  ! Methods

  TOHW_function(createElement, (arg, tagName), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: tagName
    type(Node), pointer :: np

    type(xml_doc_state), pointer :: xds
    type(element_t), pointer :: elem
    type(attribute_t), pointer :: att
    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(tagName, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif
    
    np => createNode(arg, ELEMENT_NODE, tagName, "")
    allocate(np%elExtras)
    np%elExtras%dom1 = .true.
    np%elExtras%attributes%ownerElement => np
    allocate(np%elExtras%namespaceURI(0))
    allocate(np%elExtras%prefix(0))
    allocate(np%elExtras%localname(0))
    allocate(np%elExtras%namespaceNodes%nodes(0))

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
      ! We only add default attributes if we are *not* building the doc
      xds => getXds(arg)
      elem => get_element(xds%element_list, tagName)
      if (associated(elem)) then
        do i = 1, get_attlist_size(elem)
          att => get_attribute_declaration(elem, i)
          if (attribute_has_default(att)) then
            ! Since this is a non-namespaced function, we create
            ! a non-namespaced attribute ...
            call setAttribute(np, str_vs(att%name), str_vs(att%default))
          endif
        enddo
      endif
    else
      np%inDocument = .true.
    endif

  end function createElement
    
  TOHW_function(createDocumentFragment, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    np => createNode(arg, DOCUMENT_FRAGMENT_NODE, "#document-fragment", "")
    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createDocumentFragment

  TOHW_function(createTextNode, (arg, data), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(data, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif

    np => createNode(arg, TEXT_NODE, "#text", data)
    np%textContentLength = len(data)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif
   
  end function createTextNode

  TOHW_function(createComment, (arg, data), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(data, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    elseif (index(data,"--")>0) then   
      TOHW_m_dom_throw_error(FoX_INVALID_COMMENT)
    endif
  
    np => createNode(arg, COMMENT_NODE, "#comment", data)
    np%textContentLength = len(data)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createComment

  TOHW_function(createCdataSection, (arg, data), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(data, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    elseif (index(data,"]]>")>0) then   
      TOHW_m_dom_throw_error(FoX_INVALID_CDATA_SECTION)
    endif
  
    np => createNode(arg, CDATA_SECTION_NODE, "#cdata-section", data)
    np%textContentLength = len(data)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif
  
  end function createCdataSection

  TOHW_function(createProcessingInstruction, (arg, target, data), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(target, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkChars(data, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    elseif (index(data,"?>")>0) then   
      TOHW_m_dom_throw_error(FoX_INVALID_PI_DATA)
    endif

    np => createNode(arg, PROCESSING_INSTRUCTION_NODE, target, data)
    np%textContentLength = len(data)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createProcessingInstruction

  TOHW_function(createAttribute, (arg, name), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(name, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif
  
    np => createNode(arg, ATTRIBUTE_NODE, name, "")
    allocate(np%elExtras)
    np%elExtras%dom1 = .true.
    allocate(np%elExtras%namespaceURI(0))
    allocate(np%elExtras%prefix(0))
    allocate(np%elExtras%localname(0))

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif
  
  end function createAttribute


  recursive TOHW_function(createEntityReference, (arg, name), np)
  ! Needs to be recursive in case of entity-references within each other.
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(Node), pointer :: ent, newNode
    integer :: i
    logical :: brokenNS

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif
    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(name, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif

    if (getXmlStandalone(arg).and..not.associated(getDocType(arg))) then
      TOHW_m_dom_throw_error(FoX_NO_SUCH_ENTITY)
    endif

    np => createNode(arg, ENTITY_REFERENCE_NODE, name, "")
    if (getGCstate(arg)) then ! otherwise the parser will fill these nodes in itself
      if (associated(getDocType(arg))) then
        ent => getNamedItem(getEntities(getDocType(arg)), name)
        if (associated(ent)) then
          if (getIllFormed(ent)) then
            TOHW_m_dom_throw_error(FoX_INVALID_ENTITY)
          endif
          brokenNS = arg%docExtras%brokenNS
          arg%docExtras%brokenNS = .true. ! We need to not worry about NS errors for a bit
          do i = 0, getLength(getChildNodes(ent)) - 1
            newNode => appendChild(np, cloneNode(item(getChildNodes(ent), i), .true., ex))
            ! No namespace calcs here - wait for a namespace normalization
            call setReadOnlyNode(newNode, .true., .true.)
          enddo
          arg%docExtras%brokenNS = brokenNS ! FIXME also for all new default attributes
        elseif (getXmlStandalone(arg)) then
          TOHW_m_dom_throw_error(FoX_NO_SUCH_ENTITY, (np))
        endif
      endif
    endif

    call setReadOnlyNode(np, .true., .false.)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append_nl(arg%docExtras%hangingNodes, np)
      ! All child nodes were created outside the document by cloneNode above
    else
      np%inDocument = .true.
    endif

  end function createEntityReference

  TOHW_function(createEmptyEntityReference, (arg, name), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(name, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif

    np => createNode(arg, ENTITY_REFERENCE_NODE, name, "")
    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createEmptyEntityReference

  TOHW_function(getElementsByTagName, (doc, tagName, name), list)
    type(Node), pointer :: doc
    character(len=*), intent(in), optional :: tagName, name
    type(NodeList), pointer :: list

    type(NodeListPtr), pointer :: nll(:), temp_nll(:)
    type(Node), pointer :: arg, this, treeroot
    logical :: doneChildren, doneAttributes, allElements
    integer :: i, i_tree

    if (.not.associated(doc)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (doc%nodeType==DOCUMENT_NODE) then
      if (present(name).or..not.present(tagName)) then
        TOHW_m_dom_throw_error(FoX_INVALID_NODE)
      endif
    elseif (doc%nodeType==ELEMENT_NODE) then
      if (present(name).or..not.present(tagName)) then
        TOHW_m_dom_throw_error(FoX_INVALID_NODE)
      endif
    else      
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    if (doc%nodeType==DOCUMENT_NODE) then
      arg => getDocumentElement(doc)
    else
      arg => doc
    endif

    allocate(list)
    allocate(list%nodes(0))
    list%element => doc
    if (present(name)) list%nodeName => vs_str_alloc(name)
    if (present(tagName)) list%nodeName => vs_str_alloc(tagName)

    allElements = (str_vs(list%nodeName)=="*")

    if (doc%nodeType==DOCUMENT_NODE) then
      nll => doc%docExtras%nodelists
    elseif (doc%nodeType==ELEMENT_NODE) then
      nll => doc%ownerDocument%docExtras%nodelists
    endif
    allocate(temp_nll(size(nll)+1))
    do i = 1, size(nll)
      temp_nll(i)%this => nll(i)%this
    enddo
    temp_nll(i)%this => list
    deallocate(nll)
    if (doc%nodeType==DOCUMENT_NODE) then
      doc%docExtras%nodelists => temp_nll
    elseif (doc%nodeType==ELEMENT_NODE) then
      doc%ownerDocument%docExtras%nodelists => temp_nll
    endif

    treeroot => arg
TOHW_m_dom_treewalk(`dnl
        if (this%nodeType==ELEMENT_NODE) then
          if ((allElements .or. str_vs(this%nodeName)==tagName) &
            .and..not.(getNodeType(doc)==ELEMENT_NODE.and.associated(this, arg))) &
            call append(list, this)
          doneAttributes = .true.
        endif
',`')

  end function getElementsByTagName

  TOHW_function(importNode, (doc, arg, deep) , np)
    type(Node), pointer :: doc
    type(Node), pointer :: arg
    logical, intent(in) :: deep
    type(Node), pointer :: np

    type(Node), pointer :: this, thatParent, new, treeroot
    type(xml_doc_state), pointer :: xds
    type(element_t), pointer :: elem
    type(attribute_t), pointer :: att
    logical :: doneAttributes, doneChildren, brokenNS
    integer :: i_tree

    if (.not.associated(doc).or..not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(doc)/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (getNodeType(arg)==DOCUMENT_NODE .or. &
      getNodeType(arg)==DOCUMENT_TYPE_NODE) then
      TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
    endif
    brokenNS = doc%docExtras%brokenNS
    doc%docExtras%brokenNS = .true. ! We need to do stupid NS things
    xds => getXds(doc)
    thatParent => null()
    treeroot => arg
    TOHW_m_dom_treewalk(`

        new => null()
        select case (getNodeType(this))
        case (ELEMENT_NODE)
          if (.not.doneAttributes) then
            if (getParameter(getDomConfig(doc), "namespaces")) then
              new => createElementNS(doc, getNamespaceURI(this), getTagName(this))
            else
              new => createElement(doc, getTagName(this))
            endif
          endif
        case (ATTRIBUTE_NODE)
          if (associated(this, arg).or.getSpecified(this)) then
            ! We are importing just this attribute node
            ! or this was an explicitly specified attribute; either
            ! way, we import it as is, and it remains specified.
            if (getParameter(getDomConfig(doc), "namespaces")) then
              new => createAttributeNS(doc, getNamespaceURI(this), getName(this))
            else
              new => createAttribute(doc, getName(this))
            endif
            call setSpecified(new, .true.)
          else
            ! This is an attribute being imported as part of a hierarchy,
            ! but its only here by default. Is there a default attribute
            ! of this name in the new document?
            elem => get_element(xds%element_list, &
              getTagName(getOwnerElement(this)))
            att => get_attribute_declaration(elem, getName(this))
            if (attribute_has_default(att)) then
              ! Create the new default:
              if (getParameter(getDomConfig(arg), "namespaces")) then
                ! We create a namespaced attribute. Of course, its 
                ! namespaceURI remains empty for the moment unless we know it ...
                if (prefixOfQName(getName(this))=="xml") then
                  new => createAttributeNS(np, &
                    "http://www.w3.org/XML/1998/namespace", &
                    getName(this))
                elseif (getName(this)=="xmlns" & 
                  .or. prefixOfQName(getName(this))=="xmlns") then
                  new => createAttributeNS(np, &
                    "http://www.w3.org/2000/xmlns/", &
                    getName(this))
                else
                  ! Wait for namespace fixup ...
                  new => createAttributeNS(np, "", &
                    getName(this))
                endif
              else
                new => createAttribute(doc, getName(this))
              endif
              call setValue(new, str_vs(att%default))
              call setSpecified(new, .false.)
              ! In any case, we dont want to copy the children of this node.
              doneChildren=.true.
            endif
            ! Otherwise no attribute here
          endif
        case (TEXT_NODE)
          new => createTextNode(doc, getData(this))
        case (CDATA_SECTION_NODE)
          new => createCDataSection(doc, getData(this))
        case (ENTITY_REFERENCE_NODE)
          new => createEntityReference(doc, getNodeName(this))
          ! This will automatically populate the entity reference if doc defines it, so no children needed
          doneChildren = .true.
        case (ENTITY_NODE)
          new => createEntity(doc, getNodeName(this), getPublicId(this), getSystemId(this), getNotationName(this))
        case (PROCESSING_INSTRUCTION_NODE)
          new => createProcessingInstruction(doc, getTarget(this), getData(this))
        case (COMMENT_NODE)
          new => createComment(doc, getData(this))
        case (DOCUMENT_NODE)
          TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
        case (DOCUMENT_TYPE_NODE)
          TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
        case (DOCUMENT_FRAGMENT_NODE)
          new => createDocumentFragment(doc)
        case (NOTATION_NODE)
          new => createNotation(doc, getNodeName(this), getPublicId(this), getSystemId(this))
        end select
 
        if (.not.associated(thatParent)) then
          thatParent => new
        elseif (associated(new)) then
          if (getNodeType(this)==ATTRIBUTE_NODE) then
            new => setAttributeNode(thatParent, new)
          else
            new => appendChild(thatParent, new)
          endif
        endif

        if (.not.deep) then
          if (getNodeType(arg)==ATTRIBUTE_NODE.or.getNodeType(arg)==ELEMENT_NODE) then
            continue
          else
            exit
          endif
        endif
', `', `parentNode', `')

    np => thatParent
    doc%docExtras%brokenNS = brokenNS
!    call namespaceFixup(np)

  end function importNode

  TOHW_function(createElementNS, (arg, namespaceURI, qualifiedName), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: np

    type(xml_doc_state), pointer :: xds
    type(element_t), pointer :: elem
    type(attribute_t), pointer :: att
    integer :: i
    logical :: brokenNS

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(qualifiedName, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedName, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="".and..not.arg%docExtras%brokenNS) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (namespaceURI=="http://www.w3.org/XML/1998/namespace" .neqv. &
      prefixOfQName(qualifiedName)=="xml") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (namespaceURI=="http://www.w3.org/2000/xmlns/") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    endif

    np => createNode(arg, ELEMENT_NODE, qualifiedName, "")
    allocate(np%elExtras)
    np%elExtras%namespaceURI => vs_str_alloc(namespaceURI)
    np%elExtras%prefix => vs_str_alloc(prefixOfQName(qualifiedname))
    np%elExtras%localName => vs_str_alloc(localpartOfQName(qualifiedname))
    allocate(np%elExtras%namespaceNodes%nodes(0))

    np%elExtras%attributes%ownerElement => np

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
      ! We only add default attributes if we are *not* building the doc
      xds => getXds(arg)
      elem => get_element(xds%element_list, qualifiedName)
      if (associated(elem)) then
        do i = 1, get_attlist_size(elem)
          att => get_attribute_declaration(elem, i)
          if (attribute_has_default(att)) then
            ! Since this is a namespaced function, we create a namespaced
            ! attribute. Of course, its namespaceURI remains empty
            ! for the moment unless we know it ...
            if (prefixOfQName(str_vs(att%name))=="xml") then
              call setAttributeNS(np, &
                "http://www.w3.org/XML/1998/namespace", &
                str_vs(att%name), str_vs(att%default))
            elseif (str_vs(att%name)=="xmlns" & 
              .or. prefixOfQName(str_vs(att%name))=="xmlns") then
              call setAttributeNS(np, &
                "http://www.w3.org/2000/xmlns/", &
                str_vs(att%name), str_vs(att%default))
            else
              ! Wait for namespace fixup ...
              brokenNS = arg%docExtras%brokenNS
              arg%docExtras%brokenNS = .true.
              call setAttributeNS(np, "", str_vs(att%name), &
                str_vs(att%default))
              arg%docExtras%brokenNS = brokenNS
            endif
          endif
        enddo
      endif
    else
      np%inDocument = .true.
    endif

  end function createElementNS
  
  TOHW_function(createAttributeNS, (arg, namespaceURI,  qualifiedname), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(qualifiedName, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedName, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="".and..not.arg%docExtras%brokenNS) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (namespaceURI=="http://www.w3.org/XML/1998/namespace" .neqv. &
      prefixOfQName(qualifiedName)=="xml") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (namespaceURI=="http://www.w3.org/2000/xmlns/" .neqv. &
      (qualifiedName=="xmlns" .or. prefixOfQName(qualifiedName)=="xmlns")) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    endif
  
    np => createNode(arg, ATTRIBUTE_NODE, qualifiedName, "")
    allocate(np%elExtras)
    np%elExtras%namespaceURI => vs_str_alloc(namespaceURI)
    np%elExtras%localname => vs_str_alloc(localPartofQName(qualifiedname))
    np%elExtras%prefix => vs_str_alloc(PrefixofQName(qualifiedname))

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createAttributeNS

  TOHW_function(getElementsByTagNameNS, (doc, namespaceURI, localName), list)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, localName
    type(NodeList), pointer :: list

    type(NodeListPtr), pointer :: nll(:), temp_nll(:)
    type(Node), pointer :: this, arg, treeroot
    logical :: doneChildren, doneAttributes, allLocalNames, allNameSpaces
    integer :: i, i_tree

    if (.not.associated(doc)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (doc%nodeType/=DOCUMENT_NODE.and.doc%nodeType/=ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    allNamespaces = (namespaceURI=="*")
    allLocalNames = (localName=="*")

    if (doc%nodeType==DOCUMENT_NODE) then
      arg => getDocumentElement(doc)
    else
      arg => doc
    endif

    allocate(list)
    allocate(list%nodes(0))
    list%element => doc
    list%localName => vs_str_alloc(localName)
    list%namespaceURI => vs_str_alloc(namespaceURI)

    if (doc%nodeType==DOCUMENT_NODE) then
      nll => doc%docExtras%nodelists
    elseif (doc%nodeType==ELEMENT_NODE) then
      nll => doc%ownerDocument%docExtras%nodelists
    endif
    allocate(temp_nll(size(nll)+1))
    do i = 1, size(nll)
      temp_nll(i)%this => nll(i)%this
    enddo
    temp_nll(i)%this => list
    deallocate(nll)
    if (doc%nodeType==DOCUMENT_NODE) then
      doc%docExtras%nodelists => temp_nll
    elseif (doc%nodeType==ELEMENT_NODE) then
      doc%ownerDocument%docExtras%nodelists => temp_nll
    endif

    treeroot => arg
TOHW_m_dom_treewalk(`dnl

      if (getNodeType(this)==ELEMENT_NODE) then
        if (getNamespaceURI(this)/="") then
          if ((allNameSpaces .or. getNameSpaceURI(this)==namespaceURI) &
            .and. (allLocalNames .or. getLocalName(this)==localName) &
            .and..not.(getNodeType(doc)==ELEMENT_NODE.and.associated(this, arg))) &
            call append(list, this)
        else
          if ((allNameSpaces .or. namespaceURI=="") &
            .and. (allLocalNames .or. getNodeName(this)==localName) &
            .and..not.(getNodeType(doc)==ELEMENT_NODE.and.associated(this, arg))) &
            call append(list, this)
        endif
        doneAttributes = .true. ! Never search attributes
      endif
',`')

  end function getElementsByTagNameNS


  TOHW_function(getElementById, (arg, elementId), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: elementId
    type(Node), pointer :: np

    type(Node), pointer :: this, treeroot
    integer :: i_tree
    logical :: doneChildren, doneAttributes

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => null()
    treeroot => getDocumentElement(arg)
TOHW_m_dom_treewalk(`dnl
      if (this%nodeType==ATTRIBUTE_NODE)  then
        if (getIsId(this).and.getName(this)==elementId) then
          np => getOwnerElement(this)
          return
        endif
      endif
',`')

  end function getElementById

TOHW_m_dom_get(logical, xmlStandalone, np%docExtras%xds%standalone, (DOCUMENT_NODE))
TOHW_m_dom_set(logical, xmlStandalone, np%docExtras%xds%standalone, (DOCUMENT_NODE))
! FIXME additional check on setting - do we have any undefined entrefs present?

  TOHW_function(getXmlVersion, (arg), s)
    type(Node), pointer :: arg
    character(len=3) :: s

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE &
    .and.arg%nodeType/=ENTITY_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    if (getXmlVersionEnum(arg)==XML1_0) then
      s = "1.0"
    elseif (getXmlVersionEnum(arg)==XML1_1) then
      s = "1.1"
    else
      s = "XXX"
    endif

  end function getXmlVersion

  TOHW_subroutine(setXmlVersion, (arg, s))
    type(Node), pointer :: arg
    character(len=*) :: s

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    if (s=="1.0") then
      arg%docExtras%xds%xml_version = XML1_0
    elseif (s=="1.1") then
      arg%docExtras%xds%xml_version = XML1_1
    else
      TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
    endif

  end subroutine setXmlVersion

  pure function getXmlEncoding_len(arg, p) result(n)
    type(Node), pointer :: arg
    logical, intent(in) :: p
    integer :: n

    n = 0
    if (.not.p) return
    if (arg%nodeType==DOCUMENT_NODE) &
      n = size(arg%docExtras%xds%encoding)
  end function getXmlEncoding_len

  TOHW_function(getXmlEncoding, (arg), s)
    type(Node), pointer :: arg
#ifdef RESTRICTED_ASSOCIATED_BUG
    character(len=getXmlEncoding_len(arg, .true.)) :: s
#else
    character(len=getXmlEncoding_len(arg, associated(arg))) :: s
#endif

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType==DOCUMENT_NODE) then
      s = str_vs(arg%docExtras%xds%encoding)
    elseif (arg%nodeType==ENTITY_NODE) then
      s = "" !FIXME revisit when we have working external entities
    else
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

  end function getXmlEncoding

  pure function getInputEncoding_len(arg, p) result(n)
    type(Node), pointer :: arg
    logical, intent(in) :: p
    integer :: n

    n = 0
    if (.not.p) return
    if (arg%nodeType==DOCUMENT_NODE) &
      n = size(arg%docExtras%xds%inputEncoding)
  end function getInputEncoding_len

  TOHW_function(getInputEncoding, (arg), s)
    type(Node), pointer :: arg
#ifdef RESTRICTED_ASSOCIATED_BUG
    character(len=getInputEncoding_len(arg, .true.)) :: s
#else
    character(len=getInputEncoding_len(arg, associated(arg))) :: s
#endif

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType==DOCUMENT_NODE) then
      s = str_vs(arg%docExtras%xds%inputEncoding)    
    elseif (arg%nodeType==ENTITY_NODE) then
      s = "" !FIXME revisit when we have working external entities
    else
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

  end function getInputEncoding

TOHW_m_dom_get(DOMString, documentURI, np%docExtras%xds%documentURI, (DOCUMENT_NODE))
TOHW_m_dom_set(DOMString, documentURI, np%docExtras%xds%documentURI, (DOCUMENT_NODE))

TOHW_m_dom_get(logical, strictErrorChecking, np%docExtras%strictErrorChecking, (DOCUMENT_NODE))
TOHW_m_dom_set(logical, strictErrorChecking, np%docExtras%strictErrorChecking, (DOCUMENT_NODE))

!  function adoptNode FIXME

TOHW_m_dom_get(DOMConfiguration, domConfig, np%docExtras%domConfig, (DOCUMENT_NODE))
TOHW_m_dom_set(DOMConfiguration, domConfig, np%docExtras%domConfig, (DOCUMENT_NODE))

dnl subroutine normalizeDocument - see m_dom_namespaces.m4

  TOHW_function(renameNode, (arg, n, namespaceURI, qualifiedName), np)
    type(Node), pointer :: arg
    type(Node), pointer :: n
    character(len=*), intent(in) :: namespaceURI
    character(len=*), intent(in) :: qualifiedName
    type(Node), pointer :: np
    
    type(Node), pointer :: attNode
    integer :: i
    logical :: brokenNS
    type(element_t), pointer :: elem
    type(attribute_t), pointer :: att
    type(xml_doc_state), pointer :: xds

    if (.not.associated(arg).or..not.associated(n)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.associated(arg, getOwnerDocument(n))) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    elseif (checkName(qualifiedName, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedName, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="".and..not.arg%docExtras%brokenNS) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (namespaceURI=="http://www.w3.org/XML/1998/namespace" .neqv. &
      prefixOfQName(qualifiedName)=="xml") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (namespaceURI=="http://www.w3.org/2000/xmlns/") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    endif

! FIXME what if this is called on a Level 1 node
! FIXME what if this is called on a read-only node
! FIXME what if this is called on an attribute whose specified=fals
    select case(getNodeType(n))
    case (ELEMENT_NODE, ATTRIBUTE_NODE)
      deallocate(n%nodeName)
      n%nodeName => vs_str_alloc(qualifiedName)
      deallocate(n%elExtras%namespaceURI)
      n%elExtras%namespaceURI => vs_str_alloc(namespaceURI)
      deallocate(n%elExtras%localName)
      np%elExtras%localName => vs_str_alloc(localpartOfQName(qualifiedname))
    case default
      TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
    end select

    if (getNodeType(n)==ELEMENT_NODE) then
      i = 0
      do while (i<getLength(getAttributes(n)))
        attNode => item(getAttributes(n), i)
        if (.not.getSpecified(attNode)) then
          attNode => removeAttributeNode(n, attNode)
          call destroyNode(attNode)
        else
          i = i + 1
        endif
      enddo
      xds => getXds(arg)
      elem => get_element(xds%element_list, qualifiedName)
      if (associated(elem)) then
        do i = 1, get_attlist_size(elem)
          att => get_attribute_declaration(elem, i)
          if (attribute_has_default(att)) then
            ! Since this is a namespaced function, we create a namespaced
            ! attribute. Of course, its namespaceURI remains empty
            ! for the moment unless we know it ...
            if (prefixOfQName(str_vs(att%name))=="xml") then
              call setAttributeNS(np, &
                "http://www.w3.org/XML/1998/namespace", &
                str_vs(att%name), str_vs(att%default))
            elseif (str_vs(att%name)=="xmlns" & 
              .or. prefixOfQName(str_vs(att%name))=="xmlns") then
              call setAttributeNS(np, &
                "http://www.w3.org/2000/xmlns/", &
                str_vs(att%name), str_vs(att%default))
            else
              ! Wait for namespace fixup ...
              brokenNS = arg%docExtras%brokenNS
              arg%docExtras%brokenNS = .true.
              call setAttributeNS(np, "", str_vs(att%name), &
                str_vs(att%default))
              arg%docExtras%brokenNS = brokenNS
            endif
          endif
        enddo
      endif
    endif

    np => n

  end function renameNode
      
  ! Internal function, not part of API

  TOHW_function(createNamespaceNode, (arg, prefix, URI, specified), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: prefix
    character(len=*), intent(in) :: URI
    logical, intent(in) :: specified
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => createNode(arg, XPATH_NAMESPACE_NODE, "#namespace", URI)
    allocate(np%elExtras)
    np%elExtras%prefix => vs_str_alloc(prefix)
    np%elExtras%namespaceURI => vs_str_alloc(URI)
    np%elExtras%specified = specified

  end function createNamespaceNode

  TOHW_function(createEntity, (arg, name, publicId, systemId, notationName), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    character(len=*), intent(in) :: notationName
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => createNode(arg, ENTITY_NODE, name, "")
    allocate(np%dtdExtras)
    np%dtdExtras%publicId => vs_str_alloc(publicId)
    np%dtdExtras%systemId => vs_str_alloc(systemId)
    np%dtdExtras%notationName => vs_str_alloc(notationName)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createEntity

  TOHW_function(createNotation, (arg, name, publicId, systemId), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => createNode(arg, NOTATION_NODE, name, "")
    allocate(np%dtdExtras)
    np%dtdExtras%publicId => vs_str_alloc(publicId)
    np%dtdExtras%systemId => vs_str_alloc(systemId)
    
    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createNotation

  TOHW_function(getXmlVersionEnum, (arg), n)
    type(Node), pointer :: arg
    integer :: n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    n = arg%docExtras%xds%xml_version

  end function getXmlVersionEnum

  TOHW_function(getXds, (arg), xds)
    type(Node), pointer :: arg
    type(xml_doc_state), pointer :: xds

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    xds => arg%docExtras%xds

  end function getXds


TOHW_m_dom_get(logical, GCstate, np%docExtras%xds%building, (DOCUMENT_NODE))
TOHW_m_dom_set(logical, GCstate, np%docExtras%xds%building, (DOCUMENT_NODE))

TOHW_m_dom_get(logical, liveNodeLists, np%docExtras%liveNodeLists, (DOCUMENT_NODE))
TOHW_m_dom_set(logical, liveNodeLists, np%docExtras%liveNodeLists, (DOCUMENT_NODE))

')`'dnl

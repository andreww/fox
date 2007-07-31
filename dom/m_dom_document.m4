TOHW_m_dom_publics(`

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
  public :: getXmlVersion
  public :: setXmlVersion

  public :: setDocType
  public :: setXds
  public :: setEntityReferenceValue
  public :: createNamespaceNode
  public :: createEntity
  public :: createNotation
  public :: setGCstate
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
    type(Node), pointer :: arg
    type(DOMImplementation), pointer :: imp

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    imp => arg%docExtras%implementation
    
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

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(tagName, getXds(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif
    
    np => createNode(arg, ELEMENT_NODE, tagName, "")
    allocate(np%elExtras)
    np%elExtras%attributes%ownerElement => np

    ! FIXME set namespaceURI and localName appropriately

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
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
    elseif (.not.checkName(target, getXds(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkChars(data, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    elseif (index(data,"?>")>0) then   
      TOHW_m_dom_throw_error(FoX_INVALID_PI_DATA)
    endif

    np => createNode(arg, PROCESSING_INSTRUCTION_NODE, target, data)

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
    elseif (.not.checkName(name, getXds(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif
  
    np => createNode(arg, ATTRIBUTE_NODE, name, "")
    allocate(np%elExtras)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif
  
  end function createAttribute

  subroutine setEntityReferenceValue(arg)
    type(Node), pointer :: arg

    type(Node), pointer :: this, treeroot
    integer :: i_tree, n
    logical :: doneAttributes, doneChildren

    ! Calculate value of any entity references that are only textual:
    n = 0
    treeroot => arg
TOHW_m_dom_treewalk(`
      if (getNodeType(this)==TEXT_NODE) then
        n = n + len(getData(this))
      elseif (getNodeType(this)/=ENTITY_REFERENCE_NODE) then
        n = 0
        exit
      endif
',`')
    deallocate(arg%nodeValue)
    allocate(arg%nodeValue(n))
    if (n>0) then
      n = 0
TOHW_m_dom_treewalk(`
        if (getNodeType(this)==TEXT_NODE) then
          arg%nodeValue(n+1:n+len(getData(this))) = getData(this)
          n = n + len(getData(this))
        endif
',`')
    endif
  end subroutine setEntityReferenceValue

  TOHW_function(createEntityReference, (arg, name), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(Node), pointer :: ent, newNode
    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(name, getXds(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif

    if (.not.associated(getDocType(arg))) then
      TOHW_m_dom_throw_error(FoX_NO_DOCTYPE)
    endif

    np => createNode(arg, ENTITY_REFERENCE_NODE, name, "")
    if (getGCstate(arg)) then ! otherwise the parser will fill these nodes in itself
      ! FIXME except I think that gets switched off when creating atts sometimes ... need to check
      if (associated(getDocType(arg))) then
        ent => getNamedItem(getEntities(getDocType(arg)), name)
        if (associated(ent)) then
          if (getIllFormed(ent)) then
            TOHW_m_dom_throw_error(FoX_INVALID_ENTITY)
          endif
          do i = 0, getLength(getChildNodes(ent)) - 1
            newNode => appendChild(np, cloneNode(item(getChildNodes(ent), i), .true., ex))
            call setReadOnlyNode(newNode, .true., .true.)
          enddo
        endif
        ! FIXME in case of recursive entity references?
      endif
    endif

    call setEntityReferenceValue(np)

    call setReadOnlyNode(np, .true., .false.)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
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
    elseif (.not.checkName(name, getXds(arg))) then
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
    logical :: deep
    type(Node), pointer :: np

    type(Node), pointer :: this, thatParent, new, treeroot

    logical :: doneAttributes, doneChildren
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

    if (associated(doc, getOwnerDocument(arg))) then
      np => cloneNode(arg, deep)
      return
    endif

    thatParent => null()
    treeroot => arg
    TOHW_m_dom_treewalk(`

        new => null()
        select case (getNodeType(this))
        case (ELEMENT_NODE)
          if (.not.doneAttributes) then
            ! Are there any new prefixes or namespaces to be declared?
            ! FIXME
            if (getNamespaceURI(this)=="") then
              new => createElement(doc, getTagName(this))
            else
              new => createElementNS(doc, getNamespaceURI(this), getTagName(this))
            endif
          endif
        case (ATTRIBUTE_NODE)
          if (associated(this, arg).or.getSpecified(this)) then
            ! We are importing just this attribute node
            ! or this was an explicitly specified attribute; either
            ! way, we import it as is, and it remains specified.
            if (getNamespaceURI(this)=="") then
              new => createAttribute(doc, getName(this))
            else
              new => createAttributeNS(doc, getNamespaceURI(this), getName(this))
            endif
            call setSpecified(new, .true.)
            !else FIXME
            ! This is an attribute being imported as part of a hierarchy,
            ! but its only here by default. Is there a default attribute
            ! of this name in the new document?
            ! elseif (thereIsADefault(getName(this)) FIXME
            ! new => createAttribute(doc, getName(this))
            ! call setValue(new, defaultValue)
            ! call setSpecified(new, .false.)
          else
            doneChildren = .true.
          endif
        case (TEXT_NODE)
          new => createTextNode(doc, getData(this))
        case (CDATA_SECTION_NODE)
          new => createCDataSection(doc, getData(this))
        case (ENTITY_REFERENCE_NODE)
          new => createEntityReference(doc, getNodeName(this))
          ! FIXME DOES ENTREF EXIST IN NEW DOC? Switch "this" appropriately & carry on cloning
        case (ENTITY_NODE)
          new => createEntity(doc, getName(this), getPublicId(this), getSystemId(this), getNotationName(this))
        case (PROCESSING_INSTRUCTION_NODE)
          new => createProcessingInstruction(doc, getTarget(this), getData(this))
        case (COMMENT_NODE)
          new => createEntityReference(doc, getNodeValue(this))
        case (DOCUMENT_NODE)
          TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
        case (DOCUMENT_TYPE_NODE)
          TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
        case (DOCUMENT_FRAGMENT_NODE)
          new => createDocumentFragment(doc)
        case (NOTATION_NODE)
          new => createNotation(doc, getName(this), getPublicId(this), getSystemId(this))
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
          if (getNodeType(arg)/=ELEMENT_NODE.and.getNodeType(arg)/=ATTRIBUTE_NODE) exit
        endif
', `', `parentNode', `')

    np => thatParent

  end function importNode

  TOHW_function(createElementNS, (arg, namespaceURI, qualifiedName), np)
    type(Node), pointer :: arg
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkName(qualifiedName, getXds(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedName, getXds(arg))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="") then
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

    np%elExtras%attributes%ownerElement => np

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

    ! FIXME updateNodeLists

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
    elseif (.not.checkName(qualifiedName, getXds(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedName, getXds(arg))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="") then
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
      !if (getNodeType(this)==ELEMENT_NODE .and. getNamespaceURI(this)/="") then
! FIXME again DOM seems contradictory
      if (getNodeType(this)==ELEMENT_NODE) then
        if (getNamespaceURI(this)/="") then
          if ((allNameSpaces .or. getNameSpaceURI(this)==namespaceURI) &
            .and. (allLocalNames .or. getLocalName(this)==localName)) &
            call append(list, this)
        else
          if ((allNameSpaces .or. namespaceURI=="") &
            .and. (allLocalNames .or. getNodeName(this)==localName)) &
            call append(list, this)
        endif
        doneAttributes = .true.
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

!  function getInputEncoding
!  function getXmlEncoding
!  function getXmlStandalone
!  function setXmlStandalone


  TOHW_function(getXmlVersion, (arg), s)
    type(Node), pointer :: arg
    character(len=3) :: s

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
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


!  function getStrictErrorChecking
!  function setStrictErrorChecking
!  function getDocumentURI
!  function setDocumentURI

  TOHW_subroutine(normalizeDocument, (np))
    type(Node), pointer :: np
   
    if (.not.associated(np)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (np%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    ! FIXME check domConfig features.
    ! normalize text()
    ! fixup namespaces
  end subroutine normalizeDocument

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
    type(xml_doc_state) :: xds

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    xds = arg%docExtras%xds

  end function getXds


  TOHW_function(getGCstate, (arg), b)
    type(Node), pointer :: arg
    logical :: b

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    b = arg%docExtras%xds%building

  end function getGCstate

  TOHW_subroutine(setGCstate, (arg, b))
    type(Node), pointer :: arg
    logical, intent(in) :: b

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_INTERNAL_ERROR)
    endif

    arg%docExtras%xds%building = b

  end subroutine setGCstate

')`'dnl

TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_charset, only: XML1_0, XML1_1
  use m_common_namecheck, only: checkQName, prefixOfQName, localPartOfQName
  use m_dom_error, only : NOT_FOUND_ERR, INVALID_CHARACTER_ERR, FoX_INVALID_NODE, &
    FoX_INVALID_XML_NAME, WRONG_DOCUMENT_ERR, FoX_INVALID_TEXT, & 
    FoX_INVALID_CHARACTER, FoX_INVALID_COMMENT, FoX_INVALID_CDATA_SECTION, &
    FoX_INVALID_PI_DATA, NOT_SUPPORTED_ERR, FoX_INVALID_ENTITY, FoX_NO_DOCTYPE

')`'dnl
dnl
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
  public :: createEntity
  public :: createNotation
  public :: setGCstate
')`'dnl
dnl
TOHW_m_dom_contents(`

  ! Getters and setters:

  TOHW_function(getDocType, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    np => arg%docExtras%docType

  end function getDocType

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
    np%ownerDocument => arg

  end subroutine setDocType


  TOHW_subroutine(setXds, (arg, xds))
    type(Node), pointer :: arg
    type(xml_doc_state), pointer :: xds

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
       TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
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

  TOHW_function(getDocumentElement, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => arg%docExtras%documentElement

  end function getDocumentElement

  TOHW_subroutine(setDocumentElement, (arg, np))
  ! Only for use by FoX, not exported through FoX_DOM interface
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

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
    np%attributes%ownerElement => np

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
    np%namespaceURI => vs_str_alloc("")
    np%localname => vs_str_alloc(name)
    np%prefix => vs_str_alloc(name)

    if (getGCstate(arg)) then
      np%inDocument = .false.
      call append(arg%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif
  
  end function createAttribute

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
          if (ent%illFormed) then
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

    type(Node), pointer :: ent
    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (arg%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(name, getXmlVersionEnum(arg))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkName(name, getXds(arg))) then
      TOHW_m_dom_throw_error(FoX_INVALID_XML_NAME)
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
    type(Node), pointer :: arg, this
    logical :: doneChildren, doneAttributes, allElements
    integer :: i

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

    if (tagName=="*") &
      allElements = .true.

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

    if (str_vs(list%nodeName)=="*") &
      allElements = .true.

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

    this => arg

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

    type(Node), pointer :: this, thatParent, new

    logical :: doneAttributes, doneChildren
    integer :: i
    print*,"importing Nodes"

    if (getNodeType(doc)/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    if (associated(doc, getOwnerDocument(arg))) then
      np => cloneNode(arg, deep)
      return
    endif

    this => arg
    thatParent => null()

    TOHW_m_dom_treewalk(`

        new => null()
        select case (getNodeType(this))
        case (ELEMENT_NODE)
          if (.not.doneAttributes) then
            ! Are there any new prefixes or namespaces to be declared?
            ! FIXME
            new => createElement(doc, getTagName(this))
          endif
        case (ATTRIBUTE_NODE)
          if (associated(this, arg)) then
            new => createAttribute(doc, getName(this))
            call setSpecified(new, .true.)
          elseif (getSpecified(this)) then
            new => createAttribute(doc, getName(this))
            call setSpecified(new, .true.)
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
 
        if (associated(thatParent).and.associated(new)) print*, getNodeType(thatParent), getNodeType(new)

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
          if (getNodeType(arg)/=ELEMENT_NODE.and.getNodeType(arg)/=ATTRIBUTE_NODE) return
        endif
', `', `parentNode', `')

    np => thatParent
    print*,"importDone"

  end function importNode

  TOHW_function(createElementNS, (doc, namespaceURI, qualifiedName), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(qualifiedName, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedName, getXds(doc))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)=="xml" .and. &
      namespaceURI/="http://www.w3.org/XML/1998/namespace") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    ! FIXME is this all possible errors?
      ! what if prefix = "xmlns"? or other "xml"
    endif

    ! FIXME create a namespace node for XPath?

    np => createNode(doc, ELEMENT_NODE, qualifiedName, "")
    np%namespaceURI => vs_str_alloc(namespaceURI)
    np%prefix => vs_str_alloc(prefixOfQName(qualifiedname))
    np%localName => vs_str_alloc(localpartOfQName(qualifiedname))

    np%attributes%ownerElement => np

    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

    ! FIXME updateNodeLists

  end function createElementNS
  
  TOHW_function(createAttributeNS, (doc, namespaceURI,  qualifiedname), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, qualifiedName
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(qualifiedName, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkQName(qualifiedName, getXds(doc))) then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)/="" &
     .and. namespaceURI=="") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    elseif (prefixOfQName(qualifiedName)=="xml" .and. &
      namespaceURI/="http://www.w3.org/XML/1998/namespace") then
      TOHW_m_dom_throw_error(NAMESPACE_ERR)
    ! FIXME is this all possible errors?
      ! what if prefix = "xmlns"? or other "xml"
    endif
  
    np => createNode(doc, ATTRIBUTE_NODE, qualifiedName, "")
    np%namespaceURI => vs_str_alloc(namespaceURI)
    np%localname => vs_str_alloc(localPartofQName(qualifiedname))
    np%prefix => vs_str_alloc(PrefixofQName(qualifiedname))

    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createAttributeNS

  TOHW_function(getElementsByTagNameNS, (doc, namespaceURI, localName), list)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: namespaceURI, localName
    type(NodeList), pointer :: list

    type(NodeListPtr), pointer :: nll(:), temp_nll(:)
    type(Node), pointer :: this, arg
    logical :: doneChildren, doneAttributes, allLocalNames, allNameSpaces
    integer :: i

    if (.not.associated(doc)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (doc%nodeType/=DOCUMENT_NODE.or.doc%nodeType/=ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    if (namespaceURI=="*") &
      allNameSpaces = .true.
    if (localName=="*") &
      allLocalNames = .true.

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

    this => arg

TOHW_m_dom_treewalk(`dnl
      if ((this%nodeType==ELEMENT_NODE) &
        .and. (allNameSpaces .or. str_vs(arg%namespaceURI)==namespaceURI) &
        .and. (allLocalNames .or. str_vs(arg%localName)==localName)) then
        call append(list, this)
          doneAttributes = .true.
        endif
',`')

  end function getElementsByTagNameNS


  TOHW_function(getElementById, (doc, elementId), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: elementId
    type(Node), pointer :: np

    type(Node), pointer :: this, arg
    type(NamedNodeMap), pointer :: nnm
    integer :: i
    logical :: doneChildren, doneAttributes

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    arg => getDocumentElement(doc)

    np => null()
TOHW_m_dom_treewalk(`dnl
      if (this%nodeType==ATTRIBUTE_NODE)  then
        if (getIsId(this).and.getName(this)==elementId) then
          np => this
          return
        endif
      endif
',`')

    np => null()

  end function getElementById

!  function getInputEncoding
!  function getXmlEncoding
!  function getXmlStandalone
!  function setXmlStandalone


  TOHW_function(getXmlVersion, (doc), s)
    type(Node), pointer :: doc
    character(len=3) :: s

    if (getXmlVersionEnum(doc)==XML1_0) then
      s = "1.0"
    elseif (getXmlVersionEnum(doc)==XML1_1) then
      s = "1.1"
    else
      s = "XXX"
    endif

  end function getXmlVersion

  TOHW_subroutine(setXmlVersion, (doc, s))
    type(Node), pointer :: doc
    character(len=*) :: s

    if (s=="1.0") then
      doc%docExtras%xds%xml_version = XML1_0
    elseif (s=="1.1") then
      doc%docExtras%xds%xml_version = XML1_1
    else
      TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
    endif

  end subroutine setXmlVersion


!  function getStrictErrorChecking
!  function setStrictErrorChecking
!  function getDocumentURI
!  function setDocumentURI

  ! Internal function, not part of API

  function createEntity(doc, name, publicId, systemId, notationName) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    character(len=*), intent(in) :: notationName
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      print*,"internal error in createEntity"
      stop
    endif

    np => createNode(doc, ENTITY_NODE, name, "")
    np%publicId => vs_str_alloc(publicId)
    np%systemId => vs_str_alloc(systemId)
    np%notationName => vs_str_alloc(notationName)

  end function createEntity

  function createNotation(doc, name, publicId, systemId) result(np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: publicId
    character(len=*), intent(in) :: systemId
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      print*,"internal error in createEntity"
      stop
    endif

    np => createNode(doc, NOTATION_NODE, name, "")
    np%publicId => vs_str_alloc(publicId)
    np%systemId => vs_str_alloc(systemId)
    
  end function createNotation


  TOHW_function(getXmlVersionEnum, (doc), n)
    type(Node), pointer :: doc
    integer :: n

    n = doc%docExtras%xds%xml_version

  end function getXmlVersionEnum

  TOHW_function(getXds, (doc), xds)
    type(Node), pointer :: doc
    type(xml_doc_state) :: xds

    xds = doc%docExtras%xds

  end function getXds


  TOHW_function(getGCstate, (doc), b)
    type(Node), pointer :: doc
    logical :: b

    b = doc%docExtras%xds%building

  end function getGCstate

  TOHW_subroutine(setGCstate, (doc, b))
    type(Node), pointer :: doc
    logical, intent(in) :: b

    doc%docExtras%xds%building = b

  end subroutine setGCstate

')`'dnl

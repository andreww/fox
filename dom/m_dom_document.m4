TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_charset, only: XML1_0, XML1_1
  use m_common_namecheck, only: checkQName, prefixOfQName, localPartOfQName
  use m_dom_error, only : NOT_FOUND_ERR, INVALID_CHARACTER_ERR, FoX_INVALID_NODE, &
    FoX_INVALID_XML_NAME, WRONG_DOCUMENT_ERR, FoX_INVALID_TEXT, & 
    FoX_INVALID_CHARACTER, FoX_INVALID_COMMENT, FoX_INVALID_CDATA_SECTION, &
    FoX_INVALID_PI_DATA, NOT_SUPPORTED_ERR, FoX_INVALID_ENTITY

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
  public :: getElementsByTagName
  public :: importNode
  public :: createElementNS
  public :: createAttributeNS
  public :: getElementsByTagNameNS
  public :: getElementById
  public :: getXmlVersion
  public :: setXmlVersion

  public :: createEntity
  public :: createNotation
  public :: setGCstate
')`'dnl
dnl
TOHW_m_dom_contents(`

  ! Getters and setters:

  TOHW_function(getDocType, (doc), np)
    type(Node), intent(in) :: doc
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    np => doc%docExtras%docType

  end function getDocType

  TOHW_function(getImplementation, (doc), imp)
    type(Node), intent(in) :: doc
    type(DOMImplementation), pointer :: imp

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    imp => doc%docExtras%implementation
    
  end function getImplementation

  TOHW_function(getDocumentElement, (doc), np)
    type(Node), intent(in) :: doc
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => doc%docExtras%documentElement

  end function getDocumentElement

  TOHW_subroutine(setDocumentElement, (doc, np))
  ! Only for use by FoX, not exported through FoX_DOM interface
    type(Node), pointer :: doc
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (np%nodeType/=ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.associated(np%ownerDocument, doc)) then
      TOHW_m_dom_throw_error(WRONG_DOCUMENT_ERR)
    endif
    
    doc%docExtras%documentElement => np

  end subroutine setDocumentElement

  ! Methods

  TOHW_function(createElement, (doc, tagName), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: tagName
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(tagName, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    endif  
    if (.not.checkName(tagName, getXds(doc))) then
      TOHW_m_dom_throw_error(FoX_INVALID_XML_NAME)
    endif
    
    np => createNode(doc, ELEMENT_NODE, tagName, "")
    np%attributes%ownerElement => np

    ! FIXME set namespaceURI and localName appropriately

    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createElement
    
  TOHW_function(createDocumentFragment, (doc), np)
    type(Node), pointer :: doc
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif
    
    np => createNode(doc, DOCUMENT_FRAGMENT_NODE, "#document-fragment", "")
    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createDocumentFragment

  TOHW_function(createTextNode, (doc, data), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(data, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif

    np => createNode(doc, TEXT_NODE, "#text", data)

    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif
   
  end function createTextNode

  TOHW_function(createComment, (doc, data), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(data, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    elseif (index(data,"--")>0) then   
      TOHW_m_dom_throw_error(FoX_INVALID_COMMENT)
    endif
  
    np => createNode(doc, COMMENT_NODE, "#comment", data)

    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createComment

  TOHW_function(createCdataSection, (doc, data), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: data
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(data, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    elseif (index(data,"]]>")>0) then   
      TOHW_m_dom_throw_error(FoX_INVALID_CDATA_SECTION)
    endif
  
    np => createNode(doc, CDATA_SECTION_NODE, "#text", data)

    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif
  
  end function createCdataSection

  TOHW_function(createProcessingInstruction, (doc, target, data), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: target
    character(len=*), intent(in) :: data
    type(Node), pointer :: np


    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(target, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkChars(data, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    elseif (.not.checkName(target, getXds(doc))) then
      TOHW_m_dom_throw_error(FoX_INVALID_XML_NAME)
! FIXME check validity of PI target 
    elseif (index(data,"?>")>0) then   
      TOHW_m_dom_throw_error(FoX_INVALID_PI_DATA)
    endif

    np => createNode(doc, PROCESSING_INSTRUCTION_NODE, target, data)

    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createProcessingInstruction

  TOHW_function(createAttribute, (doc, name), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(name, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkName(name, getXds(doc))) then
      TOHW_m_dom_throw_error(FoX_INVALID_XML_NAME)
    endif
  
    np => createNode(doc, ATTRIBUTE_NODE, name, "")
    np%namespaceURI => vs_str_alloc("")
    np%localname => vs_str_alloc(name)
    np%prefix => vs_str_alloc(name)

    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif
  
  end function createAttribute

  TOHW_function(createEntityReference, (doc, name), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: name
    type(Node), pointer :: np

    type(Node), pointer :: ent
    integer :: i

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (.not.checkChars(name, getXmlVersionEnum(doc))) then
      TOHW_m_dom_throw_error(INVALID_CHARACTER_ERR)
    elseif (.not.checkName(name, getXds(doc))) then
      TOHW_m_dom_throw_error(FoX_INVALID_XML_NAME)
    endif

    np => createNode(doc, ENTITY_REFERENCE_NODE, name, "")
    if (getGCstate(doc)) then ! otherwise the parser will fill these nodes in itself
      ! FIXME except I think that gets switched off when creating atts sometimes ... need to check
      ent => getNamedItem(getEntities(getDocType(doc)), name)
      if (ent%illFormed) then
        TOHW_m_dom_throw_error(FoX_INVALID_ENTITY)
      endif
      if (associated(ent)) then
        do i = 0, getLength(getChildNodes(ent)) - 1
          ent => appendChild(np, cloneNode(item(getChildNodes(ent), i), .true., ex))
          call setReadOnlyNode(ent, .true.)
        enddo
      endif
      ! FIXME in case of recursive entity references?
    endif
    if (getGCstate(doc)) then
      np%inDocument = .false.
      call append(doc%docExtras%hangingnodes, np)
    else
      np%inDocument = .true.
    endif

  end function createEntityReference

  TOHW_function(getElementsByTagName, (doc, tagName, name), list)
    type(Node), pointer :: doc
    character(len=*), intent(in), optional :: tagName, name
    type(NodeList), pointer :: list

    type(NodeListPtr), pointer :: nll(:), temp_nll(:)
    type(Node), pointer :: np
    logical :: ascending, allElements
    integer :: i

! FIXME check name and tagname for doc/element respectively ...

    if (doc%nodeType/=DOCUMENT_NODE.and.doc%nodeType/=ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    if (tagName=="*") &
      allElements = .true.

    if (doc%nodeType==DOCUMENT_NODE) then
      np => getDocumentElement(doc)
    else
      np => doc
    endif

    if (.not.associated(np)) then
      ! FIXME internal error
      continue
    endif

    allocate(list)
    allocate(list%nodes(0))
    list%element => doc
    if (present(name)) list%nodeName => vs_str_alloc(name) ! FIXME or tagName
    if (present(tagName)) list%nodeName => vs_str_alloc(tagName) ! FIXME or tagName

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

    ascending = .false.
    do
      if (ascending) then
        np => np%parentNode
        if (associated(np, doc).or.associated(np, getDocumentElement(doc))) exit
        ascending = .false.
      elseif (associated(np%firstChild)) then
        np => np%firstChild
        cycle
      endif
      if ((np%nodeType==ELEMENT_NODE) .and. &
        (allElements .or. str_vs(np%nodeName)==tagName)) then
        call append(list, np)
      endif
      if (associated(np%nextSibling)) then
        np => np%nextSibling
      else
        ascending = .true.
      endif
    enddo

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
', `', `parentNode')

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
    type(Node), pointer :: np
    logical :: noChild, allLocalNames, allNameSpaces
    integer :: i

    if (doc%nodeType/=DOCUMENT_NODE.and.doc%nodeType/=ELEMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    if (namespaceURI=="*") &
      allNameSpaces = .true.
    if (localName=="*") &
      allLocalNames = .true.

    if (doc%nodeType==DOCUMENT_NODE) then
      np => getDocumentElement(doc)
    else
      np => doc
    endif

    if (.not.associated(np)) then
      ! FIXME internal error
      continue
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

    noChild = .false.
    do
      if (noChild) then
        if (associated(np, doc).or.associated(np, getDocumentElement(doc))) then
          exit
        else
          np => np%parentNode
          noChild=  .false.
        endif
      endif
      if ((np%nodeType==ELEMENT_NODE) &
        .and. (allNameSpaces .or. str_vs(np%namespaceURI)==namespaceURI) &
        .and. (allLocalNames .or. str_vs(np%localName)==localName)) then
        call append(list, np)
      endif
      if (associated(np%firstChild)) then
        np => np%firstChild
      elseif (associated(np%nextSibling)) then
        np => np%nextSibling
      else
        noChild = .true.
      endif
    enddo

  end function getElementsByTagNameNS


  TOHW_function(getElementById, (doc, elementId), np)
    type(Node), pointer :: doc
    character(len=*), intent(in) :: elementId
    type(Node), pointer :: np

    type(Node), pointer :: attr
    type(NamedNodeMap), pointer :: nnm
    integer :: i
    logical :: noChild

    if (doc%nodeType/=DOCUMENT_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => getDocumentElement(doc)

    noChild = .false.
    do
      if (noChild) then
        if (associated(np, doc).or.associated(np, getDocumentElement(doc))) then
          exit
        else
          np => np%parentNode
          noChild=  .false.
        endif
      endif
      if (np%nodeType==ELEMENT_NODE) then
        nnm => np%attributes
        do i = 1, getLength(nnm)
          attr => item(nnm, i)
          if (attr%isId.and.getValue(attr)==elementId) &
            return
        enddo
      endif
      if (associated(np%firstChild)) then
        np => np%firstChild
      elseif (associated(np%nextSibling)) then
        np => np%nextSibling
      else
        noChild = .true.
      endif
    enddo

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

 



module m_dom_utils

  use m_common_attrs, only: getValue
  use m_common_format, only: operator(//)
  use m_common_array_str, only: str_vs, vs_str

  use m_dom_dom, only: Node, Namednodemap, Node
  use m_dom_dom, only: DOCUMENT_NODE, ELEMENT_NODE, TEXT_NODE, &
   CDATA_SECTION_NODE, COMMENT_NODE, DOCUMENT_TYPE_NODE, &
   ATTRIBUTE_NODE, ENTITY_REFERENCE_NODE, PROCESSING_INSTRUCTION_NODE
  use m_dom_dom, only: haschildnodes, getNodeName, getNodeType, &
    getFirstChild, getNextSibling, getlength, item, getOwnerElement, &
    getAttributes, getParentNode, getChildNodes, getPrefix, getLocalName, &
    getNodeName, getData, getName, getTagName, getValue, getTarget, &
    getEntities, getNotations, item, getSystemId, getPublicId, getNotationName, getStringValue
  use m_dom_error, only: DOMException, inException, throw_exception, &
    FoX_INVALID_NODE, SERIALIZE_ERR, FoX_INTERNAL_ERROR

  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_OpenFile, xml_Close
  use FoX_wxml, only: xml_AddXMLDeclaration
  use FoX_wxml, only: xml_DeclareNamespace, xml_AddDOCTYPE, xml_AddInternalEntity
  use FoX_wxml, only: xml_AddAttribute, xml_AddExternalEntity, xml_AddNotation
  use FoX_wxml, only: xml_AddCharacters
  use FoX_wxml, only: xml_NewElement
  use FoX_wxml, only: xml_EndElement
  use FoX_wxml, only: xml_AddComment
  use FoX_wxml, only: xml_AddEntityReference
  use FoX_wxml, only: xml_AddXMLPI

  implicit none

  public :: dumpTree
  public :: serialize

  private

contains

  subroutine dumpTree(startNode)

    type(Node), pointer :: startNode   

    integer           :: indent_level

    indent_level = 0

    call dump2(startNode)

  contains

    recursive subroutine dump2(input)
      type(Node), pointer :: input
      type(Node), pointer :: temp
      integer :: i
      temp => input
      do while(associated(temp))
         write(*,"(3a,i0)") repeat(" ", indent_level), &
                        getNodeName(temp), " of type ", &
                        getNodeType(temp)
         if (getNodeType(temp)==ELEMENT_NODE) then
           do i = 1, getLength(getAttributes(temp))
             write(*, "(3a)") repeat(" ", indent_level), "  ", &
               getName(item(getAttributes(temp), i-1))
           enddo
!           do i = 1, getLength(
         endif
         if (hasChildNodes(temp)) then
            indent_level = indent_level + 3
            call dump2(getFirstChild(temp))
            indent_level = indent_level - 3
         endif
         temp => getNextSibling(temp)
      enddo

    end subroutine dump2

  end subroutine dumpTree


  subroutine serialize(startNode, name, ex)
    type(DOMException), intent(out), optional :: ex

    type(Node), pointer :: startNode   
    character(len=*), intent(in) :: name

    type(xmlf_t)  :: xf
    integer :: iostat

    if (getNodeType(startNode)/=DOCUMENT_NODE &
      .and.getNodeType(startNode)/=ELEMENT_NODE) then
      call throw_exception(FoX_INVALID_NODE, "serialize", ex)
if (present(ex)) then
  if (inException(ex)) then
     return
  endif
endif

    endif
    
    !FIXME several of the below should be optional to serialize
    call xml_OpenFile(name, xf, iostat=iostat, unit=-1, preserve_whitespace=.true., warning=.false.)
    if (iostat/=0) then
      call throw_exception(SERIALIZE_ERR, "serialize", ex)
if (present(ex)) then
  if (inException(ex)) then
     return
  endif
endif

    endif
    call iter_dmp_xml(xf, startNode, ex)
    call xml_Close(xf)

  end subroutine serialize

  subroutine iter_dmp_xml(xf, arg, ex)
    type(DOMException), intent(out), optional :: ex
    type(xmlf_t), intent(inout) :: xf

    type(Node), pointer :: this, arg, treeroot, attrchild
    type(NamedNodeMap), pointer :: nnm
    integer :: i_tree, j
    logical :: doneChildren, doneAttributes
    logical :: cdata, entities
    character, pointer :: attrvalue(:), tmp(:)

!FIXME options for entityrefs & cdata ...
    cdata = .false.
    entities = .true.
    treeroot => arg

    i_tree = 0
    doneChildren = .false.
    doneAttributes = .false.
    this => treeroot
    do

      if (.not.(getNodeType(this)==ELEMENT_NODE.and.doneAttributes)) then
      if (.not.doneChildren) then

    select case(getNodeType(this))
    case (ELEMENT_NODE)
      nnm => getAttributes(this)
      do j = 0, getLength(nnm) - 1
        attrchild => item(nnm, j)
        if (getLocalName(attrchild)=="xmlns") then
          call xml_DeclareNamespace(xf, getValue(attrchild))
        elseif (getPrefix(attrchild)=="xmlns") then
          call xml_DeclareNamespace(xf, getValue(attrchild), &
            getLocalName(attrchild))
        endif
      enddo
      call xml_NewElement(xf, getTagName(this))
    case (ATTRIBUTE_NODE)
      if (getPrefix(this)/="xmlns".and.getLocalName(this)/="xmlns") then
        if (entities) then
          allocate(attrvalue(0))
          do j = 0, getLength(getChildNodes(this)) - 1
            attrchild => item(getChildNodes(this), j)
            if (getNodeType(attrchild)==TEXT_NODE) then
              tmp => attrvalue
              allocate(attrvalue(size(tmp)+getLength(attrchild)))
              attrvalue(:size(tmp)) = tmp
              attrvalue(size(tmp)+1:) = vs_str(getData(attrChild))
              deallocate(tmp)
            elseif (getNodeType(attrchild)==ENTITY_REFERENCE_NODE) then
              tmp => attrvalue
              allocate(attrvalue(size(tmp)+len(getNodeName(attrchild))+2))
              attrvalue(:size(tmp)) = tmp
              attrvalue(size(tmp)+1:) = vs_str("&"//getData(attrChild)//";")
              deallocate(tmp)
            else
              call throw_exception(FoX_INTERNAL_ERROR, "iter_dmp_xml", ex)
if (present(ex)) then
  if (inException(ex)) then
     return
  endif
endif

            endif
          enddo
          call xml_AddAttribute(xf, getName(this), str_vs(attrvalue))
          deallocate(attrvalue)
        else
          call xml_AddAttribute(xf, getName(this), getValue(this))
        endif
      endif
      doneChildren = .true.
    case (TEXT_NODE)
      call xml_AddCharacters(xf, getData(this))
    case (CDATA_SECTION_NODE)
      if (cdata) then
        call xml_AddCharacters(xf, getData(this), parsed = .false.)
      else
        call xml_AddCharacters(xf, getData(this))
      endif
    case (ENTITY_REFERENCE_NODE)
      if (entities) then
        call xml_AddEntityReference(xf, getNodeName(this))
        doneChildren = .true.
      endif
    case (PROCESSING_INSTRUCTION_NODE)
      call xml_AddXMLPI(xf, getTarget(this), getData(this))
    case (COMMENT_NODE)
      call xml_AddComment(xf, getData(this))
    case (DOCUMENT_TYPE_NODE)
      call xml_AddDOCTYPE(xf, getName(this))
      nnm => getNotations(this)
      do j = 0, getLength(nnm)-1
        if (getSystemId(item(nnm, j))=="") then
          call xml_AddNotation(xf, getNodeName(item(nnm, j)), public=getPublicId(item(nnm, j)))
        elseif (getPublicId(item(nnm, j))=="") then
          call xml_AddNotation(xf, getNodeName(item(nnm, j)), system=getSystemId(item(nnm, j)))
        else
          call xml_AddNotation(xf, getNodeName(item(nnm, j)), system=getSystemId(item(nnm, j)), &
            public=getPublicId(item(nnm, j)))
        endif
      enddo
      nnm => getEntities(this)
      do j = 0, getLength(nnm)-1
        if (getSystemId(item(nnm, j))=="") then
          call xml_AddInternalEntity(xf, getNodeName(item(nnm, j)), getStringValue(item(nnm, j)))
        elseif (getPublicId(item(nnm, j))=="".and.getNotationName(item(nnm, j))=="") then
          call xml_AddExternalEntity(xf, getNodeName(item(nnm, j)), system=getSystemId(item(nnm, j)))
        elseif (getNotationName(item(nnm, j))=="") then
          call xml_AddExternalEntity(xf, getNodeName(item(nnm, j)), system=getSystemId(item(nnm, j)), &
            public=getPublicId(item(nnm, j)))
        elseif (getPublicId(item(nnm, j))=="") then
          call xml_AddExternalEntity(xf, getNodeName(item(nnm, j)), system=getSystemId(item(nnm, j)), &
            notation=getNotationName(item(nnm, j)))
        else
          call xml_AddExternalEntity(xf, getNodeName(item(nnm, j)), system=getSystemId(item(nnm, j)), &
            public=getPublicId(item(nnm, j)), notation=getNotationName(item(nnm, j)))
        endif
      enddo
    end select


      else
        if (getNodeType(this)==ELEMENT_NODE) doneAttributes = .true.


    if (getNodeType(this)==ELEMENT_NODE) then
      call xml_EndElement(xf, getTagName(this))
    endif


      endif
      endif

      if (.not.doneChildren) then

        if (getNodeType(this)==ELEMENT_NODE.and..not.doneAttributes) then
          if (getLength(getAttributes(this))>0) then
                      this => item(getAttributes(this), 0)
          else
            doneAttributes = .true.
          endif
        elseif (hasChildNodes(this)) then
          this => getFirstChild(this)
          doneChildren = .false.
          doneAttributes = .false.
        else
          doneChildren = .true.
          doneAttributes = .false.
        endif

      else ! if doneChildren

        if (associated(this, arg)) exit
        if (getNodeType(this)==ATTRIBUTE_NODE) then
          if (i_tree<getLength(getAttributes(getOwnerElement(this)))-1) then
            i_tree= i_tree+ 1
            this => item(getAttributes(getOwnerElement(this)), i_tree)
            doneChildren = .false.
          else
            i_tree= 0
            this => getOwnerElement(this)
            doneAttributes = .true.
            doneChildren = .false.
          endif
        elseif (associated(getNextSibling(this))) then

          this => getNextSibling(this)
          doneChildren = .false.
          doneAttributes = .false.
        else
          this => getParentNode(this)
        endif
      endif

    enddo



  end subroutine iter_dmp_xml

end module m_dom_utils

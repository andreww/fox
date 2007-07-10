undefine(`index', `len', `format')dnl
include(`m_dom_treewalk.m4')dnl
module m_dom_utils

  use m_common_array_str, only: str_vs, vs_str_alloc, vs_vs_alloc
  use m_common_attrs, only: dictionary_t, add_item_to_dict, getValue, &
    hasKey, init_dict, destroy_dict
  use m_common_format, only: operator(//)

  use m_dom_dom, only: Node, Namednodemap, Node
  use m_dom_dom, only: DOCUMENT_NODE, ELEMENT_NODE, TEXT_NODE, &
   CDATA_SECTION_NODE, COMMENT_NODE, DOCUMENT_TYPE_NODE, DOCUMENT_FRAGMENT_NODE, &
   ATTRIBUTE_NODE, ENTITY_NODE, NOTATION_NODE, ENTITY_REFERENCE_NODE, PROCESSING_INSTRUCTION_NODE
  use m_dom_dom, only: haschildnodes, getNodeName, getNodeType, &
    getFirstChild, getNextSibling, getlength, item, getDocumentElement, getOwnerElement, &
    getNameSpaceURI, getPrefix, getLocalName, getAttributes, getParentNode, &
    getNodeName, getNodeValue, getData, getName, getTagName, getValue, getTarget, &
    getEntities, getNotations, item, getSystemId, getPublicId, getNotationName, getStringValue


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
      temp => input
      do while(associated(temp))
         write(*,"(3a,i0)") repeat(" ", indent_level), &
                        getNodeName(temp), " of type ", &
                        getNodeType(temp)
         if (hasChildNodes(temp)) then
            indent_level = indent_level + 3
            call dump2(getFirstChild(temp))
            indent_level = indent_level - 3
         endif
         temp => getNextSibling(temp)
      enddo

    end subroutine dump2

  end subroutine dumpTree


  subroutine serialize(startNode, name)

    type(Node), pointer :: startNode   
    character(len=*), intent(in) :: name

    type(xmlf_t)  :: xf
    integer :: iostat
    
    !FIXME several of the below should be optional to serialize
    call xml_OpenFile(name, xf, iostat=iostat, unit=-1, preserve_whitespace=.true.)
    if (iostat/=0) then
      print*,"IOSTAT", iostat
      stop
      continue
    endif
    call iter_dmp_xml(xf, startNode)
    call xml_Close(xf)

  end subroutine serialize

  subroutine iter_dmp_xml(xf, arg)
    type(xmlf_t), intent(inout) :: xf
    type(Node), pointer :: doc

    type(Node), pointer :: this, arg, treeroot
    type(NamedNodeMap), pointer :: nnm
    integer :: i_tree, j
    logical :: doneChildren, doneAttributes

!FIXME options for entityrefs & cdata ...
    treeroot => arg
TOHW_m_dom_treewalk(`dnl
    select case(getNodeType(this))
    case (ELEMENT_NODE)
      call xml_NewElement(xf, getTagName(this))
    case (ATTRIBUTE_NODE)
      call xml_AddAttribute(xf, getName(this), getValue(this))
      doneChildren = .true.
    case (TEXT_NODE)
      call xml_AddCharacters(xf, getData(this))
    case (CDATA_SECTION_NODE)
      call xml_AddCharacters(xf, getData(this), parsed = .false.)
    case (ENTITY_REFERENCE_NODE)
      call xml_AddEntityReference(xf, getNodeName(this))
      doneChildren = .true.
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
',`
    if (getNodeType(this)==ELEMENT_NODE) then
      call xml_EndElement(xf, getTagName(this))
    endif
')

  end subroutine iter_dmp_xml

end module m_dom_utils

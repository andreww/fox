module m_dom_utils

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_format, only: operator(//)
  use m_dom_types, only: fnode, fnamednodemap, fDocumentNode
  use m_dom_types, only: element_node
  use m_dom_types, only: text_node, cdata_section_node
  use m_dom_types, only: comment_node

  use m_dom_node, only: getnodetype, getnodename
  use m_dom_node, only: getnodevalue, getfirstchild
  use m_dom_node, only: getnextsibling
  use m_dom_node, only: haschildnodes, getattributes
  use m_dom_namednodemap, only: getlength, item

  use m_common_attrs, only: dictionary_t, add_item_to_dict, getValue, hasKey
!  use m_dict, only : dictionary, addKey, getValue, hasKey

  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_OpenFile, xml_Close
  use FoX_wxml, only: xml_AddXMLDeclaration
  use FoX_wxml, only: xml_DeclareNamespace
  use FoX_wxml, only: xml_AddAttribute
  use FoX_wxml, only: xml_AddCharacters
  use FoX_wxml, only: xml_NewElement
  use FoX_wxml, only: xml_EndElement
  use FoX_wxml, only: xml_AddComment

  implicit none

  public :: dumpTree
  public :: serialize

  interface serialize
    module procedure serialize_node
    module procedure serialize_doc_node
  end interface

  private

CONTAINS

  subroutine dumpTree(startNode)

    type(fnode), pointer :: startNode   

    character(len=50) :: indent = " "
    integer           :: indent_level

    indent_level = 0

    call dump2(startNode)

  contains

    recursive subroutine dump2(input)
      type(fnode), pointer :: input
      type(fnode), pointer :: temp     
      temp => input
      do while(associated(temp))
         write(*,'(3a,i3)') indent(1:indent_level), &
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
!----------------------------------------------------------------

  subroutine serialize_node(startNode,fname)

    type(fnode), pointer :: startNode   
    character(len=*), intent(in) :: fname

    type(xmlf_t)  :: xf

    call xml_OpenFile(fname,xf)
    call dump_xml(xf, startNode)
    call xml_Close(xf)

  end subroutine serialize_node

  subroutine serialize_doc_node(startNode, fname)
    type(fDocumentNode), pointer :: startNode   
    character(len=*), intent(in) :: fname

    type(xmlf_t)  :: xf

    call xml_OpenFile(fname,xf)
    call xml_AddXMLDeclaration(xf)
    call dump_xml(xf, startNode%documentElement)
    call xml_Close(xf)
  end subroutine serialize_doc_node

  recursive subroutine dump_xml(xf, input)
    type(xmlf_t)  :: xf
    type(fnode), pointer         :: input
    type(fnode), pointer         :: node, attr, child
    type(fnamedNodeMap), pointer :: attr_map
    integer  ::  i
    integer, save :: nns = -1
    type(dictionary_t), save :: simpleDict
    character, pointer :: prefix(:), elementQName(:)

    node => input
    if (.not. associated(node)) return
    select case (getNodeType(node))
      
    case (ELEMENT_NODE)
      
      if (size(node%namespaceURI)==0) then
        elementQName => vs_str_alloc(getNodeName(node))
      else
        if (hasKey(simpleDict, str_vs(node%namespaceURI))) then
          prefix = getValue(simpleDict, str_vs(node%namespaceURI))
        else
          nns = nns + 1
          if (nns == 0) then
            allocate(prefix(0))
          else
            prefix => vs_str_alloc('ns'//nns)
          endif
          call addItem(simpleDict, str_vs(node%namespaceURI), str_vs(node%prefix))
        endif
        if (size(prefix)==0) then
          elementQName => vs_str_alloc(getNodeName(node))
        else
          elementQName => vs_str_alloc(str_vs(node%prefix)//":"//getNodeName(node))
        endif
      endif
      call xml_NewElement(xf, str_vs(elementQName))
      if (size(node%namespaceURI)==0) then
        if (size(prefix)==0) then
          call xml_DeclareNamespace(xf, str_vs(node%namespaceURI))
        else
          call xml_DeclareNamespace(xf, str_vs(node%namespaceURI), str_vs(node%prefix))
        endif
      endif
      deallocate(prefix)
      !TOHW need to check for & print out attribute namespaces as well.
      attr_map => getAttributes(node)
      do i = 0, getLength(attr_map) - 1
        attr => item(attr_map,i)
        call xml_AddAttribute(xf, getNodeName(attr), getNodeValue(attr))
      enddo
      child => node % firstChild
      do while (associated(child))
        call dump_xml(xf, child)
        child => child % nextSibling
      enddo
      call xml_EndElement(xf,str_vs(elementQName))
      deallocate(elementQName)
      
    case (TEXT_NODE)
      
      call xml_AddPcdata(xf, getNodeValue(node))
      
    case (CDATA_SECTION_NODE)
      
      call xml_AddCdataSection(xf, getNodeValue(node))
      
    case (COMMENT_NODE)
      
      call xml_AddComment(xf, getNodeValue(node))
      
    end select

  end subroutine dump_xml
  
end module m_dom_utils
